#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <erl_nif.h>
#include <alsa/asoundlib.h>

#define INT24_MAX  8388607
#define INT24_MIN  -8388607
#define INT24_MAX_F  8388607.0f
#define INT24_MIN_F  -8388607.0f

#define FRAME_TYPE _Float32
#define FRAME_SIZE sizeof(FRAME_TYPE)

static const snd_pcm_format_t formats[] = {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  SND_PCM_FORMAT_FLOAT_LE,
  SND_PCM_FORMAT_S32_LE,
  SND_PCM_FORMAT_S24_LE,
  SND_PCM_FORMAT_S16_LE
#else
  SND_PCM_FORMAT_FLOAT_BE,
  SND_PCM_FORMAT_S32_BE,
  SND_PCM_FORMAT_S24_BE,
  SND_PCM_FORMAT_S16_BE
#endif
};

#define NUMFORMATS (sizeof(formats) / sizeof(formats[0]))

typedef struct
{
  snd_pcm_t * handle;
  unsigned int format_index;
  snd_pcm_access_t access;
  unsigned int period_size;
  unsigned int channels;
  FRAME_TYPE** channel_bufs;
  int fd;
} xalsa_t;

static ErlNifResourceType* res_type;

static int set_hwparams(xalsa_t * unit,
                        unsigned int *rate, unsigned int *channels, unsigned int n,
                        snd_pcm_uframes_t *buffer_size,
                        snd_pcm_uframes_t *period_size){
  int err;
  /*
     rate     period_size
     44100    256
     48000    256
     96000    512
     192000   1024
  */
  snd_pcm_hw_params_t *params;
  snd_pcm_t *handle = unit->handle;
  snd_pcm_hw_params_malloc(&params);
  err = snd_pcm_hw_params_any(handle, params);
  if (err < 0) {
    printf("Broken configuration for playback: no configurations available: %s\n",
	   snd_strerror(err));
    return err;
  }

  /* set hardware resampling */
  err = snd_pcm_hw_params_set_rate_resample(handle, params, 1);
  if (err < 0) {
    printf("Resampling setup failed for playback: %s\n", snd_strerror(err));
    return err;
  }
  /* set the interleaved read/write format */
  unit->access = SND_PCM_ACCESS_MMAP_NONINTERLEAVED;
  err = snd_pcm_hw_params_set_access(handle, params, unit->access);
  if (err < 0) {
    unit->access = SND_PCM_ACCESS_MMAP_INTERLEAVED;
    err = snd_pcm_hw_params_set_access(handle, params, unit->access);
    if (err < 0) {
      printf("Access type not available for playback: %s\n", snd_strerror(err));
      return err;
    }
    printf("Interleaved access for playback\n");
  }
  /* set the sample format */
  unsigned int format_index = 0;
  while((err = snd_pcm_hw_params_set_format(handle, params, formats[format_index]) < 0) &&
        format_index < NUMFORMATS) {
    format_index++;
  }
  if (err < 0) {
    printf("Sample format not available for playback: %s\n", snd_strerror(err));
    return err;
  }
  printf("Sample format selected for playback: %i\n", format_index);
  unit->format_index = format_index;

  /* set the count of channels */
  err = snd_pcm_hw_params_set_channels_near(handle, params, channels);
  if (err < 0) {
    printf("Channels count (%i) not available for playbacks: %s\n", *channels, snd_strerror(err));
    return err;
  }
  unit->channels = *channels;

  /* set the stream rate */
  err = snd_pcm_hw_params_set_rate_near(handle, params, rate, 0);
  if (err < 0) {
    printf("Rate %iHz not available for playback: %s\n", *rate, snd_strerror(err));
    return err;
  }

  err = snd_pcm_hw_params_set_period_size_near(handle, params, period_size, 0);
  if (err < 0) {
    printf("Unable to set period size for playback: %s\n", snd_strerror(err));
    return err;
  }
  unit->period_size = *period_size;

  *buffer_size = n * (*period_size);
  err = snd_pcm_hw_params_set_buffer_size_near(handle, params, buffer_size);
  if (err < 0) {
    printf("Unable to set buffer size for playback: %s\r\n", snd_strerror(err));
    return err;
  }

  /* write the parameters to device */
  err = snd_pcm_hw_params(handle, params);
  snd_pcm_hw_params_free(params);
  if (err < 0) {
    printf("Unable to set hw params for playback: %s\n", snd_strerror(err));
    return err;
  }
  /* Alloc channel buffers */
  unit->channel_bufs = enif_alloc(sizeof(FRAME_TYPE*) * unit->channels);
  if(unit->channel_bufs == NULL) return -1;
  for(unsigned int channel = 0; channel < unit->channels; channel++){
    unit->channel_bufs[channel] = enif_alloc(FRAME_SIZE * unit->period_size);
    if(unit->channel_bufs[channel] == NULL) return -1;
  }
  return format_index;
}

static ERL_NIF_TERM pcm_open_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char device[128];                     /* playback device */
  int err = 0;
  xalsa_t * unit = enif_alloc_resource(res_type, sizeof(xalsa_t));
  snd_pcm_t** handle_p = &unit->handle;

  // we read the erlang string (erlang list)
  if (!enif_get_string(env, argv[0], device, 128, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env); // blame user if address isn't a string
  }

  if ((err = snd_pcm_open(handle_p, device, SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK)) < 0) {
    printf("Playback open error: %s\n", snd_strerror(err));
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM term = enif_make_resource(env, unit);
  enif_release_resource(unit);
  return term;
}

static ERL_NIF_TERM pcm_close_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int ret;

  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }

  ret = snd_pcm_close(unit->handle);
  return enif_make_int(env, ret);
}

static ERL_NIF_TERM pcm_dump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int ret = 0;
  snd_output_t *output = NULL;
  int err;

  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }

  err = snd_output_stdio_attach(&output, stdout, 0);
  if (err < 0) {
    printf("Output failed: %s\n", snd_strerror(err));
    return -1;
  }

  ret = snd_pcm_dump(unit->handle, output);

  return enif_make_int(env, ret);
}

static ERL_NIF_TERM pcm_set_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int channels, rate, buffer_period_size_ratio;
  int ret;
  snd_pcm_uframes_t buffer_size, period_size; // ulong

  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[1], &channels)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[2], &rate)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_ulong(env, argv[3], &period_size)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[4], &buffer_period_size_ratio)) {
    return enif_make_badarg(env);
  }

  buffer_size = buffer_period_size_ratio * period_size;

  ret = set_hwparams(unit, &rate, &channels,
                     buffer_period_size_ratio,
                     &buffer_size,
                     &period_size);
  if (ret < 0) {
    return enif_make_uint(env, ret);
  }else{
    unit->format_index = ret;
  }

  snd_pcm_sw_params_t *sw_params;
  snd_pcm_sw_params_malloc(&sw_params);
  snd_pcm_sw_params_current(unit->handle, sw_params);
  snd_pcm_sw_params_set_start_threshold(unit->handle, sw_params,
                                        (buffer_size / period_size) * period_size - 1);
  // snd_pcm_sw_params_set_start_threshold(unit->handle, sw_params, 0U);
  snd_pcm_sw_params_set_avail_min(unit->handle, sw_params, period_size);
  snd_pcm_sw_params(unit->handle, sw_params);
  snd_pcm_sw_params_free(sw_params);

  return enif_make_tuple4(env,
                          enif_make_uint(env, rate),
                          enif_make_uint(env, channels),
                          enif_make_ulong(env, buffer_size),
                          enif_make_ulong(env, period_size));
}

#define MAX(x,y) (((x)>(y)) ? (x) : (y))
#define MIN(x,y) (((x)<(y)) ? (x) : (y))

/* This function covers both MMAP_INTERLEAVED and MMAP_NONINTERLEAVED cases */
static ERL_NIF_TERM pcm_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int format_index, size; // No of frames (per channel)
  unsigned int i, channel;
  int err = 0;
  const snd_pcm_channel_area_t *areas;
  snd_pcm_uframes_t offset, frames;
  snd_pcm_sframes_t avail;

  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }

  size = unit->period_size;
  format_index = unit->format_index;

  avail = snd_pcm_avail_update(unit->handle);
  if (avail < size) {
    return enif_make_int(env, avail);
  }

  frames = size;
  err = snd_pcm_mmap_begin(unit->handle, &areas, &offset, &frames);
  if(err < 0){
    // printf("mmap begin error for playback:: %s\n", snd_strerror(err));
    return enif_make_int(env, err);
  }
  if(frames != size) {
    return enif_make_int(env, frames);
  }
  FRAME_TYPE max;
  unsigned int step;
  unsigned char * mmapp;
  FRAME_TYPE * f;
  int32_t tmp32;
  for(channel = 0; channel < unit->channels; channel++){
    f = unit->channel_bufs[channel];
    step = areas[channel].step / 8;
    mmapp = (((unsigned char *)areas[channel].addr) + (areas[channel].first / 8));
    mmapp += offset * step;
    switch(format_index) {
    case 0:
      for(i = 0; i < frames; i++){
        *((FRAME_TYPE *) mmapp) = MAX(MIN(f[i], 1.0), -1.0);
        mmapp += step;
      }
      break;
    case 1:
    case 2:
      max = INT24_MAX_F;
      for(i = 0; i < frames; i++){
        if(f[i] <= -1.0f)
          tmp32 = INT24_MIN;
        else if(f[i] >= 1.0f)
          tmp32 = INT24_MAX;
        else
          tmp32 = (int32_t) (f[i] * max);

        if(format_index == 1)
          tmp32 = tmp32 << 8;

        *((int32_t *) mmapp) = tmp32;
        mmapp += step;
      }
      break;
    case 3:
      max = (FRAME_TYPE) (INT16_MAX - 1);
      for(i = 0; i < frames; i++){
        *((int16_t *) mmapp) = (int16_t) (MAX(MIN(f[i], 1.0), -1.0) * max);
        mmapp += step;
      }
      break;
    }
  }
  err = snd_pcm_mmap_commit(unit->handle, offset, frames);
  if(err < 0){
    printf("mmap commit error for playback:: %s\n", snd_strerror(err));
  }
  return enif_make_int(env, err);
}

static ERL_NIF_TERM pcm_prepare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  return enif_make_int(env, snd_pcm_prepare(unit->handle));
}

static ERL_NIF_TERM pcm_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err = 0;
  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  if (snd_pcm_state(unit->handle) == SND_PCM_STATE_PREPARED) {
    err = snd_pcm_start(unit->handle);
  }

  return enif_make_int(env, err);
}

static ERL_NIF_TERM pcm_recover(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int errno;
  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &errno)) {
    return enif_make_badarg(env);
  }
  return enif_make_int(env, snd_pcm_recover(unit->handle, errno, 1));
}


static ERL_NIF_TERM pcm_avail_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  return enif_make_int(env, snd_pcm_avail_update(unit->handle));
}

/*   Callback handling - asynchronous notification */

static void pcm_async_callback(snd_async_handler_t *ahandler)
{
  ErlNifPid* pid = snd_async_handler_get_callback_private(ahandler);
  ErlNifEnv* msg_env = enif_alloc_env();
  if(!enif_send(NULL, pid, msg_env, enif_make_atom(msg_env, "pcm_ready4write"))){
    printf("xalsa_nif pcm_async_callback: enif_send failed\n");
  }
  enif_free_env(msg_env);
}

static ERL_NIF_TERM pcm_add_async_handler(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  snd_async_handler_t *ahandler;
  int err;
  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));
  if (!enif_get_local_pid(env, argv[1], pid)) {
    return enif_make_badarg(env);
  }
  err = snd_async_add_pcm_handler(&ahandler, unit->handle, pcm_async_callback, pid);
  return enif_make_int(env, err);
}

static ERL_NIF_TERM pcm_add_fd(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct pollfd *ufds;
  int count, err;
  xalsa_t * unit;

  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  count = snd_pcm_poll_descriptors_count(unit->handle);
  if (count != 1) {
    printf("Invalid poll descriptors count %i\n", count);
    return enif_make_int(env, count);
  }

  ufds = enif_alloc(sizeof(struct pollfd) * (count + 2));
  if ((err = snd_pcm_poll_descriptors(unit->handle, ufds, count)) < 0) {
    printf("Unable to obtain poll descriptors for playback: %s\n", snd_strerror(err));
    return enif_make_int(env, err);
  }
  unit->fd = ufds[0].fd;
  enif_free(ufds);
  return enif_make_int(env, unit->fd);
}

static ERL_NIF_TERM pcm_enable_ready4write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int rv;
  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }
  rv = enif_select_write(env,
                         (ErlNifEvent) unit->fd,
                         unit, // void* obj,
                         NULL,
                         enif_make_atom(env, "pcm_ready4write"),
                         NULL);
  if( rv < 0 ) {
    return enif_make_badarg(env);
  }
  return enif_make_int(env, rv);
}

static ERL_NIF_TERM sum_map(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  unsigned int i, size; // No of frames
  ERL_NIF_TERM key, value, map_out;
  const ERL_NIF_TERM * tuple_array, * channel_array;
  ERL_NIF_TERM out_array[16]; // Max 16 channels per card
  unsigned int channel_arity;
  ErlNifMapIterator iter;
  ErlNifBinary bin;

  xalsa_t * unit;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &unit)){
    return enif_make_badarg(env);
  }

  if (
      !enif_get_tuple(env, argv[1], (int *) &channel_arity, &channel_array) ||
      channel_arity != unit->channels) {
    return enif_make_badarg(env);
  }

  size = unit->period_size;
  unsigned int bytesize = size * FRAME_SIZE;

  for (unsigned int channel = 0; channel < unit->channels; channel++) {
    if (!enif_map_iterator_create(env, channel_array[channel],
                                  &iter, ERL_NIF_MAP_ITERATOR_FIRST))
      {
        return enif_make_badarg(env);
      }

    FRAME_TYPE *buf = (FRAME_TYPE *) unit->channel_bufs[channel];

    for(i = 0; i < size; i++)
      buf[i] = 0.0;

    map_out = enif_make_new_map(env);

    FRAME_TYPE* framesp;
    unsigned int noframes;
    int arity, notify_flag;
    ERL_NIF_TERM new_binary;
    while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
      if(!(enif_get_tuple(env, value, &arity, &tuple_array) &&
           enif_get_int(env, tuple_array[0], &notify_flag) &&
           enif_inspect_binary(env, tuple_array[1], &bin))) {
        return enif_make_badarg(env);
      }
      framesp = (FRAME_TYPE *) bin.data;
      noframes = (bin.size / FRAME_SIZE);
      for(i = 0; (i < size) && (i < noframes); i++){
        buf[i] += framesp[i];
      }

      if(i < noframes) { // bin.data larger than size
        unsigned int newsize = bin.size - bytesize;
        if(notify_flag && newsize <= bytesize){
          ErlNifPid pid;
          if(enif_get_local_pid(env, key, &pid)) {
            ErlNifEnv* msg_env = enif_alloc_env();
            enif_send(env, &pid, msg_env, enif_make_atom(msg_env, "ready4more"));
            enif_free_env(msg_env);
          }
          notify_flag = 0;
        }
        new_binary =
          enif_make_sub_binary(env,
                               tuple_array[1],
                               bytesize,
                               newsize);

        // insert it into the map along with its label converted as atom
        enif_make_map_put(env, map_out,
                          key,
                          enif_make_tuple2(env, enif_make_int(env, notify_flag), new_binary),
                          &map_out);
      } else if(notify_flag){
        ErlNifPid pid;
        if(enif_get_local_pid(env, key, &pid)) {
          ErlNifEnv* msg_env = enif_alloc_env();
          enif_send(env, &pid, msg_env, enif_make_atom(msg_env, "ready4more"));
          enif_free_env(msg_env);
        }
      }
      enif_map_iterator_next(env, &iter);
    }

    enif_map_iterator_destroy(env, &iter);
    out_array[channel] = map_out;
  }
  return enif_make_tuple_from_array(env, out_array, unit->channels);
}

static ERL_NIF_TERM float_list_to_binary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  ERL_NIF_TERM float_list, new_binary, head, tail;
  unsigned int list_length, bin_size;
  double frame;
  FRAME_TYPE * float_data;

  float_list = argv[0];
  if(!enif_get_list_length(env, float_list, &list_length)){
    return enif_make_badarg(env);
  }

  bin_size = list_length * FRAME_SIZE;
  float_data = (FRAME_TYPE *) enif_make_new_binary(env, bin_size, &new_binary);
  while(enif_get_list_cell(env, float_list, &head, &tail)){
    if(!enif_get_double(env, head, &frame)) {
      return enif_make_badarg(env);
    }
    *float_data++ = (FRAME_TYPE) frame;
    float_list = tail;
  }
  return new_binary;
}

static ErlNifFunc nif_funcs[] = {
  {"open_handle", 1, pcm_open_handle, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"close_handle", 1, pcm_close_handle},
  {"dump", 1, pcm_dump, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"set_params", 5, pcm_set_params, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"write", 1, pcm_write},
  {"prepare", 1, pcm_prepare},
  {"start", 1, pcm_start},
  {"recover", 2, pcm_recover},
  {"avail_update", 1, pcm_avail_update},
  {"add_async_handler", 2, pcm_add_async_handler},
  {"add_fd", 1, pcm_add_fd},
  {"enable_ready4write", 1, pcm_enable_ready4write},
  {"sum_map", 2, sum_map},
  {"float_list_to_binary", 1, float_list_to_binary}
};

// ErlNifResourceDtor
static void pcm_resource_dtor(ErlNifEnv* env, void * obj){
  xalsa_t * unit = (xalsa_t*) obj;
  for(unsigned int channel = 0; channel < unit->channels; channel++){
    enif_free(unit->channel_bufs[channel]);
  }
  enif_free(unit->channel_bufs);
}

static int open_pcm_resource_type(ErlNifEnv* env)
{
  const char* mod = "xalsa_nif";
  const char* resource_type = "pcm";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  res_type = enif_open_resource_type(env, mod, resource_type,
                                     pcm_resource_dtor, flags, NULL);
  return ((res_type == NULL) ? -1:0);
}

static int load(ErlNifEnv* caller_env, void** priv_data, ERL_NIF_TERM load_info)
{
  return open_pcm_resource_type(caller_env);
}

static int upgrade(ErlNifEnv* caller_env, void** priv_data, void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
  return open_pcm_resource_type(caller_env);
}

ERL_NIF_INIT(xalsa_pcm, nif_funcs, load, NULL, upgrade, NULL);
