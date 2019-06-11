#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <erl_nif.h>
#include <alsa/asoundlib.h>

#define FRAME_TYPE float
#define FRAME_SIZE sizeof(float)

#if FRAME_TYPE == float
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define NATIVE_FORMAT SND_PCM_FORMAT_FLOAT_LE
#else
#define NATIVE_FORMAT SND_PCM_FORMAT_FLOAT_BE
#endif
#else
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define NATIVE_FORMAT SND_PCM_FORMAT_FLOAT64_LE
#else
#define NATIVE_FORMAT SND_PCM_FORMAT_FLOAT64_BE
#endif
#endif

static ErlNifResourceType* res_type;

static int set_hwparams(snd_pcm_t *handle,
                        unsigned int *rate, unsigned int *channels,
                        snd_pcm_uframes_t *buffer_size,
                        snd_pcm_uframes_t *period_size){
  int err; //, dir;
  // unsigned int buffer_time = 10000;            /* ring buffer length in us */
  // unsigned int period_time = 5000;            /* period time in us */
  /*
     buffer_size = 2 * period_size
     rate     period_size
     44100    224
     48000    240
     96000    480
     192000   960
  */
  snd_pcm_hw_params_t *params;
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
  err = snd_pcm_hw_params_set_access(handle, params, SND_PCM_ACCESS_RW_NONINTERLEAVED);
  if (err < 0) {
    printf("Access type not available for playback: %s\n", snd_strerror(err));
    return err;
  }
  /* set the sample format */
  err = snd_pcm_hw_params_set_format(handle, params, NATIVE_FORMAT);
  if (err < 0) {
    printf("Sample format not available for playback: %s\n", snd_strerror(err));
    return err;
  }
  /* set the count of channels */
  err = snd_pcm_hw_params_set_channels_near(handle, params, channels);
  if (err < 0) {
    printf("Channels count (%i) not available for playbacks: %s\n", *channels, snd_strerror(err));
    return err;
  }
  /* set the stream rate */
  err = snd_pcm_hw_params_set_rate_near(handle, params, rate, 0);
  if (err < 0) {
    printf("Rate %iHz not available for playback: %s\n", *rate, snd_strerror(err));
    return err;
  }

  /* set the buffer time */
  /* err = snd_pcm_hw_params_set_buffer_time_near(handle, params, &buffer_time, &dir); */
  /* if (err < 0) { */
  /*   printf("Unable to set buffer time %i for playback: %s\n", buffer_time, snd_strerror(err)); */
  /*   return err; */
  /* } */

  err = snd_pcm_hw_params_set_buffer_size(handle, params, *buffer_size);
  if (err < 0) {
    printf("Unable to set buffer size for playback: %s\n", snd_strerror(err));
    return err;
  }

  /* set the period time */
  /* err = snd_pcm_hw_params_set_period_time_near(handle, params, &period_time, &dir); */
  /* if (err < 0) { */
  /*   printf("Unable to set period time %i for playback: %s\n", period_time, snd_strerror(err)); */
  /*   return err; */
  /* } */
  err = snd_pcm_hw_params_set_period_size(handle, params, *period_size, 0);
  if (err < 0) {
    printf("Unable to get period size for playback: %s\n", snd_strerror(err));
    return err;
  }

  /* write the parameters to device */
  err = snd_pcm_hw_params(handle, params);
  snd_pcm_hw_params_free(params);
  if (err < 0) {
    printf("Unable to set hw params for playback: %s\n", snd_strerror(err));
    return err;
  }
  return 0;
}

static ERL_NIF_TERM pcm_open_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char device[128];                     /* playback device */
  int err = 0;
  snd_pcm_t** handle_p = enif_alloc_resource(res_type, sizeof(snd_pcm_t*));

  // we read the erlang string (erlang list)
  if (!enif_get_string(env, argv[0], device, 128, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env); // blame user if address isn't a string
  }

  if ((err = snd_pcm_open(handle_p, device, SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK)) < 0) {
    printf("Playback open error: %s\n", snd_strerror(err));
    return enif_make_badarg(env);
  }

  ERL_NIF_TERM term = enif_make_resource(env, handle_p);
  enif_release_resource(handle_p);
  return term;
}

static ERL_NIF_TERM pcm_close_handle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int ret;

  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }

  ret = snd_pcm_close(*handle_p);
  return enif_make_int(env, ret);
}

static ERL_NIF_TERM pcm_dump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int ret = 0;
  snd_output_t *output = NULL;
  int err;

  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }

  err = snd_output_stdio_attach(&output, stdout, 0);
  if (err < 0) {
    printf("Output failed: %s\n", snd_strerror(err));
    return -1;
  }

  ret = snd_pcm_dump(*handle_p, output);

  return enif_make_int(env, ret);
}

static ERL_NIF_TERM pcm_set_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int ret, channels, rate;
  snd_pcm_uframes_t buffer_size, period_size; // ulong
  snd_pcm_t** handle_p;

  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
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

  buffer_size = 2 * period_size;

  ret = set_hwparams(*handle_p, &rate, &channels, &buffer_size, &period_size);
  if (ret < 0) {
    return enif_make_uint(env, ret);
  }

  snd_pcm_sw_params_t *sw_params;
  snd_pcm_sw_params_malloc(&sw_params);
  snd_pcm_sw_params_current(*handle_p, sw_params);
  snd_pcm_sw_params_set_start_threshold(*handle_p, sw_params,
                                        (buffer_size / period_size) * period_size - 1);
  snd_pcm_sw_params_set_avail_min(*handle_p, sw_params, period_size);
  snd_pcm_sw_params(*handle_p, sw_params);
  snd_pcm_sw_params_free(sw_params);

  return enif_make_tuple4(env,
                          enif_make_uint(env, rate),
                          enif_make_uint(env, channels),
                          enif_make_ulong(env, buffer_size),
                          enif_make_ulong(env, period_size));
}

static ERL_NIF_TERM pcm_writei(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  unsigned size; // No of frames (per channel)
  int err;

  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }
  if (!enif_inspect_binary(env, argv[1], &bin)) {
    return enif_make_badarg(env);
  }
  if (!enif_get_uint(env, argv[2], &size)) {
    return enif_make_badarg(env);
  }

  err = snd_pcm_writei(*handle_p, bin.data, size);
  return enif_make_int(env, err);
}

static ERL_NIF_TERM pcm_writen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary bin;
  ERL_NIF_TERM buf_list;

  unsigned size; // No of frames (per channel)
  unsigned channels;
  int err;
  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }

  buf_list = argv[1];
  if(!enif_get_list_length(env, buf_list, &channels)){
    return enif_make_badarg(env);
  }

  void** bufs = enif_alloc(sizeof(void*) * channels);
  unsigned channel = 0;
  ERL_NIF_TERM head, tail;
  while(channel < channels){
    if(!enif_get_list_cell(env, buf_list, &head, &tail)){
      return enif_make_badarg(env);
    }
    if (!enif_inspect_binary(env, head, &bin)) {
      return enif_make_badarg(env);
    }
    bufs[channel] = bin.data;
    channel++;
    buf_list = tail;
  }

  if (!enif_get_uint(env, argv[2], &size)) {
    return enif_make_badarg(env);
  }

  err = snd_pcm_writen(*handle_p, bufs, size);
  enif_free(bufs);
  return enif_make_int(env, err);
}

static ERL_NIF_TERM pcm_prepare(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }
  return enif_make_int(env, snd_pcm_prepare(*handle_p));
}

static ERL_NIF_TERM pcm_start(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int err = 0;
  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void **) &handle_p)){
    return enif_make_badarg(env);
  }
  if (snd_pcm_state(*handle_p) == SND_PCM_STATE_PREPARED) {
    err = snd_pcm_start(*handle_p);
  }

  return enif_make_int(env, err);
}

static ERL_NIF_TERM pcm_recover(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int errno;
  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void **) &handle_p)){
    return enif_make_badarg(env);
  }
  if (!enif_get_int(env, argv[1], &errno)) {
    return enif_make_badarg(env);
  }
  return enif_make_int(env, snd_pcm_recover(*handle_p, errno, 1));
}


static ERL_NIF_TERM pcm_avail_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }
  return enif_make_int(env, snd_pcm_avail_update(*handle_p));
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
  snd_pcm_t** handle_p;
  if (!enif_get_resource(env, argv[0], res_type, (void**) &handle_p)){
    return enif_make_badarg(env);
  }
  ErlNifPid* pid = (ErlNifPid*) enif_alloc(sizeof(ErlNifPid));
  if (!enif_get_local_pid(env, argv[1], pid)) {
    return enif_make_badarg(env);
  }
  err = snd_async_add_pcm_handler(&ahandler, *handle_p, pcm_async_callback, pid);
  return enif_make_int(env, err);
}

static ERL_NIF_TERM sum_map(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]){
  unsigned i, size; // No of frames
  ERL_NIF_TERM key, value, map_out;
  const ERL_NIF_TERM * tuple_array;
  ErlNifMapIterator iter;
  ErlNifBinary bin;

  if (!enif_map_iterator_create(env, argv[0], &iter, ERL_NIF_MAP_ITERATOR_FIRST)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_uint(env, argv[1], &size)) {
    return enif_make_badarg(env);
  }

  unsigned bytesize = size * FRAME_SIZE;

  ERL_NIF_TERM buf_term;
  FRAME_TYPE *buf = (FRAME_TYPE *) enif_make_new_binary(env, bytesize, &buf_term);

  for(i = 0; i < size; i++)
    buf[i] = 0.0;

  map_out = enif_make_new_map(env);

  FRAME_TYPE* framesp;
  unsigned noframes;
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
      unsigned newsize = bin.size - bytesize;
      // unsigned char * newbin_data =
      // enif_make_new_binary(env, newsize, &new_binary);
      // memcpy(newbin_data, &bin.data[bytesize], newsize);
      new_binary =
	enif_make_sub_binary(env,
			     tuple_array[1],
			     bytesize,
			     newsize);

      // insert it into the map along with its label converted as atom
      enif_make_map_put(env, map_out,
			key,
			enif_make_tuple2(env, tuple_array[0], new_binary),
			&map_out);

      if(notify_flag && newsize <= bytesize){
	ErlNifPid pid;
	if(enif_get_local_pid(env, key, &pid)) {
	  ErlNifEnv* msg_env = enif_alloc_env();
	  enif_send(env, &pid, msg_env, enif_make_atom(msg_env, "ready4more"));
	  enif_free_env(msg_env);
	}
      }
    }
    enif_map_iterator_next(env, &iter);
  }

  enif_map_iterator_destroy(env, &iter);

  return enif_make_tuple2(env, buf_term, map_out);
}

static ErlNifFunc nif_funcs[] = {
  {"open_handle", 1, pcm_open_handle, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"close_handle", 1, pcm_close_handle},
  {"dump", 1, pcm_dump, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"set_params", 4, pcm_set_params, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"writei", 3, pcm_writei},
  {"writen", 3, pcm_writen},
  {"prepare", 1, pcm_prepare},
  {"start", 1, pcm_start},
  {"recover", 2, pcm_recover},
  {"avail_update", 1, pcm_avail_update},
  {"add_async_handler", 2, pcm_add_async_handler},
  {"sum_map", 2, sum_map}
};

static int open_pcm_resource_type(ErlNifEnv* env)
{
  const char* mod = "xalsa_nif";
  const char* resource_type = "pcm";
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  res_type = enif_open_resource_type(env, mod, resource_type,
                                     NULL, flags, NULL);
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
