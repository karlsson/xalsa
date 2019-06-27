-module(xalsa_pcm).
-export([open_handle/1,
	 close_handle/1,
	 dump/1,
	 set_params/4,
	 writei/3,
	 writen/3,
	 prepare/1,
	 start/1,
         recover/2,
         avail_update/1,
         add_async_handler/2,
         writel/3, % List of Lists as input
         errno/1,
	 sum_map/2,
	 float_list_to_binary/1]).

-on_load(load_nifs/0).

-define(APPNAME, xalsa).
-define(LIBNAME, xalsa).

open_handle(_Device) -> not_loaded(?LINE).
close_handle(_Handle) -> not_loaded(?LINE).
dump(_Handle) -> not_loaded(?LINE).
set_params(_Handle, _Channels, _Rate, _PeriodSize) -> not_loaded(?LINE).
writei(_Handle, _PeriodBin, _NoOfFrames) -> not_loaded(?LINE).
writen(_Handle, _PeriodBinList, _NoOfFrames) -> not_loaded(?LINE).
prepare(_Handle) -> not_loaded(?LINE).
start(_Handle) -> not_loaded(?LINE).
recover(_Handle, _ErrNo) -> not_loaded(?LINE).
avail_update(_Handle) -> not_loaded(?LINE).
add_async_handler(_Handle, _Pid) -> not_loaded(?LINE).
sum_map(_Map, _Size) -> not_loaded(?LINE).
float_list_to_binary(_List) -> not_loaded(?LINE).

load_nifs() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

writel(Handle, FramesList, NoOfFrames) ->
    writen(Handle, lists:map(fun float_list_to_binary/1, FramesList), NoOfFrames).

errno(-11) -> eagain;
errno(-32) -> epipe;
errno(-86) -> estrpipe;
errno(A) when A >= 0 -> A.
