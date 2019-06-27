defmodule Xalsa do
  @moduledoc """
  The Xalsa module implements the API.
  The client may send any number of frames as a binary array of 32 bit floats and may
  optionally receive back a notification in form of a :ready4more message 5-10 ms 
  before all frames are consumed by the ALSA driver.
  """

  @typedoc """
  The sample rate is set in the application environment
  (config file for the application).
  """
  @type rates() :: 44100 | 48000 | 96000 | 192_000

  @doc """
  Send frames in a binary array of Frame:32/float-native.
  If the `notify` flag is true a :ready4more message will be sent to the
  process in the `from` argument when the process frames are due to be consumed
  within 5-10 ms. This so that the process may synthesize/produce more frames.
  """
  @spec send_frames(
          frames :: binary(),
          channel :: pos_integer(),
          notify :: boolean(),
          from :: pid()
        ) :: :ok
  def send_frames(frames, channel, notify \\ false, from \\ self()) do
    notify = boolean_to_integer(notify)
    {procid, localchannel} = :xalsa_manager.get_id(channel)
    send(procid, {:frames, {from, localchannel, frames, notify}})
    :ok
  end

  @doc """
  The client will wait until :ready4more notification is sent from `xalsa` server.
  """
  @spec wait_ready4more() :: :ok
  def wait_ready4more() do
    receive do
      :ready4more -> :ok
    end
  end

  @doc """
  Flush any :ready4more messages from the `xalsa` server in input queue.
  Returns true if there were any.
  """
  @spec flush_ready4more() :: boolean()
  def flush_ready4more(), do: flush_ready4more(false)

  defp flush_ready4more(received) do
    receive do
      :ready4more -> flush_ready4more(true)
    after
      0 -> received
    end
  end

  @doc """
  Return the number of frames that the ALSA driver consumes per callback cycle.
  """
  @spec period_size() :: pos_integer()
  def period_size(), do: :xalsa_manager.period_size()

  @doc "The sample rate, number of frames per second"
  @spec rate() :: rates()
  def rate(), do: :xalsa_manager.rate()

  @doc """
  Lists the PCM names and their number of channels respectively.

  ## Example

      iex> Xalsa.pcms()
      ["plughw:1,0": 2]

  """
  @spec pcms() :: list()
  def pcms(), do: :xalsa_manager.pcms()

  @doc "Total number of channels for all PCM devices together."
  @spec no_of_channels() :: pos_integer()
  def no_of_channels, do: :xalsa_manager.no_of_channels()

  @doc """
  The maximum buffer prepare time in µs, for all started pcm cards,
  since application start or last reset.

  ## Examples

      iex> Xalsa.max_mix_time()
      [254]

  """
  @spec max_mix_time() :: [pos_integer()]
  def max_mix_time(), do: :xalsa_manager.max_mix_time()

  @doc "Reset the max prepare time gauges to 0"
  @spec clear_max_mix_time() :: :ok
  def clear_max_mix_time(), do: :xalsa_manager.clear_max_mix_time()

  @doc "Convert a list of (Erlang) floats to a binary of 32 bit (C) floats"
  @spec float_list_to_binary([float()]) :: binary()
  def float_list_to_binary(fl) do
    :xalsa_pcm.float_list_to_binary(fl)
  end

  @spec boolean_to_integer(boolean()) :: 0 | 1
  defp boolean_to_integer(true), do: 1
  defp boolean_to_integer(false), do: 0
end
