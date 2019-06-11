defmodule XalsaTest do
  use ExUnit.Case
  ## doctest Xalsa

  test "play it sam" do
    allowed_rates = [44100, 48000, 96000, 192_000]
    Application.start(:xalsa)
    rate = Application.get_env(:xalsa, :rate)
    assert rate in allowed_rates
    duration = frames(0.4, rate)

    pitch = fn tone, m ->
      {_phase, bin} = bin_one_buf(0, duration, step(freq(tone), rate, m))
      bin
    end

    notes = [:C, :D, :E, :F, :G, :A, :B]
    [c, d, e, f, g, a, _b] = for x <- notes, do: pitch.(x, 1)
    ## double frequency
    [c2, d2, e2, f2, g2, a2, _b2] = for x <- notes, do: pitch.(x, 2)
    twinkle = [c, c, g, g, a, a, g <> g, f, f, e, e, d, d, c <> c]
    twinkle2 = [c2, c2, g2, g2, a2, a2, g2 <> g2, f2, f2, e2, e2, d2, d2, c2 <> c2]
    spawn(fn -> send_notes(twinkle, 1) end)
    spawn(fn -> send_notes(twinkle2, 1) end)
    spawn(fn -> send_notes(twinkle, 2) end)
    spawn(fn -> send_notes(twinkle2, 2) end)
    :timer.sleep(1000)

    [mtime] = Xalsa.max_mix_time()
    assert mtime > 0
    assert mtime < 1000
    Xalsa.clear_max_mix_time()
    [mtime2] = Xalsa.max_mix_time()
    assert mtime2 < mtime

    :timer.sleep(6000)
    assert is_number(Xalsa.period_size())
    assert is_number(Xalsa.no_of_channels())
    Application.stop(:xalsa)
  end

  defp send_notes([], _) do
    :ok
  end

  defp send_notes([h | t], channel) do
    Xalsa.send_frames(h, channel, true)
    Xalsa.wait_ready4more()
    :timer.sleep(70)
    send_notes(t, channel)
  end

  defp step(freq, rate, m), do: 2 * :math.pi() * m * freq / rate

  defp frames(timeinseconds, rate), do: round(timeinseconds * rate)

  defp bin_one_buf(startphase, bufsize, step) do
    bin_one_buf(startphase, bufsize, step, 0.2)
  end

  defp bin_one_buf(startphase, bufsize, step, v) do
    {phase, acc} = one_buf(startphase, bufsize, step, v)
    {phase, Xalsa.float_list_to_binary(acc)}
  end

  defp one_buf(startphase, bufsize, step, v) do
    one_buf(startphase, bufsize, bufsize, step, v, [])
  end

  defp one_buf(phase, 0, _, _, _, acc) do
    {phase, acc}
  end

  defp one_buf(phase, n, bufsize, 0, v, acc) do
    one_buf(phase, n - 1, bufsize, 0, v, [0.0 | acc])
  end

  defp one_buf(phase, n, bufsize, step, v, acc) do
    x = v * :math.sin(phase)
    one_buf(phase + step, n - 1, bufsize, step, v, [x | acc])
  end

  defp freq(tone), do: freq(tone, 4)

  defp freq(:A, 3), do: 220.0
  defp freq(:Asharp, 3), do: 233.08
  defp freq(:B, 3), do: 246.94
  defp freq(:C, 4), do: 261.63
  defp freq(:Csharp, 4), do: 277.18
  defp freq(:D, 4), do: 293.66
  defp freq(:Dsharp, 4), do: 311.13
  defp freq(:E, 4), do: 329.63
  defp freq(:F, 4), do: 349.23
  defp freq(:Fsharp, 4), do: 369.99
  defp freq(:G, 4), do: 392.00
  defp freq(:Gsharp, 4), do: 415.30
  defp freq(:A, 4), do: 440.00
  defp freq(:Asharp, 4), do: 466.16
  defp freq(:B, 4), do: 493.88
  defp freq(:C, 5), do: 523.25
  defp freq(:Csharp, 5), do: 554.37
  defp freq(:D, 5), do: 587.33
end
