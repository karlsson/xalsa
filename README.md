# Xalsa

**Elixir ALSA Connector**

Actually most code is in Erlang but with an API in Elixir as frontend.
[ALSA](https://www.alsa-project.org/wiki/Main_Page) stands for Advanced Linux Sound Architecture. Xalsa uses NIFs to connect to the ALSA library.

So take **note** that this application will only run under Linux.

The idea is to create some base for experimenting with sound synthesis in Elixir/Erlang using the process concept as central part in the architecture.

## What it is

Sound card drivers are configured to do asynchronous calls around every 5 ms to the xalsa_server process which delivers the number of frames (PCM samples) to the device driver to keep the card busy for yet another round of 5 ms. The frames are sent to a ring buffer made of two frame buffers. While one is used for generating audio the other is filled from the xalsa server.

The xalsa_server queues samples on a per channel and process id basis, meaning that sample frames sent from one process to a specific channel will be queued behind existing frames from the same process if they exist (not yet consumed by the audio driver). Frames from other processes will be put in their own queue. Frames from the different queues will be mixed before delivered to the driver. This means every process may synthesize and sequence their own tone(s) independently.

The frames are to be in a binary array of 32 bit floats for the C api. The Xalsa module holds a helper function to convert from an Elixir list of floats.

## Installation

- If using from own application, append line: `{:xalsa, "~> 0.2.0"}` to
your dependency list.
- else checkout from github.
- mix deps.get
- mix compile. In order to compile the c code you will need some alsa development libraries to be installed (libasound2-dev).

## Configuration

The [config/config.exs](https://github.com/karlsson/xalsa/blob/master/config/config.exs) file has comments on how to configure your PCM devices. This can be a bit tricky. Alsa commands `aplay -l` and `aplay -L` can be a good start when looking for device names and card numbers.

Default setting of application environment is in mix.exs.

**NOTE**: Other applications on Linux like Pulseaudio may compete with your sound driver resources. In this case consult the documentation for your Linux distribution. For some reason it just worked on my Kubuntu distro.

## Running

- mix test
- `Application.start(:xalsa)` from your own application. Check the test/xalsa_test.exs script for an example.
- or iex -S mix

### Background
I watched the keynote speech [Distributed Jamming and Composition with Sonic Pi and Erlang](https://www.erlang-factory.com/euc2017/sam-aaron) with Sam Aaron and Joe Armstrong from the Erlang User Conference 2017 and found it interesting how the Erlang process and timing abilities could be useful on the higher levels of music composition/synthesis.

[Sonic Pi](https://sonic-pi.net/), fantastic and fun, relies on the SuperCollider (SC) server which is a highly optimized clockwork to synthesize sound. SC has been around for some time and even though highly optimized it does not scale well on todays multicore processors. This is addressed in a new implementation called Supernova which adds a concept of Parallel Groups where the user explicitly can define code that can be executed in parallel. To my understanding the user still has to define the parallel parts though.

So considering that the BEAM (Erlang VM) has good support for multicore and that processes are easily spawned, it was interesting to see if this could be something useful even for these low level parts of sound synthesis.

Some things I wanted to find out:
- How soft realtime Erlang can meet some hard realtime driver conditions.
- How to decouple periodical sound card driver callbacks and distribute to many Erlang (Elixir) processes using processes and message passing. 
- If NIFs are good when optimizing sound generation.
- Check if an old Erlang programmer can learn and appreciate Elixir. Since Elixir is "the new kid in the block" I thought that any sound synthesis software would get more community attention if using Elixir instead of Erlang.

Results sofar is that Xalsa can mixin frames from around 50 erlang processes before missing the buffer timeframe (5 ms) on an ordinary laptop at 44100 Hz sample rate. The SC server manages timeframes in the ms range and with far more (at least 10x) so called UGens. So Xalsa is quite a bit behind but if synthesis can be pushed up some layer and spread on more cores it may still be feasible. The intention is to test this in other applications using Xalsa. This will probably also answer how well Elixir plays in an Erlang wired brain.
