# Xalsa

**Elixir ALSA Connector**

Actually most code is (currently) in Erlang but with an API in Elixir as frontend.
[ALSA](https://www.alsa-project.org/wiki/Main_Page) stands for Advanced Linux Sound Architecture.
Xalsa uses NIFs to connect to the ALSA library.

The idea is to create some base for experimenting with sound synthesis in Elixir/Erlang using the process concept as central part in the architecture.

## What it is

Sound card drivers are configured to do asynchronous calls around every 5 ms to the xalsa_server process which delivers the number of frames (PCM samples) to the device driver to keep the card busy for yet another round of 5 ms. The frames are sent to a ring buffer made of two frame buffers. While one is used for generating audio the other is filled from the xalsa server.

The xalsa_server queues samples on a per channel and process id basis, meaning that sample frames sent from one process to a specific channel will be queued behind existing frames from the same process if they exist (not yet consumed by the audio driver). Frames from other processes will be put in their own queue. Frames from the different queues will be mixed before delivered to the driver. This means every process may synthesize and sequence their own tone(s) independently.

The frames are to be in a binary array of 32 bit floats for the C api. The Xalsa module holds a helper function to convert from an Elixir list of floats.

## Installation

- Currently checkout from github.
- mix deps.get
- mix compile. In order to compile the c code you will need some alsa development libraries to be installed (libasound2-dev).

If using from own application, append line: `{:xalsa, "~> 0.1.0"}` to
your dependency list.


## Configuration

The config/config.exs file has comments on how to configure your PCM devices, which can be a bit tricky. Default setting of application environment is in mix.exs.

## Running

- mix test
- `Application.start(:xalsa)` from your own application. Check the test/xalsa_test.exs script for an example.
- or iex -S mix
