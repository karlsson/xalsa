# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# This configuration is loaded before any dependency and is restricted
# to this project. If another project depends on this project, this
# file won't be loaded nor affect the parent project. For this reason,
# if you want to provide default values for your application for
# third-party users, it should be done in your "mix.exs" file.

# You can configure your application as:
#
#     config :xalsa, key: :value
#
# and access this configuration in your application as:
#
#     Application.get_env(:xalsa, :key)
#
# rate 44100 | 48000 | 96000 | 192000   - default 44100
# Configuring PCM devices can be tricky.
# Use alsa commands aplay -l and aplay -L to find your sound cards and PCM
# devices. Look for PCMs supporting
# "Hardware device with all software conversions"
# You can configure more than one card the second card will get channel
# numbers allocated after the first card. So if two cards with 2 channels
# each are configured the first card will get channel no 1 and 2, second will
# get 3 and 4.
#
# config :xalsa,
#   rate: 44100,
#   pcms: [{:"plughw:PCH,0",
#           [channels: 2, period_size: 256, period_buffer_size_ratio: 2]},
#          {:"plughw:HDMI,3", [channels: 2]}]

# Optional; channels, period_size, period_buffer_size_ratio

# channels default: 2

# period_size default as function of rate
#  44100: 256
#  48000: 256
#  96000: 512
# 192000: 1024

# period_buffer_size_ratio default: 2

# You can also configure a third-party app:
#
#     config :logger, level: :info
#

# It is also possible to import configuration files, relative to this
# directory. For example, you can emulate configuration per environment
# by uncommenting the line below and defining dev.exs, test.exs and such.
# Configuration from the imported file will override the ones defined
# here (which is why it is important to import them last).
#
#     import_config "#{Mix.env()}.exs"
