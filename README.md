# Neuronome

A [Nerves](http://www.nerves-project.org)-based physical controller for [SonicPi](http://sonic-pi.net/).

# Hardware

There's still a lot to write up in this section, but hopefully this will give at least some idea of
what I've been working with.

Rough outline of the parts list:
 - 4x Sparkfun 4x4 RGB LED Button pads
 - 1x Sparkfun 8x8 RGB LED Matrix SPI backpack
 - 1x MCP23017 I2C 16-port GPIO
 - A handful of breadboards
 - Nx jumper cables (where N is a sufficiently large integer)
 - 1x FTDI USB Serial cable
 - 1x Raspberry Pi 3 (hopefully to be replaced later with a rpi0 or rpi0w)

How to connect up the Backpack is reasonably straightforward: the connections for the button pads
are of much the same kind as the RGB matrix the backpack is designed for: common cathode columns,
3x input for each row (R, G, B channels).

Connecting the buttons for the MCP23017 is a little more fiddly: rows go to GPIO Bank A, columns to Bank B.
The firmware takes care of the rest, pretty much.

TODO: Schematics, pictures etc.

Set a couple of useful environment vars in the shell you will build the firmware from:

```
$ export NERVES_WIFI_SSID="your case sensitive SSID here"
$ export NERVES_WIFI_PSK="your case sensitive PSK here"
```

Build the firmware from this repo and burn it to an SD card:

```
$ mix firmwware && mix firmware.burn
```

Shove the SD card into the Pi, and boot it up. Obviously it will all work first time because doesn't it always?

# Problems

1: The first annoyance you'll meet is that the wifi doesn't connect properly on first boot. This is a big TODO that I want to
deal with but it's not annoying me enough just yet.

After letting the device boot, I use the convenience fun `Neuronome.wifi()` to trigger connecting.

2: There's very little in the way of control. This is still a very early stage of development, and I really have no idea what I'm doing
so I make it up as I go. Ultimately there will be multiple modes of operation for the device, and the rough idea is that each mode will be
represented by some kind of process, which may or may not be receiving button presses and writing to the LEDs at any time. Switching between modes
is likely to remain a very manual process for now, but later it'll either be via some control sequence on the button pads or maybe via some
extra hardware that I am yet to attach. Watch this space!


# Usage as a controller

BeatGrid
------

With the Beatgrid (start it by running `Neuronome.BeatGrid.begin()` on the console), you place samples (one in each row) in an 8 beat loop sequence.

There's not a lot more to it than that. For now it requires that you manually run (before starting the mode) the file at `sonic-pi/beat_grid.rb`.


### Thanks

HUGE thanks to both [Sam Aaron](https://twitter.com/samaaron) and [Justin Schneck](https://twitter.com/mobileoverlord) for giving me the
inspiration, motivation and advice I needed to get this project really going. Without their respective keynotes at [EUC 2017](http://www.erlang-factory.com/euc2017)
and subsequent encouragement, I would never have made it even this far.



-----------

# Default Nerves instructions:

## Targets

Nerves applications produce images for hardware targets based on the
`MIX_TARGET` environment variable. If `MIX_TARGET` is unset, `mix` builds an
image that runs on the host (e.g., your laptop). This is useful for executing
logic tests, running utilities, and debugging. Other targets are represented by
a short name like `rpi3` that maps to a Nerves system image for that platform.
All of this logic is in the generated `mix.exs` and may be customized. For more
information about targets see:

https://hexdocs.pm/nerves/targets.html#content

## Getting Started

To start your Nerves app:
  * `export MIX_TARGET=my_target` or prefix every command with
    `MIX_TARGET=my_target`. For example, `MIX_TARGET=rpi3`
  * Install dependencies with `mix deps.get`
  * Create firmware with `mix firmware`
  * Burn to an SD card with `mix firmware.burn`

## Learn more

  * Official docs: https://hexdocs.pm/nerves/getting-started.html
  * Official website: http://www.nerves-project.org/
  * Discussion Slack elixir-lang #nerves ([Invite](https://elixir-slackin.herokuapp.com/))
  * Source: https://github.com/nerves-project/nerves
