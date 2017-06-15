defmodule Neuronome.Buttons do
  use GenServer

  alias ElixirALE.I2C


  def start_link() do
    GenServer.start_link(Neuronome.Buttons, [])
  end

  def init(args \\ []) do
    [ dev ] = I2C.device_names()
    {:ok, pid} = I2C.start_link(dev, 0x20)
    # PortA is Col pins
    I2C.write(pid, <<0x00, 0xFF>>) # Set PortA to outputs
    # PortB is Row pins
    I2C.write(pid, <<0x01, 0x00>>) # Set PortB to inputs
    # Set inputs to pullup-resistor
    I2C.write(pid, <<0x0D, 0xFF>>)

    # Set all Columns HIGH
    I2C.write(pid, <<0x12, 0xFF>>)

    ## TODO Debounce
    {:ok, pid}
  end

end
