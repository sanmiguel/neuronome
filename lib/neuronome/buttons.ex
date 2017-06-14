defmodule Neuronome.Buttons do
  use GenServer

  alias ElixirALE.I2C


  def start_link() do
    GenServer.start_link(Neuronome.Buttons, [])
  end

  def init(args \\ []) do
    {:ok, pid} = I2C.start_link("i2c-1", <<0x20>>)
    I2C.write(pid, <<0x00>>) # Set PortA to
    I2C.write(pid, <<0x01>>) # inputs
    I2C.write(pid, <<0x01>>) # Set PortB to
    I2C.write(pid, <<0x00>>) # outputs

    {:ok, pid}
  end

end
