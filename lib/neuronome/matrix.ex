defmodule Neuronome.Matrix do
  use GenServer

  alias ElixirALE.SPI

  def start_link() do
    GenServer.start_link(Neuronome.Matrix, [])
  end
  
  def init(args \\ []) do
    {:ok, spi} = SPI.start_link("spidev0.0",
                                          [delay_us: 500,
                                           speed_hz: 125_000])
    initmap = :binary.copy(<< 0x00 >>, 64)
    write(spi, initmap)
    {:ok, {spi, initmap}}
  end

  def handle_call({:write, bitmap}, _, {spi, _}) when byte_size(bitmap) == 64 do
    write(spi, bitmap)
    {:reply, :ok, {spi, bitmap}}
  end

  defp write(spi, bitmap) when byte_size(bitmap) == 64 do
    SPI.transfer(spi, bitmap)
  end
end
