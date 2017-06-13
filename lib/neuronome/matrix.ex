defmodule Neuronome.Matrix do
  use GenServer

  alias ElixirALE.SPI

  def start_link() do
    GenServer.start_link(Neuronome.Matrix, [])
  end
  
  def init(args \\ []) do
    {:ok, spi} = SPI.start_link("spidev0.0",
                                          [delay_us: 750,
                                           speed_hz: 100_000])
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

  def blank() do
    << 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
       0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 >>
  end

  # TODO Replace b < 7 with b < 3
  def byte(r, g, b) when r < 7 and g < 7 and b < 7 do
    << r :: size(3), g :: size(3), b :: size(2) >>
  end

  def gray(l) when l < 7, do: byte(l, l, l)

  defp cloudbase() do
    << 0x00, 0x00, 0x00, 0x37, 0x37, 0x37, 0x00, 0x00,
       0x00, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37, 0x00,
       0x37, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37,
       0x37, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37,
       0x00, 0x37, 0x37, 0x37, 0x37, 0x37, 0x37, 0x00 >>
  end

  def cloud_loop(spi, n) do
    cloud_loop(spi, n, cloudbase(), [drops(1), drops(1), drops(1)])
  end
  def cloud_loop(_, 0, _, _), do: :ok
  def cloud_loop(spi, n, base, [drops1, drops2, _]) do
    drops0 = drops(1)
    cloud = base <> drops0 <> reverse(drops1) <> reverse(drops1)
    cloud = :binary.part(cloud, {0, 64})
    SPI.transfer(spi, cloud)
    :timer.sleep(500)
    cloud_loop(spi, (n-1), base, [drops0, drops1, drops2])
  end

  def byte_to_str(<< x :: binary-size(1) >>) do
    << i :: integer >> = x
    int_to_str(i, 16)
  end

  def int_to_str(i, b) when i < b, do: "0"<>:erlang.integer_to_binary(i, b)
  def int_to_str(i, b), do: :erlang.integer_to_binary(i, b)

  def str(bytes, len), do: str(bytes, <<>>, len, 0)
  defp str(xs, str, len, len), do: str(xs, str<>"\n", len, 0)
  defp str(<<>>, str, _, _), do: str
  defp str(<< x :: binary-size(1), xs :: binary >>, str, len, n) do
    str(xs, str <> " " <> byte_to_str(x), len, n+1)
  end

  def reverse(bin), do: reverse(bin, <<>>)
  defp reverse(<<>>, acc), do: acc
  defp reverse(<< x :: binary-size(1), bin :: binary >>, acc) do
    reverse(bin, acc <> x)
  end

  def cloud() do
    cloudbase() <>
      drops() <> drops() <> drops()
  end

  def drops(drops \\ 1) do
    # TODO wind
    # cloud base width is 6
    # pick a random number 0-5
    idx = Enum.random(0..5)
    rain = <<0x00>> <> :binary.copy(<<0x00>>, idx) <> << 0x01 >>
    rain <> :binary.copy(<<0x00>>, (8 - idx + 1))
  end

  defp pad(bin, _, len) when byte_size(bin) == len, do: bin
  defp pad(bin, b, len) when byte_size(bin) < len, do: pad(bin<>b, b, len)

end
