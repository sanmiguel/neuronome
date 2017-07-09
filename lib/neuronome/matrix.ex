defmodule Neuronome.Matrix do
  use GenServer
  use Bitwise

  alias ElixirALE.SPI

  def start_link() do
    GenServer.start_link(Neuronome.Matrix, [], name: Matrix)
  end

  @doc """
  Writes 64 bytes to the matrix
  """
  def write(bytes) when byte_size(bytes) == 64 do
    GenServer.call(Matrix, {:write, bytes})
  end

  @doc """
  Sets the given button position (1..64) to the given byte
  """
  def set(pos, val), do: GenServer.call(Matrix, {:set, pos, val})
  def set_row(pos, val), do: GenServer.call(Matrix, {:set, pos, val})
  
  def init(args \\ []) do
    {:ok, spi} = SPI.start_link("spidev0.0",
                                          [delay_us: 750,
                                           speed_hz: 100_000])
    initmap = :binary.copy(<< 0x00 >>, 64)
    write(spi, initmap)
    {:ok, validate({spi, initmap})}
  end

  def handle_call({:write, bitmap}, _, {spi, _}) when byte_size(bitmap) == 64 do
    write(spi, bitmap)
    {:reply, :ok, validate({spi, bitmap})}
  end
  def handle_call({:set, pos, val}, _, {spi, bytes}) when byte_size(val) * pos <= 64 do
    newbytes = set(pos, val, bytes)
    write(spi, newbytes)
    {:reply, :ok, validate({spi, newbytes})}
  end

  defp write(spi, bitmap) when byte_size(bitmap) == 64 do
    SPI.transfer(spi, bitmap)
  end

  defp validate({spi, bitmap}=state) when is_pid(spi) and byte_size(bitmap) == 64, do: state

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

  def set(pos, val, bytes) do
    pos = pos - 1 # positions are 1-indexed
    len = bit_size(val)
    << head :: binary-size(pos), _ :: size(len), tail :: binary >> = bytes
    << head::binary, val::binary, tail::binary >>
  end

  # TODO Replace b < 7 with b < 3
  def byte(r, g, b) when r < 7 and g < 7 and b < 7 do
    << r :: size(3), g :: size(3), b :: size(2) >>
  end

  def gray(l) when l < 7, do: byte(l, l, l)

  defp cloudbase() do
    << 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0x00, 0x00,
       0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00,
       0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
       0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
       0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x00 >>
  end

  def term_animate(n) do
    render = fn(fr) -> TermRender.format(fr) |> IO.puts end
    cloud_loop(render, n, cloudbase(), [drops(1), drops(1), drops(1)])
  end
  def cloud_loop(spi, n) do
    render = fn(fr) -> SPI.transfer(spi, fr) end
    cloud_loop(render, n, cloudbase(), [drops(1), drops(1), drops(1)])
  end
  def cloud_loop(_, 0, _, _), do: :ok
  def cloud_loop(render, n, base, [drops1, drops2, _]) do
    drops0 = drops(1)
    cloud = base <> drops0 <> drops1 <> drops2
    cloud = :binary.part(cloud, {0, 64})
    render.(cloud)
    :timer.sleep(500)
    cloud_loop(render, (n-1), base, [drops0, drops1, drops2])
  end

  defmodule TermRender do
    def dot(), do: ""

    def colour(<< r :: 3, g :: 3, b :: 2 >>) do
      r = trunc( 5 * r / 7 )
      g = trunc( 5 * g / 7 )
      b = trunc( 5 * b / 3 )
      IO.ANSI.color(r, g, b)
    end

    def format(bytes), do: format(bytes, 8)
    def format(bytes, len), do: format(bytes, [], len, 0)
    defp format(xs, str, len, len), do: format(xs, [str, "\n"], len, 0)
    defp format(<<>>, str, _, _), do: str
    defp format(<< x :: binary-size(1), xs :: binary >>, str, len, n) do
      format(xs, [ str, " ", IO.ANSI.format([ colour(x), dot() ]) ], len, n+1)
    end

  end

  def byte_to_str(<< x :: binary-size(1) >>) do
    << i :: integer >> = x
    int_to_str(i, 16)
  end


  def int_to_str(i, b) when i < b, do: "0"<>:erlang.integer_to_binary(i, b)
  def int_to_str(i, b), do: :erlang.integer_to_binary(i, b)

  def reverse(bin), do: reverse(bin, <<>>)
  defp reverse(<<>>, acc), do: acc
  defp reverse(<< x :: binary-size(1), bin :: binary >>, acc) do
    reverse(bin, acc <> x)
  end

  def cloud() do
    cloudbase() <>
      drops() <> drops() <> drops()
  end

  def drops(drops \\ 1, len \\ 8) do
    # TODO wind
    # cloud base width is 6
    # pick a random number 0-5
    idx = Enum.random(0..5)
    rain = <<0x00>> <> :binary.copy(<<0x00>>, idx) <> << 0x01 >>
    rain <> :binary.copy(<<0x00>>, (len - idx + 1))
      |> :binary.part({0, len})
  end

  defp pad(bin, _, len) when byte_size(bin) == len, do: bin
  defp pad(bin, b, len) when byte_size(bin) < len, do: pad(bin<>b, b, len)

end
