defmodule Neuronome.Buttons do
  use GenServer
  use Bitwise

  alias ElixirALE.I2C

  @holdtime 500
  @debounce_time 10
  @max_active 10
  @closed true
  @open false

  def start_link() do
    GenServer.start_link(Neuronome.Buttons, [])
  end


  end

  def transpose([r|_]=matrix), do: transpose(matrix, Enum.map(r, fn _ -> [] end))
  def transpose([], matrix), do: matrix |> Enum.map(&Enum.reverse/1)
  def transpose([row | rows], matrix), do: transpose(rows, add_row(row, matrix, []))
  def add_row([], [], cols), do: cols |> Enum.reverse
  def add_row([v | vs], [c | cs], cols), do: add_row(vs, cs, [ [ v|c ] | cols ])


end
