defmodule Neuronome.Buttons.Grid do
  alias Neuronome.Buttons.Grid
  alias Neuronome.Buttons.Key

  defstruct [rowpins: [], colpins: [], keys: []]

  def new(rowpins, colpins) do
    %Grid{
      rowpins: rowpins,
      colpins: colpins,
      keys: grid(colpins, rowpins)
    }
  end

  defp grid(colpins, rowpins) do
    grid(colpins, 0, rowpins, [])
  end
  defp grid([], _, _, keys), do: Enum.reverse(keys)
  defp grid([cpin | cpins], colidx, rowpins, keys) do
    grid(cpins, colidx+1, rowpins, [ column(cpin, colidx, rowpins) | keys ])
  end
  defp column(colpin, colidx, rowpins) do
    column(colpin, (1+colidx*length(rowpins)), rowpins, 0, [])
  end
  defp column(colpin, _, [], _, keys), do: Enum.reverse(keys)
  defp column(colpin, offset, [rpin|rpins], ridx, keys) do
    code = offset + ridx
    key = Key.new(code, code)
    column(colpin, offset, rpins, ridx+1, [key| keys])
  end

end
