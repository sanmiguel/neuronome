defmodule NeuronomeButtonsGridTest do
  use ExUnit.Case

  alias Neuronome.Buttons.Grid

  test "simple 2x2 grid has 4 buttons" do
    grid = Grid.new([0, 1], [2, 3])
    assert 4 == grid.keys |> :lists.flatten |> length
  end

end


