defmodule NeuronomeButtonsKeyTest do
  use ExUnit.Case

  alias Neuronome.Buttons.Key

  test "new key is new" do
    assert Key.new(1, 1) == Key.new(1, 1)
  end

  test "pressed key is pressed" do
    k = Key.new(0, 0)
    assert :idle == k.state
    k = Key.next_state(k, true)
    assert :pressed == k.state
  end

  test "pressed-released key is released" do
    k = Key.new(0, 0)
    assert :idle == k.state
    k = Key.next_state(k, true)
    k = Key.next_state(k, false)
    assert :released == k.state
  end

  test "idempotent pressings" do
    k = Key.new(0, 0)
    k = Key.next_state(k, true)
    k = Key.next_state(k, true)
    assert :pressed == k.state
  end

  test "released idle is idle" do
    k = Key.new(0, 0)
    k = Key.next_state(k, false)
    assert :idle == k.state
    k = Key.next_state(k, false)
    assert :idle == k.state
  end

  test "press-hold gives :hold" do
    k = Key.new(0, 0)
    t = 0
    k = Key.next_state(k, t, true)
    assert :pressed == k.state
    k = Key.next_state(k, t+250, true)
    assert :pressed == k.state
    k = Key.next_state(k, t+501, true)
    assert :hold == k.state
  end
end
