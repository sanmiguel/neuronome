defmodule Neuronome.BeatGrid do
  @moduledoc """
  A 'performance profile' for the Neuronome, which aims to simulate
  the basic functionality of a simple grid controller

  Each column represents a beat in the bar
  Each row represents a sample to trigger on a beat
  """
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Callback fun for :osc_server on ticks
  """
  def tick(t) do
    GenServer.cast(__MODULE__, {:tick, t})
  end

  @doc """
  Use a gen_server for now to manage stateful processing.

  8 beat loop, up to 7 samples (top row is for displaying position
  and, maybe later, controls).

  At any given time we need to know where in the loop we are (1-8)
  and the currently chosen sample positions.

  Pressing a button on a smaple row toggles that beat for that sample
  (which must in turn toggle the light for that button)
  """
  def init(args \\ []) do
    st0 = %{ pos: 0 , running: false }
    {:ok, st0}
  end

  def handle_cast({:tick, t}, st0) do
    st1 = %{ st0 | pos: rem(t, 8) }
    render(st1)
    {:noreply, st1}
  end
  def handle_cast(:start, st0) do
    GenServer.cast({:global, :osc_server}, {:add_method, '/beatgrid/tick', __MODULE__, :tick})
    st1 = %{ st0 | running: true }
    render(st1)
    {:noreply, st1}
  end
  def handle_cast(:stop, st0) do
    GenServer.cast({:global, :osc_server}, {:delete_method, '/beatgrid/tick'})
    st1 = %{ st0 | pos: 0, running: false }
    render(st1)
    {:noreply, st1}
  end

  defp render(st0) do
    beat_btn = 1 + st0.pos
    beat_rowbytes = Neuronome.Matrix.set(beat_btn, <<0xff>>, :binary.copy(<<0x00>>, 8))
    Neuronome.Matrix.set(1, beat_rowbytes)
  end
end
