defmodule Neuronome.BeatGrid do
  @moduledoc """
  A 'performance profile' for the Neuronome, which aims to simulate
  the basic functionality of a simple grid controller

  Each column represents a beat in the bar
  Each row represents a sample to trigger on a beat
  Most important is that the lights reflect what was sent to SonicPi!!
  """
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Convenience function to start the session
  """
  def begin() do
    bg = case Performance.begin(__MODULE__) do
      {:ok, bg} -> bg
      {:error, {:already_started, bg}} -> bg
    end
    GenServer.cast(bg, :begin)
    {:ok, bg}
  end

  @doc """
  Callback fun for :osc_server on ticks
  """
  def tick(t) do
    GenServer.cast(__MODULE__, {:tick, t})
  end

  @doc """
  Callback fun to sync loop starts
  """
  def loop() do
    GenServer.cast(__MODULE__, {:loop})
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
    st0 = default_state()
    {:ok, st0}
  end

  defp default_state() do
    buttons = Enum.into(:lists.seq(9, 64), %{}, fn x -> {x, << 0x00 >>} end)
    client =
      case OSC.Client.start_link() do
        {:ok, client} -> client
        {:error, {:already_started, client}} -> client
      end
    %{ pos: 0 , running: false, buttons: buttons, client: client, reset: false }
  end

  # TODO MOve this
  defp send_times(st0) do
    # messages =
      for r <- :lists.seq(6, 8) do
        ts = get_times(r, st0)
        address = '/beatgrid/sample/' ++ Integer.to_charlist(r)
        # {:message, address, ts}
        OSC.Client.send_message(address, ts)
      end
      # OSC.Client.send_bundle(:immediately, messages)
  end
  defp get_times(r, st0) do
    start = 8 * (r - 1)
    :lists.seq(start+1, start+8)
      |> Enum.filter(fn(b) -> Map.fetch(st0.buttons, b) != {:ok, <<0x00>>} end)
      |> Enum.map(fn(t) -> t - start end)
  end

  def handle_cast(:begin, st0) do
    reset_method('/beatgrid/tick', :tick)
    reset_method('/beatgrid/loop', :loop)
    # Subscribe to button events
    beatgrid = self()
    sub_fwd = fn ->
      # TODO This is hilariously slow for multiple concurrent button presses
      stream = GenEvent.stream(ButtonActivity)
      for event <- stream do
        send(beatgrid, event)
      end
    end
    # TODO This task needs to die when this process dies
    Task.start_link(sub_fwd)
    st1 = %{ st0 | running: true }
    render(st1)
    send_times(st1)
    OSC.Client.send_message('/beatgrid/begin')
    {:noreply, st1}
  end
  def handle_cast({:tick, t}, st0) do
    st1 = %{ st0 | pos: rem(t, 8) }
    # TODO This is hideously side-effect ridden
    render(st1)
    case st1.pos do
      7 ->
        send_times(st1)
        #OSC.Client.send_message('/beatgrid/loopend', [])
      _ -> :ok
    end
    # TODO Maybe we should have a timeout in here somewhere
    # that returns us to pos: 0 if no ticks are received for a while?
    {:noreply, st1}
  end
  def handle_cast({:loop}, st0) do
    pos = st0.pos
    pos = 8 * div(pos, 8)
    st1 = %{ st0 | pos: pos }
    # send_times(st1)
    render(st1)
    OSC.Client.send_message('/beatgrid/loopgo', [])
    {:noreply, st1}
  end
  def handle_cast(:stop, st0) do
    GenServer.cast({:global, :osc_server}, {:delete_method, '/beatgrid/tick'})
    st1 = %{ st0 | pos: 0, running: false }
    render(st1)
    {:noreply, st1}
  end

  # For now we only watch for :released events
  # TODO Add a ~10ms buffer (write MAX every 10ms)
  def handle_info({:button, :hold, 1, _}, %{}=st0) do
    dflt_st = default_state()
    {:noreply, %{ dflt_st | reset: true }}
  end

  def handle_info({:button, :released, 1, _}, %{ reset: true }=st0) do
    {:noreply, %{ st0 | reset: false }}
  end
  def handle_info({:button, :released, 1, _}, %{}=st0) do
    send_times(st0)
    OSC.Client.send_message('/beatgrid/loopgo')
    {:noreply, st0}
  end
  def handle_info({:button, :released, 8, _}, %{}=st0) do
    OSC.Client.send_message('/beatgrid/loopend')
    {:noreply, st0}
  end
  def handle_info({:button, :released, code, _}, %{}=st0) when code > 8 do
    # bottom row = 57-64
    st1 = %{ st0 | buttons: toggle(code, st0.buttons) }
    render(st1)
    {:noreply, st1}
  end
  def handle_info({:button, _, _, _}, st0), do: {:noreply, st0}

  defp toggle(button, %{}=buttons) do
    %{ ^button => val } = buttons
    Map.put(buttons, button, toggle(val))
  end
  defp toggle(<<0x00>>), do: <<0x01>>
  defp toggle(<<0x01>>), do: <<0x00>>

  defp render(st0) do
    beat_btn = 1 + st0.pos
    beat_rowbytes = Neuronome.Matrix.set(beat_btn, <<0xff>>, :binary.copy(<<0x00>>, 8))
    body = st0.buttons
           |> Enum.sort(fn({x, _}, {y, _}) -> x > y end)
           |> Enum.reverse
           |> Enum.into(<<>>, fn {_, b} -> b end)
    Neuronome.Matrix.write(beat_rowbytes<>body)
  end
  defp reset_method(address, callback) do
    GenServer.cast({:global, :osc_server}, {:delete_method, address})
    GenServer.cast({:global, :osc_server}, {:add_method, address, __MODULE__, callback})
  end
end
