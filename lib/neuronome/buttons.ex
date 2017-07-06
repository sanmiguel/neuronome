defmodule Neuronome.Buttons do
  use Bitwise

  alias ElixirALE.I2C
  alias Neuronome.Buttons.Hardware
  alias Neuronome.Buttons.Key
  alias Neuronome.Buttons.Grid

  @debounce_time 10
  @max_active 10


  @moduledoc """
  Buttons.Process is a cut-down process running a tight loop.
  NOte that currently this provides no OTP functionality.
  """
  defmodule Process do
    alias Neuronome.Buttons.Hardware
    alias Neuronome.Buttons

    @debounce_time 10
    defp millis(), do: :os.system_time / 1_000_000 |> trunc

    def start_link() do
      fun = fn() -> __MODULE__.start() end
      spawn_link(fun)
    end

    def start() do
      {:ok, state} = Hardware.init()
      loop(millis(), state, [])
    end

    def loop(lasttime, state, keybuffer) do
      timenow = millis()
      delta = timenow - lasttime
      cond do
        delta < @debounce_time ->
          loop(lasttime, state, keybuffer)
        delta >= @debounce_time ->
          {scan, state} = Hardware.scan_keys(state)
          {activity, grid} = Buttons.update_list(keybuffer, scan, state.grid)
          case activity do
            [] -> :ok
            _ ->
              for %Key{code: co, char: ch, state: st} <- activity do
                GenEvent.notify(ButtonActivity, {:button, st, co, ch})
              end
          end
          loop(timenow, %{state | grid: grid}, keybuffer)
      end
    end
  end

  defmodule ActivityHandler do
    use GenEvent

    def start_link() do
      {:ok, pid} = GenEvent.start_link(name: ButtonActivity)
      :ok = GenEvent.add_handler(pid, __MODULE__, [])
      {:ok, pid}
    end

    def handle_event({:button, state, code, char}=act, activity)
        when state in [ :pressed, :released, :idle, :hold ] do
      {:ok, activity ++ [act]}
    end
  end

  @moduledoc """
  This module provides a gen_server that starts and links to the Button.Process above,
  allowing us to fit it into an OTP supervision tree.
  """
  defmodule Bridge do
    use GenServer

    alias Neuronome.Buttons.Process

    def start_link() do
      GenServer.start_link(__MODULE__, [], name: Buttons.Bridge)
    end

    # TODO This process is probably not the best place for the keypress log
    # to go to but whatever
    def init(args \\ []) do
      proc = Process.start_link()
      {:ok, {proc, []}}
    end

    def handle_call(:flush, _, {proc, log}) do
      {:reply, {:ok, log}, {proc, []}}
    end

    def handle_info({:activity, activity}, {proc, log}) do
      {:noreply, {proc, log ++ activity}}
    end
  end
  end

  def transpose([r|_]=matrix), do: transpose(matrix, Enum.map(r, fn _ -> [] end))
  def transpose([], matrix), do: matrix |> Enum.map(&Enum.reverse/1)
  def transpose([row | rows], matrix), do: transpose(rows, add_row(row, matrix, []))
  def add_row([], [], cols), do: cols |> Enum.reverse
  def add_row([v | vs], [c | cs], cols), do: add_row(vs, cs, [ [ v|c ] | cols ])

  def update_list(lastkeys, results, %Grid{}=grid) do
    # Delete IDLE keys
    {_idles, lastkeys} = Enum.split_with(lastkeys, fn(%{state: st})-> st == :idle end)
    ## TODO DO we actually need the lastkeys persisted across scans like this?
    # It leads to it becoming effectively a log of changes to keys
    # unless we do something to remove items from it
    {activekeys, keymap} = update_row(results, grid.keys, lastkeys, [])
    {activekeys, %Grid{grid | keys: keymap}}
  end

  def update_row([], [], active, keys), do: {active, Enum.reverse(keys)}
  def update_row([rl | rls], [kl | kls], active, keys) do
    {active, keycol} = update_col(rl, kl, active, [])
    update_row(rls, kls, active, [ keycol | keys ])
  end
  def update_col([], [], active, keys), do: {active, Enum.reverse(keys)}
  def update_col([r | rs], [k | ks], active, keys) do
    k = Key.next_state(k, r)
    if k.state_changed do
      update_col(rs, ks, [k|active], [k|keys])
    else
      update_col(rs, ks, active, [k|keys])
    end
  end
end
