defmodule Neuronome.Buttons do
  use GenServer
  use Bitwise

  alias ElixirALE.I2C
  alias Neuronome.Buttons.Hardware

  defmodule Key do
    @enforce_keys [:code, :char]
    defstruct [state: :idle,
               state_changed: false,
               code: -1, char: -1, hold_timer: nil]

    # TODO open and closed need putting somewhere inheritable
    @closed true
    @open false

    @holdtime 500

    def new(code, char) do
      %Key{ code: code, char: char}
    end

    def next_state(%Key{}=key, idx, button_state) do
      key = set_unchanged(key)
      transition(key, idx, millis(), button_state)
    end

    ## TODO Move this somewhere else
    defp millis(), do: :os.system_time / 1_000_000 |> trunc

    defp transition(%Key{state: :idle}=key, idx, time, @closed) do
      transition_to(%{key | hold_timer: time}, idx, :pressed)
    end
    # TODO NB This uses a timer per key, not a global timer as per the original
    defp transition(%Key{state: :pressed, hold_timer: time_was}=key, idx, time_now, _)
    when (time_now - time_was)> @holdtime do
      transition_to(key, idx, :hold)
    end
    defp transition(%Key{state: :pressed}=key, idx, _, @open) do
      transition_to(key, idx, :released)
    end
    defp transition(%Key{state: :hold}=key, idx, _, @open) do
      transition_to(key, idx, :released)
    end
    defp transition(%Key{state: :released}=key, idx, _, button_state) do
      transition_to(key, idx, :idle)
    end
    defp transition(key, _idx, _, _button_state), do: key

    def set_unchanged(%Key{}=key), do: %{key | state_changed: false}

    defp transition_to(%Key{}=key, _idx, next_state) do
      # TODO Trigger event listeners (char) / send messages / whatever
      %{key | state: next_state, state_changed: true}
    end
    defp pressed(cols, col, row) do
      :lists.nth(row+1, :lists.nth(col+1, cols))
    end
  end

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
