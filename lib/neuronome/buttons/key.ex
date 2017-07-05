defmodule Neuronome.Buttons.Key do
  alias Neuronome.Buttons.Key

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

  # Remember: debouncing is taken care of outside of this module
  # The enclosing module should only call next_state/2 when debouncing is clear
  def next_state(%Key{}=key, button_state) do
    key = set_unchanged(key)
    transition(key, millis(), button_state)
  end

  ## TODO Move this somewhere else
  defp millis(), do: :os.system_time / 1_000_000 |> trunc

  defp transition(%Key{state: :idle}=key, time, @closed) do
    transition_to(%{key | hold_timer: time}, :pressed)
  end
  # TODO NB This uses a timer per key, not a global timer as per the original
  defp transition(%Key{state: :pressed, hold_timer: time_was}=key, time_now, _)
  when (time_now - time_was)> @holdtime do
    transition_to(key, :hold)
  end
  defp transition(%Key{state: :pressed}=key, _, @open) do
    transition_to(key, :released)
  end
  defp transition(%Key{state: :hold}=key, _, @open) do
    transition_to(key, :released)
  end
  defp transition(%Key{state: :released}=key, _, button_state) do
    transition_to(key, :idle)
  end
  defp transition(key, _, _button_state), do: key

  def set_unchanged(%Key{}=key), do: %{key | state_changed: false}

  defp transition_to(%Key{}=key, next_state) do
    # TODO Trigger event listeners (char) / send messages / whatever
    %{key | state: next_state, state_changed: true}
  end

  defp pressed(cols, col, row) do
    :lists.nth(row+1, :lists.nth(col+1, cols))
  end
end

