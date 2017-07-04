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


  defmodule Hardware do
    @gpioa <<0x12>>
    @gpiob <<0x13>>
    @iodira <<0x00>>
    @iodirb <<0x01>>
    @iocon <<0x0A>>
    @gppua <<0x0C>>
    @gppub <<0x0D>> # This might not be necessary because serial write mode

    def init(args \\ []) do
      [ dev ] = I2C.device_names()
      {:ok, pid} = I2C.start_link(dev, 0x20)

      # Set IOCON: Slew rate disabled # Maybe we don't want this?
      I2C.write(pid, @iocon <> <<0x00>>)
      I2C.write(pid, @gppua <> <<255, 255>>) # Set all pullups everywhere
      I2C.write(pid, @iodira <> <<255, 255>>) # Set all GPIO inputs
      I2C.write(pid, @gpioa <> <<255, 255>>) # Make output latch agree with pulled-up pins
      # TODO Wtf does this mean see
      # https://github.com/joeyoung/arduino_keypads/blob/master/Keypad_MC17/Keypad_MC17.cpp#L112

      # TODO Tweak the order of these
      rows = :lists.seq(0, 7) # @gpioa
      cols = :lists.seq(8, 15) # @gpiob

      # pin_state tracks the :high/:low state of each pin (when it's an output)
      pin_state = pin_state_set(pid)
      # iodir_state tracks the IO dir of each pin
      iodir_state = 0xFFFF
      # NB We keep {pin,iodir}_state as integers because it's easier
      # for the Bitwise operations later
      state = %{
        pid: pid,
        rows: rows, cols: cols,
        pin_state: pin_state,
        iodir_state: iodir_state}
      {:ok, state}
    end

    """
    key_state: idle | pressed | hold | released
    mode: :output | :input | :input_pullup
    """

    defp pin_state_set(pid) do
      << pin_state :: integer-size(16) >> = I2C.write_read(pid, @gpioa, 2)
      pin_state
    end

    defp pin_read(%{pid: pid}, pin) do
      << values :: integer-size(16) >> = I2C.write_read(pid, @gpioa, 2)
      mask = 0b1 <<< pin
      case values &&& mask do
        ^mask -> true
        _ -> false
      end
    end

    defp pin_write(%{pid: pid, pin_state: pin_state}=state, pin, level) do
      mask = 0b1 <<< pin
      pin_state = mask_pin_state(level, pin_state, mask)
      I2C.write(pid, @gpioa <> << pin_state :: integer-size(16) >>)
      %{state | pin_state: pin_state}
    end
    defp mask_pin_state(:high, pin_state, mask), do: pin_state ||| mask
    defp mask_pin_state(_low , pin_state, mask), do: pin_state &&& ~~~mask

    # Set the mode for an entire gpio bank (@gpioa or @gpiob via @iodira and @iodirb resp.)
    defp gpio_mode(state, offset, mode) when offset in [ 0, 8 ] do
      # NB offset should be either 0 or 8
      mask = 0b11111111 <<< offset
      mask_mode(state, mode, mask)
    end

    defp pin_mode(state, pin, mode) do
      mask = 0b1 <<< pin
      mask_mode(state, mode, mask)
    end

    defp mask_mode(%{pid: pid, iodir_state: iods}=state, mode, mask) do
      iods = mask_iodir_state(mode, iods, mask)
      I2C.write(pid, @iodira <> << iods :: integer-size(16) >>)
      %{ state | iodir_state: iods }
    end

    defp mask_iodir_state(:output, iodir_state, mask), do: iodir_state &&& ~~~mask
    defp mask_iodir_state(_input , iodir_state, mask), do: iodir_state ||| mask

    # rows: list of pins (0-idxd) on @gpiob
    # cols: list of pins (0-idxd) on @gpioa
    def scan_keys(%{pid: pid, rows: rows, cols: cols,
                    pin_state: pin_state, iodir_state: iodir_state}=state) do
      # Set all row pins to INPUT PULLUP
      state = gpio_mode(state, 0, :input_pullup)

      read_row = fn(r) -> not pin_read(state, r) end
      # For each column pin
      # - set to OUTPUT
      # - set LOW
      # - for each row pin:
      #  - read pin, invert (active low, invert to high)
      # - set HIGH
      # - set to INPUT
      read_col = fn(c, %{pin_state: ps, iodir_state: iods}=st) ->
        st = st |> pin_mode(c, :output) |> pin_write(c, :low)
        row_vals = Enum.map(rows, read_row)
        st = st |> pin_write(c, :high) |> pin_mode(c, :input)
        {row_vals, st}
      end
      {matrix, state} = Enum.map_reduce(cols, state, read_col)
    end

  end
  end

  def transpose([r|_]=matrix), do: transpose(matrix, Enum.map(r, fn _ -> [] end))
  def transpose([], matrix), do: matrix |> Enum.map(&Enum.reverse/1)
  def transpose([row | rows], matrix), do: transpose(rows, add_row(row, matrix, []))
  def add_row([], [], cols), do: cols |> Enum.reverse
  def add_row([v | vs], [c | cs], cols), do: add_row(vs, cs, [ [ v|c ] | cols ])


end
