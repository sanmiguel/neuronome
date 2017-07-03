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
      {:ok, {pid, rows, cols, pin_state, iodir_state}, @debounce_time}
    end

    """
    key_state: idle | pressed | hold | released
    mode: :output | :input | :input_pullup
    """

    defp pin_state_set(pid) do
      << pin_state :: integer-size(16) >> = I2C.write_read(pid, @gpioa, 2)
      pin_state
    end

    defp pin_read(pid, pin) do
      << values :: integer-size(16) >> = I2C.write_read(pid, @gpioa, 2)
      mask = 0b1 <<< pin
      case values &&& mask do
        ^mask -> true
        _ -> false
      end
    end

    defp pin_write(pid, pin, level, pin_state) do
      mask = 0b1 <<< pin
      pin_state = mask_pin_state(level, pin_state, mask)
      I2C.write(pid, @gpioa <> << pin_state :: integer-size(16) >>)
      pin_state
    end
    defp mask_pin_state(:high, pin_state, mask), do: pin_state ||| mask
    defp mask_pin_state(_low , pin_state, mask), do: pin_state &&& ~~~mask

    defp pin_mode(pid, pin, mode, iodir_state) do
      mask = 0b1 <<< pin
      iodir_state = mask_iodir_state(mode, iodir_state, mask)
      I2C.write(pid, @iodira <> << iodir_state :: integer-size(16) >>)
      iodir_state
    end

    defp mask_iodir_state(:output, iodir_state, mask), do: iodir_state &&& ~~~mask
    defp mask_iodir_state(_input , iodir_state, mask), do: iodir_state ||| mask

    # rows: list of pins (0-idxd) on @gpiob
    # cols: list of pins (0-idxd) on @gpioa
    def scan_keys(pid, rows, cols, pin_state, iodir_state) do
      # Set all row pins to INPUT PULLUP
      reduce_fun = fn(r, iods) -> pin_mode(pid, r, :input_pullup, iods) end
      iodir_state = Enum.reduce(rows, iodir_state, reduce_fun)

      read_row = fn(r) -> not pin_read(pid, r) end
      # For each column pin
      # - set to OUTPUT
      # - set LOW
      # - for each row pin:
      #  - read pin, invert (active low, invert to high)
      # - set HIGH
      # - set to INPUT
      read_col = fn(c, {ps, iods}) ->
        iods = pin_mode(pid, c, :output, iods)
        ps = pin_write(pid, c, :low, ps)
        row_vals = Enum.map(rows, read_row)
        ps = pin_write(pid, c, :high, ps)
        iods = pin_mode(pid, c, :input, iods)
        {row_vals, {ps, iods}}
      end
      {matrix, {pin_state, iodir_state}} = Enum.map_reduce(cols, {pin_state, iodir_state}, read_col)
    end

  end
  end

  def transpose([r|_]=matrix), do: transpose(matrix, Enum.map(r, fn _ -> [] end))
  def transpose([], matrix), do: matrix |> Enum.map(&Enum.reverse/1)
  def transpose([row | rows], matrix), do: transpose(rows, add_row(row, matrix, []))
  def add_row([], [], cols), do: cols |> Enum.reverse
  def add_row([v | vs], [c | cs], cols), do: add_row(vs, cs, [ [ v|c ] | cols ])


end
