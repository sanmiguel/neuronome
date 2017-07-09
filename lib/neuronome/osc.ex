defmodule OSC do
  @moduledoc """
  A place for OSC related behaviour.
  To start with, a client for sending messages to SonicPi
  """
  defmodule Client do
    use GenServer

    def start_link() do
      GenServer.start_link(__MODULE__, [], name: OSC.Client)
    end
    def send_message(address), do: send_message(address, [])
    def send_message(address, args) do
      GenServer.cast(OSC.Client, {:send, :message, address, args})
    end
    def send_messages(messages) do
      GenServer.cast(OSC.Client, {:send_many, messages})
    end
    def send_bundle(:immediately, messages) do
      GenServer.cast(OSC.Client, {:send, :bundle, :immediately, messages})
    end

    def init(args \\ []) do
      {:ok, sock} = :gen_udp.open(0, [:binary])
    end

    def handle_cast({:send, :bundle, :immediately, ms}, sock) do
      bytes = :osc_lib.encode({:bundle, :immediately, ms})
      :ok = :gen_udp.send(sock, '192.168.8.108', 4559, bytes)
      {:noreply, sock}
    end
    def handle_cast({:send_many, messages}, sock) do
      for {addr, args} <- messages, do: send_message(addr, args, sock)
      {:noreply, sock}
    end
    def handle_cast({:send, :message, address, args}, sock) do
      send_message(address, args, sock)
      {:noreply, sock}
    end

    defp send_message(address, args, sock) do
      IO.puts("OSC #{inspect address} #{inspect args}")
      bytes = :osc_lib.encode({:message, address, args})
      :ok = :gen_udp.send(sock, '192.168.8.108', 4559, bytes)
    end
      
  end
end
