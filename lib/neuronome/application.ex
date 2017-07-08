defmodule Neuronome.Application do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    # Define workers and child supervisors to be supervised
    children = [
      worker(Neuronome.Buttons.ActivityHandler, []),
      worker(Neuronome.Buttons.Bridge, []),
      worker(Neuronome.Matrix, []),
      worker(Task, [fn -> init_network() end], restart: :transient, id: Nerves.Init.Network),
    ]

    opts = [strategy: :one_for_one, name: Neuronome.Supervisor]
    Supervisor.start_link(children, opts)
  end

  def init_network() do
    [ssid: ssid, key_mgmt: km, psk: k] = Application.get_env(:neuronome, :wlan0)
    {:ok, _} = Nerves.Network.setup("wlan0", ssid: ssid, key_mgmt: km, psk: k)
    {:ok, _} = Nerves.Network.setup("wlan0", ssid: ssid, key_mgmt: km, psk: k)
  end
end
