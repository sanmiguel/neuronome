defmodule Performance do

  def start_session() do
    import Supervisor.Spec, warn: false

    opts = [strategy: :simple_one_for_one, name: Performance.Supervisor]

    childspec = worker(Performance.Worker, [])
    Supervisor.start_link([childspec], opts)
  end

  def begin(mod) do
    Supervisor.start_child(Performance.Supervisor, [mod])
  end

  defmodule Worker do
    def start_link(module) do
      module.start_link()
    end
  end
end
