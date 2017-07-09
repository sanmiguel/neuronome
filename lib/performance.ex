defmodule Performance do
  defmodule Sup do
    use Supervisor

    def start_link() do
      Supervisor.start_link(__MODULE__, [], [name: Performance.Sup])
    end

    def init([]) do
      childspec = worker(Performance.Worker, [])
      supervise([childspec], strategy: :simple_one_for_one)
    end
  end

  defmodule Worker do
    def start_link(module) do
      module.start_link()
    end
  end

  def begin(mod) do
    Supervisor.start_child(Performance.Sup, [mod])
  end
end
