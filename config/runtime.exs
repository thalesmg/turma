import Config

config Turma.Decurio,
  inventory: %{
    "legionarius" => ["localhost:19876"],
    "decurio" => ["localhost:19876"]
  },
  name: "decurio"

config Turma.Legionarius,
  bind: {"0.0.0.0", 19876}
