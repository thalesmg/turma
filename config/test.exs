import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :praefectus, PraefectusWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "8RAuwK2Zlhj02EBWkDVp6hBr0HUN0FEtb0Q5sAZX/2YOJ0ukoSILCkS3ZcpKY3BU",
  server: false

# Print only warnings and errors during test
config :logger, level: :warn

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime
