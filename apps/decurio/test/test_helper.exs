ExUnit.start(capture_log: true)

System.cmd("epmd", ["-daemon"], into: IO.stream(), stderr_to_stdout: true)
