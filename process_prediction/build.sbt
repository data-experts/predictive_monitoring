mainClass in (Compile, run) := Some("de.huberlin.informatik.promi.experiments.RunScripted")

fork in run := true

javaOptions += "-Xmx12G"