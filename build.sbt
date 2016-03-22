name := "Pacman"
version := "1.0.3"
scalaVersion := "2.11.7"
libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.60-R9"

// Fork a new JVM for 'run' and 'test:run', to avoid JavaFX double initialization problems
fork := true
