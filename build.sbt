name := "cloudflare-test"
Global / onChangedBuildSource := ReloadOnSourceChanges

scalaVersion := "3.1.1"

enablePlugins(ScalaJSPlugin)

scalaJSLinkerConfig ~= { conf =>
  conf
    .withModuleKind(ModuleKind.ESModule)
}

libraryDependencies +=
  "com.indoorvivants.cloudflare" %%% "worker-types" % "3.3.0"

lazy val buildWorkers = taskKey[Unit]("Copy Scala.js output to the ./function folder")

buildWorkers := {
  // where Scala.js puts the generated .js files
  val output = (Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
  // trigger (if necessary) JS compilation
  val _      = (Compile / fastLinkJS).value

  // where (relative to root of our build) we want to copy them
  val destination = (ThisBuild / baseDirectory).value / "functions"

  // access SBT's logger, for ease of debugging
  val log = streams.value.log

  // .js files produced by Scala.js
  val filesToCopy = IO.listFiles(output).filter(_.ext == "js")
  
  if(destination.exists()) {
    // .js files already at the destination 
    val filesToDelete = IO.listFiles(destination).filter(_.ext == "js")
    // delete stale .js files
    filesToDelete.foreach {f => 
      log.debug(s"Deleting $f")
    }
  }
   
  // copy new .js files to destination
  filesToCopy.foreach {from => 
    val to = destination / from.name
    log.debug(s"Copying $from to $to")
    IO.copyFile(from, to)
  }
}
