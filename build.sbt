name := "cloudflare-test"

scalaVersion := "3.1.1"

enablePlugins(ScalaJSPlugin)

scalaJSLinkerConfig ~= { conf =>
  conf
    .withModuleKind(ModuleKind.ESModule)
}
