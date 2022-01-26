package app

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel(name = "onRequest", moduleID = "request_headers")
def request_headers(context: Any) =
  println(context)
  "hello"

@JSExportTopLevel(name = "onRequest", moduleID = "request_method")
def request_method(context: Any) =
  println(context)
  "bye"
