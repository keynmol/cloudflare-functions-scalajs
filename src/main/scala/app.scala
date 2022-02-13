package app

import com.indoorvivants.cloudflare.cloudflareWorkersTypes.*
import com.indoorvivants.cloudflare.std
import com.indoorvivants.cloudflare.cloudflareWorkersTypes.KVNamespace
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.Promise
import scala.scalajs.js.{Dynamic => JSDynamic}

type Params = std.Record[String, scala.Any]

@JSExportTopLevel(name = "onRequest", moduleID = "request_headers")
def request_headers(context: EventContext[Any, String, Params]) =
  val str = StringBuilder()
  context.request.headers.forEach { (_, value, key, _) =>
    str.append(s"Keys: $key, value: $value\n")
  }
  global.Response("hello, world. Your request comes with \n" + str.result)

@JSExportTopLevel(name = "onRequestGet", moduleID = "request_method")
def request_method(context: EventContext[Any, String, Params]) =
  global.Response("Your request came via " + context.request.method)
@JSExportTopLevel(name = "onRequestGet", moduleID = "test_db")
def test_db(context: EventContext[JSDynamic, String, Params]) =
  val database = context.env.HOT_TAKERY.asInstanceOf[KVNamespace]

  def getCurrent: Promise[Int] = 
    database.get("test-counter").`then`{
      case null => 0
      case str => str.toInt
    } 

  def put(value: Int): Promise[Unit] = 
    database.put("test-counter", value.toString)
  
  getCurrent.`then`(num => 
    put(num + 1).`then`(_ => 
      global.Response(s"Current value is $num")
    )  
  )
