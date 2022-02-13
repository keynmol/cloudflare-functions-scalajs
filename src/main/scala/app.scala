package app

import com.indoorvivants.cloudflare.cloudflareWorkersTypes.KVNamespace
import com.indoorvivants.cloudflare.cloudflareWorkersTypes.*
import com.indoorvivants.cloudflare.std

import scala.annotation.implicitNotFound
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.Promise
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.{Dynamic as JSDynamic}
import scala.util.NotGiven

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
    database.get("test-counter").`then` {
      case null => 0
      case str  => str.toInt
    }

  def put(value: Int): Promise[Unit] =
    database.put("test-counter", value.toString)

  getCurrent.`then`(num =>
    put(num + 1).`then`(_ => global.Response(s"Current value is $num"))
  )
end test_db

import Logic.*
import DB.*
import Domain.*

@JSExportTopLevel(name = "onRequestPost", moduleID = "vote")
def vote(context: EventContext[JSDynamic, String, Params]) =
  val database = context.env.HOT_TAKERY.asInstanceOf[KVNamespace]

  given hotTakes: HotTakes = HotTakes(database)
  given Votes              = Votes(database)
  given blocks: IPBlocks   = IPBlocks(database)

  val headers = context.request.headers

  for
    // processing input
    formData <- context.request.formData()
    voteData <- extractVote(formData)
    ip       <- getIP(headers)

    hotTakeId = voteData._1
    vote      = voteData._2

    // verifying data state
    hotTakeExists <- hotTakes.get(hotTakes.key(hotTakeId.raw)).map(_.isDefined)
    blocked       <- votingIsBlocked(ip, hotTakeId)

    result <-
      if hotTakeExists && !blocked then
        // block IP and change vote counter
        blockVoting(ip, hotTakeId) *>
          changeVote(hotTakeId, vote) *>
          Promise.resolve(global.Response(""))
      else Promise.resolve(badRequest("NO! STOP! NOOOOOOOO!"))
  yield result
  end for
end vote

extension [A](p: Promise[A])
  inline def map[B](inline f: A => B)(using
      @implicitNotFound("Seems like you need `flatMap` instead of `map`")
      ev: NotGiven[B <:< Promise[?]]
  ): Promise[B] =
    p.`then`(f)

  inline def flatMap[B](inline f: A => Promise[B]): Promise[B] =
    p.`then`(f)

  inline def *>[B](other: Promise[B]): Promise[B] =
    flatMap(_ => other)
end extension

object Logic:
  def badRequest(msg: String): Response =
    global.Response(msg, ResponseInit().setStatus(400))

  def getIP(headers: Headers): Promise[IP] =
    val ipSource =
      Option(headers.get("cf-connecting-ip")) orElse
        Option(headers.get("x-real-ip"))

    ipSource match
      case None    => Promise.reject("Missing X-Real-IP header!")
      case Some(s) => Promise.resolve(IP(s))
  end getIP

  def blockVoting(ip: IP, hotTakeId: HotTakeID)(using
      db: IPBlocks
  ): Promise[Unit] =
    import scala.concurrent.duration.*

    val key = db.key(ip.raw + "-" + hotTakeId.raw)

    db.putWithExpiration(key, KV.Value(""), 24.hours)

  def votingIsBlocked(ip: IP, hotTakeId: HotTakeID)(using
      db: IPBlocks
  ): Promise[Boolean] =
    val key = db.key(ip.raw + "-" + hotTakeId.raw)
    db.get(key).map(_.isDefined)

  def changeVote(hotTakeId: HotTakeID, vote: Vote)(using
      db: Votes
  ): Promise[Unit] =
    val key = db.key(hotTakeId.raw)
    for
      current <- db.get(key)
      intValue = current.flatMap(_.raw.toIntOption).getOrElse(0)
      newValue = vote match
        case Vote.Yah => intValue + 1
        case Vote.Nah => intValue - 1
      _ <- db.put(key, KV.Value(newValue.toString))
    yield ()
  end changeVote

  def extractVote(fd: FormData): Promise[(HotTakeID, Vote)] =
    val hotTakeId: Option[HotTakeID] =
      Option(fd.get("id"))
        .collectFirst { case s: String =>
          s
        }
        .map(HotTakeID.apply)

    val vote: Option[Vote] = Option(fd.get("vote"))
      .collectFirst { case s: String => s.trim.toLowerCase }
      .collectFirst {
        case "yah" => Vote.Yah
        case "nah" => Vote.Nah
      }

    hotTakeId.zip(vote) match
      case None    => Promise.reject("Form data is invalid")
      case Some(o) => Promise.resolve(o)

  end extractVote
end Logic

trait OpaqueString[T](using ap: T =:= String):
  def apply(s: String): T  = ap.flip(s)
  extension (k: T) def raw = ap(k)

object KV:
  opaque type Key = String
  object Key extends OpaqueString[Key]

  opaque type Value = String
  object Value extends OpaqueString[Value]
end KV

object DB:
  opaque type HotTakes = KVNamespace
  object HotTakes extends KV[HotTakes]("hot-take-")

  opaque type Votes = KVNamespace
  object Votes extends KV[Votes]("votes-")

  opaque type IPBlocks = KVNamespace
  object IPBlocks extends KV[IPBlocks]("block-")
end DB

object Domain:
  opaque type IP = String
  object IP extends OpaqueString[IP]

  opaque type HotTakeID = String
  object HotTakeID extends OpaqueString[HotTakeID]

  enum Vote:
    case Yah, Nah
end Domain

trait KV[T](scope: String)(using ap: T =:= KVNamespace):
  def apply(kv: KVNamespace): T = ap.flip(kv)

  extension (kv: T)
    def key(str: String): KV.Key = KV.Key(scope + str)

    def get(key: KV.Key): Promise[Option[KV.Value]] =
      ap(kv)
        .get(scope + key.raw)
        .`then`(str => Option(str))
        .`then`(_.map(KV.Value.apply))

    def put(key: KV.Key, value: KV.Value): Promise[Unit] =
      ap(kv).put(scope + key.raw, value.raw)

    def putWithExpiration(
        key: KV.Key,
        value: KV.Value,
        expiresIn: FiniteDuration
    ): Promise[Unit] =
      ap(kv).put(
        scope + key.raw,
        value.raw,
        KVNamespacePutOptions().setExpirationTtl(expiresIn.toSeconds.toInt)
      )

    def list(): Promise[List[KV.Key]] =
      ap(kv)
        .list(KVNamespaceListOptions.apply().setPrefix(scope))
        .`then`(result => result.keys.map(k => KV.Key(k.name)).toList)
  end extension
end KV
