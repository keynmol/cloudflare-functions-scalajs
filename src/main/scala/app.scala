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

// --- FUNCTIONS

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
  given IPBlocks           = IPBlocks(database)

  val headers     = context.request.headers
  val redirectUri = global.URL(context.request.url).setPathname("/").toString

  for
    // processing input
    formData <- context.request.formData()
    voteData <- extractVote(formData)
    ip       <- getIP(headers)

    hotTakeId = voteData._1
    vote      = voteData._2

    hotTakeExists <- hotTakes.get(hotTakes.key(hotTakeId.raw)).map(_.isDefined)
    blocked       <- votingIsBlocked(ip, hotTakeId)

    result <-
      if !hotTakeExists then
        Promise.resolve(badRequest("Hot take doesn't exist!"))
      else if blocked then
        Promise.resolve(
          badRequest(
            "You are termporarily blocked from voting for this hot take"
          )
        )
      else
        // block IP and change vote counter
        blockVoting(ip, hotTakeId) *>
          changeVote(hotTakeId, vote) *>
          Promise.resolve(global.Response.redirect(redirectUri))
  yield result
  end for
end vote

@JSExportTopLevel(name = "onRequestGet", moduleID = "index")
def index(context: EventContext[JSDynamic, String, Params]) =
  val database = context.env.HOT_TAKERY.asInstanceOf[KVNamespace]

  given HotTakes = HotTakes(database)
  given Votes    = Votes(database)

  val htmlHeaders =
    ResponseInit().setHeadersVarargs(
      scala.scalajs.js.Tuple2("Content-type", "text/html")
    )

  listHotTakes.map { takes =>
    global.Response(renderHotTakes(takes).render, htmlHeaders)
  }
end index

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

  def getHotTake(
      id: HotTakeID
  )(using
      hotTakes: HotTakes,
      votes: Votes
  ): Promise[Option[HotTake]] =
    for
      retrieved      <- hotTakes.get(hotTakes.key(id.raw))
      retrievedCount <- votes.get(votes.key(id.raw))

      info  = retrieved.map(_.into(HotTakeInfo))
      count = retrievedCount.flatMap(_.raw.toIntOption) orElse Option(0)
    yield (info zip count).map { case (i, cnt) => HotTake(id, i, cnt) }

  def listHotTakes(using
      hotTakes: HotTakes,
      votes: Votes
  ): Promise[List[HotTake]] =
    hotTakes.list().flatMap { keys =>
      val ids = keys.map(_.into(HotTakeID)).map(getHotTake)
      Promise.all(scalajs.js.Array.apply(ids*)).map(_.toList.flatten)
    }

  def renderHotTake(take: HotTake) =
    import scalatags.Text.all.*
    div(
      cls := "card",
      div(
        cls := "card-body",
        div(
          cls := "row",
          div(cls := "col-2", style := "text-align: center", h1(take.votes)),
          div(
            cls := "col-6",
            cls := "align-middle",
            h2(cls := "card-title", take.info.raw)
          ),
          div(
            cls := "col-3",
            form(
              action := "/vote",
              method := "POST",
              input(`type` := "hidden", name := "id", value := take.id.raw),
              input(
                `type` := "submit",
                value  := "yah",
                name   := "vote",
                cls    := "btn btn-success"
              ),
              input(
                `type` := "submit",
                value  := "nah",
                name   := "vote",
                cls    := "btn btn-danger"
              )
            )
          )
        )
      )
    )
  end renderHotTake

  def renderHotTakes(takes: List[HotTake]) =
    import scalatags.Text.all.*
    html(
      head(
        scalatags.Text.tags2.title("Reviews"),
        link(
          href := "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css",
          rel := "stylesheet"
        )
      ),
      body(
        div(
          cls   := "container",
          style := "padding:20px;",
          h1("Hot takes galore"),
          takes.map(renderHotTake),
          a(
            href := "https://blog.indoorvivants.com/2022-01-25-cloudflare-functions-with-scalajs",
            "What's going on?"
          )
        )
      )
    )
  end renderHotTakes

end Logic

trait OpaqueString[T](using ap: T =:= String):
  def apply(s: String): T = ap.flip(s)
  extension (k: T)
    def raw                                = ap(k)
    def into[X](other: OpaqueString[X]): X = other.apply(raw)

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

  opaque type HotTakeInfo = String
  object HotTakeInfo extends OpaqueString[HotTakeInfo]

  case class HotTake(id: HotTakeID, info: HotTakeInfo, votes: Int)

  enum Vote:
    case Yah, Nah
end Domain

// --- Definitions and QoL interfaces

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

trait KV[T](scope: String)(using ap: T =:= KVNamespace):
  def apply(kv: KVNamespace): T = ap.flip(kv)

  extension (kv: T)
    def key(str: String): KV.Key = KV.Key(str)

    def descope(key: KV.Key): KV.Key =
      if key.raw.startsWith(scope) then KV.Key(key.raw.drop(scope.length))
      else key

    def get(key: KV.Key): Promise[Option[KV.Value]] =
      println(s"Retrieving ${scope + key.raw}")
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
        .`then`(result => result.keys.map(k => descope(KV.Key(k.name))).toList)
  end extension
end KV
