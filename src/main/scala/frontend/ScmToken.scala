package llmaam.frontend.scm

import scala.util.parsing.combinator.RegexParsers

/** Reader data, also used to retain patterns and macro templates before expansion. */
enum Datum:
  case Atom(text: String)
  case Lst(items: List[Datum], tail: Option[Datum] = None)
  case Vec(items: List[Datum])

  def pretty: String = this match
    case Atom(s) => s
    case Lst(xs, tail) => (xs.map(_.pretty) ++ tail.map(t => ". " + t.pretty)).mkString("(", " ", ")")
    case Vec(xs) => xs.map(_.pretty).mkString("#(", " ", ")")

trait SchemeTokenParser extends RegexParsers:
  import Datum.*

  override protected val whiteSpace = """(?:\s++|;[^\r\n]*+)++""".r
  private val delimiter = """(?=$|#;|[\s()\[\]{}"';`,])"""
  private val dot = ("\\." + delimiter).r
  private val atom = (
    """"(?:\\.|[^"\\])*+"""" + "|" +
    """#\\(?:[a-zA-Z]+|(?s:.))""" + delimiter + "|" +
    """(?i:#[tf])""" + delimiter + "|" +
    """[^\s()\[\]{}"';`,#]++""" + delimiter
  ).r
  private val abbreviations = Map("'" -> "quote", "`" -> "quasiquote", "," -> "unquote", ",@" -> "unquote-splicing")
  private val closing = Map("(" -> ")", "[" -> "]", "{" -> "}")

  protected def comment: Parser[Unit] = "#;" ~! datum ^^^ ()
  private def items: Parser[List[Datum]] =
    rep(comment ^^^ None | raw ^^ (Some(_))) ^^ (_.flatten)

  private def raw: Parser[Datum] =
    ("(" | "[" | "{") >> { open =>
      commit((items ~ opt(dot ~> datum <~ rep(comment)) <~ closing(open)) ^? {
        case xs ~ tail if xs.nonEmpty || tail.isEmpty => Lst(xs, tail)
      })
    } | "#(" ~> commit(items <~ ")") ^^ Vec.apply |
      (",@" | "," | "'" | "`") ~! datum ^^ { case prefix ~ d => Lst(List(Atom(abbreviations(prefix)), d)) } |
      not(dot) ~> atom ^^ Atom.apply

  protected def datum: Parser[Datum] = rep(comment) ~> raw
