package spbau.scala.ordian.task03

import scala.util.parsing.combinator._

object JsonParser extends JavaTokenParsers {

  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value)}

  def value: Parser[Any] = (
    obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )

  def run(s: String) = parseAll(obj, s)
}

object Two extends App {
  println(JsonParser.run(
    """
      |{
      |  "id": 1,
      |  "name": "Foo",
      |  "price": 123,
      |  "tags": [
      |    "Bar",
      |    "Eek"
      |  ],
      |  "stock": {
      |    "warehouse": 300,
      |    "retail": 20
      |  }
      |}
    """.stripMargin))
}
