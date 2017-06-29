package csp.ch03

// From SBT: ~run-main csp.ch03.Ex06

object Ex06 {
  sealed trait JSON
  case class  JString (s:String)              extends JSON
  case class  JNumber (d:Double)              extends JSON
  case class  JObject (m:List[(String,JSON)]) extends JSON
  case class  JArray (m:List[JSON])           extends JSON
  case object JTrue                           extends JSON
  case object JFalse                          extends JSON
  case object JNull                           extends JSON

  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[String] = P (CharIn ('0' to '9').rep (1).!)
    val jint : Parser[String] = P (("-".? ~ ("0" | (CharIn ('1' to '9') ~ CharIn ('0' to '9').rep))).!)
    val frac : Parser[String] = P ("." ~ digits).!
    val exp : Parser[String] = P ("e" ~ digits).!
    val number : Parser[Double] = P (jint ~ frac.? ~ exp.?).!.map (s => s.toDouble)

    val jtrue  : Parser[JSON] =  "true".!.map (_ =>  JTrue)
    val jfalse : Parser[JSON] = "false".!.map (_ => JFalse)
    val jnull  : Parser[JSON] =  "null".!.map (_ =>  JNull)

    val jstring : Parser[String] = P ("\"" ~ CharsWhile (ch => ch != '"' && ch != '\\').! ~ "\"")
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace ((" " | "\n").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

    val jarray : Parser[JSON] = 
      P ("[" ~ (value ~ ("," ~ value).rep).? ~ "]").map { 
        case None             => JArray (Nil)
        case Some ((v, vseq)) => JArray (v :: vseq.toList) 
      }

    val jpair : Parser[(String,JSON)] = 
      P (jstring ~ ":" ~ value)

    val jobject : Parser[JSON] = 
      P ("{" ~ (jpair ~ ("," ~ jpair).rep).? ~ "}").map { 
        case None                => JObject (Nil)
        case Some ((s, v, pseq)) => JObject ((s, v) :: pseq.toList) 
      }

    val value : Parser[JSON] = 
      jstring.map (n => JString (n)) |
      number.map (n => JNumber (n)) | 
      jobject |
      jarray |
      jtrue | 
      jfalse | 
      jnull

    val start : Parser[JSON] = P (value ~ End)
  }

  import fastparse.all.{Parsed,Parser}

  def test [X] (p : Parser[X], s : String) : Unit = {
    val result : fastparse.core.Parsed[X, Char, String] = p.parse (s) 
    result match {
      case Parsed.Success (value, successIndex) => {
        println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, value, successIndex))
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
      }
    }
  }

  def main (args : Array[String]) {
    println ("=" * 80)

    val p01 : Parser[JSON] = MyParsers.start
    test (p01, "5")
    test (p01, "[]")
    test (p01, "[ 1.0, 2, 3e5 ]")
    test (p01, "[ 1.0, [ 11, 12, 13.0 ], 3e5 ]")
    test (p01, "\"foobar\"")
    test (p01, "{ \"foo\" : 1 }")
    test (p01, "{ \"foo\" : \"bar\", \"label\" : [ 1, 2.0e5 ] }")
    test (p01, """{ "foo" : "bar", "label" : [ 1, 2.0e5 ] }""")
    test (p01, """{ 
          "foo" : "bar", 
          "label" : [ 1, 2.0e5 ] 
    }""")
    println ("=" * 80)
  }
}