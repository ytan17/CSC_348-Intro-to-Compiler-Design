import fastparse.all._

// From SBT: ~run-main Ex01

object Ex01 {
  def test (p : Parser[Unit], s : String) : Unit = {
    val result : fastparse.core.Parsed[Unit, Char, String] = p.parse (s) 
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

    // Just "hello".
    val p01 : Parser[Unit] = P ("hello") // we want "hello" as input
    test (p01, "hello") // We got "hello" as input, so it should work correctly
    test (p01, "hellox") // successfully parsed. Result is (). Index is 5.
    test (p01, "he") // Failed to parse "he". Last parser is "hello". Index is 0. Extra is Extra(...)
    test (p01, "xhello") // Failed.
    println ("=" * 80)

    // "hello" followed by "world" (~ is called SEQUENCING).
    val p02 : Parser[Unit] = P ("hello" ~ "world") // p02,another parser. 
      //"hello" is a parser. "world" is a parser. "~" means run the first parser first, then run the second parser.
    test (p02, "helloworld") // Successed.
    test (p02, "hello world") // Failed. -"hello" is successfully read, but failed on "world"
    test (p02, "hello") 
    test (p02, "hellowor")
    println ("=" * 80)

    // "hello" repeated zero or more times [.rep], followed by exactly one "world".
    val p03 : Parser[Unit] = P ("hello".rep ~ "world")
    test (p03, "world") // successed.
    test (p03, "helloworld") //ok
    test (p03, "hellohelloworld") //ok
    test (p03, "hellohellohelloworld") //ok
    println ("=" * 80)

    // ("hello" followed by "world") repeated zero or more times.
    // NOTE: Look at index carefully!!!
    val p04 : Parser[Unit] = P (("hello" ~ "world").rep)
    test (p04, "world") //ok. index is 0. -repeated zero.
    test (p04, "helloworld") //ok. index is 10.
    test (p04, "hellohelloworld") //ok. index is 0
    test (p04, "hellohelloworldworld") //ok. index is 0.
    test (p04, "helloworldhelloworld") //ok. index is 20.
    println ("=" * 80)

    // (either "hello" or "world") repeated zero or more times.
    // NOTE: Look at index carefully!!!
    val p05 : Parser[Unit] = P (("hello" | "world").rep)
    test (p05, "world") //ok. index is 5.
    test (p05, "helloworld") //ok. index is 10.
    test (p05, "hellohelloworld") //ok. index is 15.
    test (p05, "hellohelloworldworld") //ok. index is 20.
    test (p05, "helloworldhelloworld") //ok. index is 20.
    println ("=" * 80)

    // ("hello" followed by an optional "world") repeated zero or more times.
    // NOTE: Look at index carefully!!!
    val p06 : Parser[Unit] = P (("hello" ~ "world".?).rep)
    test (p06, "world") // F. "?" is only for "world". index is 10.
    test (p06, "helloworld") //ok. index is 10.
    test (p06, "hellohelloworld") //ok. index is 20.
    test (p06, "hellohelloworldworld") //ok. index is 15.  --hello,0 world; hello, world; bcs no hello, stop to read "world".
    test (p06, "helloworldhelloworld") //ok. index is 20.
    println ("=" * 80)

    // ("hello" followed by "world") repeated zero or more times but must finish at the end of the string.
    // NOTE: compare with p04 above.
    val p07 : Parser[Unit] = P (("hello" ~ "world").rep ~ End)
    test (p07, "world") //F
    test (p07, "helloworld") //ok
    test (p07, "hellohelloworld") //F
    test (p07, "hellohelloworldworld") //F
    test (p07, "helloworldhelloworld") //ok
    println ("=" * 80)

      //[0-9]+
    val digits : Parser[Unit] = P (CharIn ('0' to '9').rep (1))
    val twoDigits : Parser[Unit] = P (CharIn ('0' to '9').rep (exactly=2))

      //write a parser for month, and refer it to parser for day.
    val month : Parser[Unit] = P ("Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec")
    val day : Parser[Unit] = digits
    val date : Parser[Unit] = month ~ " " ~ day
    val time : Parser[Unit] = twoDigits ~ ":" ~ twoDigits ~ ":" ~ twoDigits
    val hostname  = P (CharIn ('a' to 'z').rep (1))
    val process  = P ("sshd" ~ "[" ~ digits ~ "]")
    val message : Parser[Unit] = P (AnyChar.rep)
    val app01 : Parser[Unit] = P (date ~ " " ~ time ~ " " ~ hostname ~ " " ~ process ~ ":" ~ message ~ End) //look for " " - a space; "End" - force it to read until the end.
      
    test (app01, "Jan 29 07:07:02 reed sshd[15279]: User root from 95-24-31-141.broadband.corbina.ru not allowed because not listed in AllowUsers")
    println ("=" * 80)
  }
}