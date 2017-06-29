import fastparse.all._

// From SBT: ~run-main Ex02

object Ex02 {
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

    // Use ".!" to CAPTURE a value.
    val p01 : Parser[String] = P ("hello".!)  //with ".!", diff: not return unit, it returns string.
    test (p01, "hello") //ok. index is 5.
    test (p01, "hellox") //ok. index is 5.
    test (p01, "he") //F
    test (p01, "xhello") //F
    println ("=" * 80)

    // SEQUENCING gives a pair of values.
    val p02 : Parser[(String, String)] = P ("hello".! ~ "world".!)
    test (p02, "helloworld") //ok
    test (p02, "hello world") //F - parser is white space sensitive.
    test (p02, "hello") //F
    test (p02, "hellowor") //F
    println ("=" * 80)

    // REPEATED "hello" occurrences can be captured as a single string.
      //first string capture "hello" zore or more times, second string capture "world"
    val p03 : Parser[(String, String)] = P ("hello".rep.! ~ "world".!) 
    test (p03, "world") //ok
    test (p03, "helloworld") //ok
    test (p03, "hellohelloworld") //ok
    test (p03, "hellohellohelloworld") //ok
    println ("=" * 80)

    // Alternatively, REPEATED "hello" occurrences can be captured individually and then returned as a Seq (similar to a List).
    val p03b : Parser[(Seq[String], String)] = P ("hello".!.rep ~ "world".!)
    test (p03b, "world") //result is (ArrayBuffer(),world)
    test (p03b, "helloworld") //result is (ArrayBuffer(hello),world)
    test (p03b, "hellohelloworld") //result is (ArrayBuffer(hello,hello),world)
    test (p03b, "hellohellohelloworld")
    println ("=" * 80)

    // Capture ("hello" followed by "world") repeated zero or more times.
      //seq of pair of string
    val p04 : Parser[Seq[(String,String)]] = P (("hello".! ~ "world".!).rep)
    test (p04, "world") //ok. result is ArrayBuffer().
    test (p04, "helloworld") //ok. result is ArrayBuffer((hello,world)).
    test (p04, "hellohelloworld") //ok. result is ArrayBuffer().
    test (p04, "hellohelloworldworld") //ok. result is ArrayBuffer().
    test (p04, "helloworldhelloworld") //ok. result is ArrayBuffer((hello,world),(hello,world)).
    println ("=" * 80)

    // Alternatively, capture "hello" and "world" as a single string.
      //seq of string
    val p04b : Parser[Seq[String]] = P (("hello" ~ "world").!.rep)
    test (p04b, "world")
    test (p04b, "helloworld")
    test (p04b, "hellohelloworld")
    test (p04b, "hellohelloworldworld")
    test (p04b, "helloworldhelloworld") //result is ArrayBuffer((helloworld),(helloworld)).
    println ("=" * 80)

    // Alternatively, capture the entire string.
      //big string, but you may not wanna do this so often.
    val p04c : Parser[String] = P (("hello" ~ "world").rep.!)
    test (p04c, "world")
    test (p04c, "helloworld")
    test (p04c, "hellohelloworld")
    test (p04c, "hellohelloworldworld")
    test (p04c, "helloworldhelloworld") //result is helloworldhelloworld.
    println ("=" * 80)

    // Capture (EITHER "hello" or "world") repeated zero or more times.
    val p05 : Parser[Seq[String]] = P (("hello".! | "world".!).rep)
    test (p05, "world")
    test (p05, "helloworld")
    test (p05, "hellohelloworld")
    test (p05, "hellohelloworldworld")
    test (p05, "helloworldhelloworld") // result is ArrayBuffer(hello,world,hello,world).
    println ("=" * 80)

    // Capture ("hello" followed by an optional "world") repeated zero or more times.
    val p06 : Parser[Seq[(String, Option[String])]] = P (("hello".! ~ "world".!.?).rep)
    test (p06, "world")
    test (p06, "helloworld")
    test (p06, "hellohelloworld")
    test (p06, "hellohelloworldworld") //result is ArrayBuffer((hello,None),(hello,some(world))).
    test (p06, "helloworldhelloworld")
    println ("=" * 80)

    // ("hello" followed by "world") repeated zero or more times but must finish at the end of the string.
    // NOTE: compare with p04 above.
    val p07 : Parser[Seq[(String,String)]] = P (("hello".! ~ "world".!).rep ~ End)
    test (p07, "world")
    test (p07, "helloworld")
    test (p07, "hellohelloworld")
    test (p07, "hellohelloworldworld")
    test (p07, "helloworldhelloworld")
    println ("=" * 80)

    val digitsAsText : Parser[String] = P (CharIn ('0' to '9').rep (1).!)
      
      //use .map (s=>s.toInt) to make char into int
    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt) 

    val twoDigits : Parser[Int] = P (CharIn ('0' to '9').rep (exactly=2).!).map (s => s.toInt)

    val month : Parser[String] = P (("Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" | "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec").!)
    val day : Parser[Int] = digits
      
      //did not do capture for " ", just read it. so Parser[(String,Int)] just for month and day
    val date : Parser[(String,Int)] = month ~ " " ~ day
    val time : Parser[(Int,Int,Int)] = twoDigits ~ ":" ~ twoDigits ~ ":" ~ twoDigits
    val hostname : Parser[String] = P (CharIn ('a' to 'z').rep (1).!)
    val process : Parser[(String,Int)] = P ("sshd".! ~ "[" ~ digits ~ "]")
    val message : Parser[String] = P (AnyChar.rep.!)
    val app01 : Parser[(String,Int,(Int,Int,Int),String,(String,Int),String)] = P (date ~ " " ~ time ~ " " ~ hostname ~ " " ~ process ~ ":" ~ message ~ End)
      
    test (app01, "Jan 29 07:07:02 reed sshd[15279]: User root from 95-24-31-141.broadband.corbina.ru not allowed because not listed in AllowUsers")
    println ("=" * 80)
  }
}