package csp.ch03

// From SBT: ~run-main csp.ch03.Project

object Project {

   ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Abstract Syntax
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Expr
  case class CstI (n : Int)                           extends Expr
  case class Var (nm : String)                        extends Expr
  case class Prim (nm : String, e1 : Expr, e2 : Expr) extends Expr
  case class Call (nm : String, es : List[Expr])      extends Expr
  case class NewArray (sz : Expr)                                     extends Expr
  case class ReadElt (arr : Expr, idx : Expr)                         extends Expr
  case class WriteElt (arr : Expr, idx : Expr, e : Expr)              extends Expr

  
  sealed trait Stmt
  case class Asgn (nm : String, e : Expr)                                     extends Stmt
  // case class AsgnT (nm : String, e1 : Expr, e2 : Expr)                        extends Stmt
  case class If (e1 : Expr, s1 : Stmt, s2 : Stmt)                             extends Stmt
  case class For (nm : String, low : Expr, high : Expr, s : Stmt)             extends Stmt
  case class While (e : Expr, s : Stmt)                                       extends Stmt
  case class Block (ss : List[Stmt])                                          extends Stmt
  // case class blockForLoop (ss : List[Stmt])                                          extends Stmt
  case class Print (e : Expr)                                                 extends Stmt
  case class PrintString (arr : Expr)                                         extends Stmt
  case class Return (e : Expr)                                                extends Stmt
  // case class WithDoc (nm : String, ss : List[String])             extends Stmt
  // case class UseDoc (nm : String, ss : List[String])              extends Stmt

  // case class Params (nm : String, e1 : Expr, e2 : Expr, ss : List[Stmt]) extends Stmt


  case class FuncDef (nm : String, params : List[String], body : Stmt)

  case class Procedure (nm1: String, funs : List[FuncDef], main : Stmt)

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Parsing
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def foldAssocLeft (p : (Expr, List[(String,Expr)])) : Expr = {
    p match {
      case (e1, Nil) => e1
      case (e1, (op, e2) :: rest) => foldAssocLeft (Prim (op, e1, e2), rest)
    }
  }

  object MyParsersNoWhitespace {
    import fastparse.all._

    val digits : Parser[Int] = P (CharIn ('0' to '9').rep (1).!).map (s => s.toInt)

    val integer : Parser[Expr] = digits.map (n => CstI (n))

    val keywords : List[String] = List ("if","then","end if","while","loop","end loop","for","in","reverse","return","with","use","procedure","is","begin","end","function","Put_Line","Put_LineString", "newarray", "read", "write")

    val alpha : Parser[String] = P ((CharIn ('A' to 'Z') | CharIn ('a' to 'z') | CharIn ('_' to '_')).!)

    //val literal_string : Parser[String] = P( (CharIn(' ' to '!') | CharIn('#' to '~')).rep().!)

    val ident : Parser[String] = P ((alpha ~ (alpha | CharIn ('0' to '9')).rep (0)).!).filter (s => !keywords.contains (s))

    val header : Parser[List[String]] = P((ident.rep (1, sep = "_").map (s => s.toList)))

    val variable : Parser[Expr] = ident.map (s => Var (s))
  }

  object MyParsers {
    val White = fastparse.WhitespaceApi.Wrapper {
      import fastparse.all._
      NoTrace ((" "| "\n"| "\r" | "\t").rep)
    }

    import fastparse.noApi._
    import White._

    import MyParsersNoWhitespace._

   val atExpr : Parser[Expr] = P (
      integer | 
      (ident ~ ("(" ~ expr.rep (sep = ",").map (s => s.toList) ~ ")").?).map {
        case (nm, None)      => Var (nm)
        case (nm, Some (es)) => Call (nm, es)
      } | 
      ("newarray" ~ "(" ~/ expr ~ ")").map (e => NewArray (e)) |
      ("read" ~ "(" ~/ expr ~ "," ~ expr  ~ ")").map { case (arr, idx) => ReadElt (arr, idx) } |
      ("write" ~ "(" ~/ expr ~ "," ~ expr ~ "," ~ expr  ~ ")").map { case (arr, idx, e) => WriteElt (arr, idx, e) } |
      ("(" ~/ expr ~ ")")) 

    val typeParser: Parser[Expr] = P ( 
      (":" ~ multDiv)
    )

    val string: Parser[Expr] = P ( 
      (atExpr ~ (((" ").rep).! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )

    val multDiv : Parser[Expr] = P (
      (atExpr ~ (("*" | "/").! ~ atExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val addSub : Parser[Expr] = P (
      (multDiv ~ (("+" | "-" | "mod").! ~ multDiv).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val gtLtGeLeExpr : Parser[Expr] = P (
      (addSub ~ ((">" | "<" | ">=" | "<=").! ~ addSub).rep.map (s => s.toList)).map (foldAssocLeft)
    )
    val eqNeExpr : Parser[Expr] = P (
      (gtLtGeLeExpr ~ (("=" | "<>").! ~ gtLtGeLeExpr).rep.map (s => s.toList)).map (foldAssocLeft)
    )

    val expr : Parser[Expr] = P (eqNeExpr)

    val AssignStmt : Parser[Stmt] = P((ident ~ ":=" ~ expr ~ ";").map { case (nm, e) => Asgn (nm, e) } )

    // val AssignWithType : Parser[Stmt] = P((ident ~ typeParser ~ ":=" ~ expr ~ ";").map { case (nm, e1, e2) => AsgnT (nm, e1, e2) } )

    val IfStmt : Parser[Stmt] = P(("if" ~ expr ~ "then" ~ stmt ~ "else" ~ stmt ~ "end if;").map { case (e1, s1, s2) => If (e1, s1, s2) } )

    val ForStmt : Parser[Stmt] = P(("for" ~ ident ~ "in" ~ "reverse".rep ~ expr ~ ".." ~ expr ~ "loop" ~ stmt ~ "end loop;").map { case (nm, e1, e2, s) => For (nm, e1, e2, s) })

    val WhileStmt : Parser[Stmt] = P(("while" ~  expr ~ "loop" ~ stmt ~ "end loop;").map { case (e, s) => While (e, s) })

    val returnStmt : Parser[Stmt] = P(("return" ~  expr ~ ";").map { case (e) => Return (e) })

    val print : Parser[Stmt] = P(("Put_Line" ~ "(" ~ expr ~ ")" ~ ";").map { case (e) => Print (e) })

    val printString : Parser[Stmt] = P(("Put_LineString" ~ "(" ~ expr ~ ")" ~ ";").map { case (e) => PrintString (e) })

    // val params : Parser[Stmt] = P(("(" ~ ident ~ typeParser ~ ")" ~ "return" ~ atExpr ~ "is" ~ AssignStmt.rep).map{case(nm,e1,e2,ss) => Params(nm,e1,e2,ss.toList)})

    val block : Parser[Stmt] = P("begin" ~ (stmt.rep).map { case ss => Block (ss.toList) } ~ "end;")

    // val blockForLoop : Parser[Stmt] = P("loop" ~ (stmt.rep).map { case ss => Block (ss.toList) } ~ "end loop;")

    val stmt : Parser[Stmt] = P(AssignStmt | IfStmt | ForStmt | WhileStmt | returnStmt | print | printString | block )

    // val withDoc : Parser[Stmt] = P(("with" ~ ident ~ "." ~ header ~ ";").map{case (nm, ss) => WithDoc (nm, ss.toList)} )
    // val useDoc : Parser[Stmt] = P(("use" ~ ident ~ "." ~ header ~ ";").map{case (nm, ss) => UseDoc (nm, ss.toList)} )

    val funcdef : Parser[FuncDef] = P(("function" ~ ident ~ "(" ~ ident.rep (sep=",").map (s => s.toList) ~ ")" ~ stmt).map { case (nm, params, body) => FuncDef (nm, params, body) } )

    val procedure : Parser[Procedure] = P (
      ("procedure" ~ident ~ "is" ~ "begin" ~ funcdef.rep.map (s => s.toList) ~ "main" ~ stmt ~ "end;").map { case (nm, funcdefs, body) => Procedure (nm, funcdefs, body) })


    //val start : Parser[Stmt] = P (stmt ~ End)
    val start2 : Parser[Procedure] = P (procedure ~ End)
  

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //Pretty printing
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////

  def ppExpr (e : Expr) : String = {
    e match {
      case CstI (i)                     => i.toString
      case Var (x)                      => x
      case Prim (op, e1, e2)            => "(%s %s %s)".format (ppExpr (e1), op, ppExpr (e2))
      case Call (nm, es)                => "(%s (%s))".format (nm, es.map (ppExpr).mkString (", "))
      case NewArray (sz)                => "(newarray (%s))".format (ppExpr (sz))
      case ReadElt (arr, idx)           => "(read (%s, %s))".format (ppExpr (arr), ppExpr (idx))
      case WriteElt (arr, idx, e)       => "(write (%s, %s, %s))".format (ppExpr (arr), ppExpr (idx), ppExpr (e))
    }
  }

  def ppBlock (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Block (ss) => {
        val sb = new StringBuilder
        for (s <- ss) {
          sb.append (ppStmt (newIndent, s))
        }
        sb.toString
      }
      case _ => {
        "%s".format (ppStmt (newIndent, s))
      }
    }
  }

  def ppStmt (indent : String, s : Stmt) : String = {
    val newIndent = indent + "  "
    s match {
      case Asgn (nm, e)           => 
        "%s%s := %s;\n".format (indent, nm, ppExpr (e))
      case If (e, s1, s2)         => 
        "%sif (%s) then \n%s%s else \n%s%s end if;\n".format (indent, ppExpr (e), ppBlock (indent, s1), indent, ppBlock (indent, s2), indent)
      case Block (ss) => {
        "%sbegin \n%s%s end\n".format (indent, ppBlock (indent, s), indent)
      }
      case For (nm, low, high, s) => {
        "%sfor %s in reverse %s .. %s loop \n%s%s end loop;\n".format (indent, nm, ppExpr (low), ppExpr (high), ppBlock (indent, s), indent)
      }
      case While (e, s)           => 
        "%swhile (%s) loop \n%s%s end loop;\n".format (indent, ppExpr (e), ppBlock (indent, s), indent)
      case Print (e)              => 
        "%sPut_Line (%s);\n".format (indent, ppExpr (e))
      case PrintString (e)              => 
        "%sPut_LineString (%s);\n".format (indent, ppExpr (e))
      case Return (e)             => 
        "%sreturn (%s);\n".format (indent, ppExpr (e))
      // case params (nm, e1, e2, ss) => 
      //   "(%s%s%s) return %s is %s\n".format(ident,indent,ppExpr(e),ppExpr(e),List(Asgn))
    }
  }

  def ppFuncDef (f : FuncDef) : String = {
    "function %s (%s)\n%s".format (f.nm, f.params.mkString (", "), ppStmt ("", f.body))
  }

  def ppProcedure (p : Procedure) : String = {
    p.funs.map (f => ppFuncDef (f)).mkString ("\n") + ppStmt ("", p.main)
  }

 ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Code Generation
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  type Env = Map[String,String]
  type FuncEnv = Map[String,FuncDef]

  val emptyEnv : Env = Map.empty

  var labelCounter : Int = 0
  def newLabel () : String = {
    labelCounter = labelCounter + 1
    "lab%03d".format (labelCounter)
  }

  // Generate x86-64 assembly to evaluate e.
  // Result is at the top of the stack.
  // The following registers may be changed by the generated assembly language: %rax, %rbx, %rsp, %rip
  def compileExpr (e : Expr, env : Env, fenv : FuncEnv) : String = {
    e match {
      case CstI (i)           => 
        "\tpushq\t$%d\n".format (i)
      case Var (x)            => 
        env.get (x) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (x))
          case Some (lab) => 
            "\tpushq\t%s\n".format (lab)
        }
      case Prim (op, e1, e2) => {
        val insts1 = compileExpr (e1, env, fenv) 
        val insts2 = compileExpr (e2, env, fenv)
        val push = "\tpushq\t%rax\n"
        def pop (reg : String) = "\tpopq\t%%%s\n".format (reg)
        val instsOp : String = op match {
          case  "+" => "\taddq\t%rbx, %rax\n"
          case  "-" => "\tsubq\t%rbx, %rax\n"
          case  "*" => "\timulq\t%rbx, %rax\n"
          case  "/" => "\tdiv\t%rbx,%rax\n"
          case  "=" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets ZF if ((rax-rbx) = 0) as signed, i.e., (rax = rbx)
            "\tsete\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if ZF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          // case "<>" => b2i (i1 != i2) 
          case  "<" => {
            "\tcmpq\t%rbx, %rax\n" +    // sets SF if ((rax-rbx) < 0) as signed, i.e., (rax < rbx)
            "\tsets\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if SF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          case  ">" => {
           "\tcmpq\t%rax, %rbx\n" +    // sets SF if ((rax-rbx) > 0) as signed, i.e., (rax > rbx)
            "\tsets\t%al\n" +           // sets low-order byte (%al) of %rax to 1 if SF is set, otherwise to 0
            "\tmovzbl\t%al, %eax\n"     // extends %al to %rax (recall that assignment to a 32-bit register clears the upper 32-bits of the corresponding 64-bit register)
          }
          case "<=" => {
           "\tcmpq\t%rbx, %rax\n" +    // sets SF if ((rax-rbx) < 0) as signed, i.e., (rax < rbx)
            "\tsets\t%al\n" + 
            "\tsete\t%al\n" +
            "\tmovzbl\t%al, %eax\n"
          }
          case ">=" => {
            "\tcmpq\t%rax, %rbx\n" +    // sets SF if ((rax-rbx) < 0) as signed, i.e., (rax < rbx)
            "\tsets\t%al\n" + 
            "\tsete\t%al\n" +
            "\tmovzbl\t%al, %eax\n"
          }
          // case "mod" =>{
          //   "\tmovzbl\t$0,%dx\n" +
          //   "\tmovzbl\t%ax,%rax\n" +
          //   "\tmovzbl\t%bx,%rbx\n" +
          //   "\tdiv\t%bx\n" +
          //   "\tmovzbl\t%rbx,%dx\n"
          // }

          case   _ => throw new RuntimeException ("unknown primitive " + op)
        }
        insts1 +
        insts2 +
        pop ("rbx") +
        pop ("rax") +
        instsOp + 
        push
      }
      case Call (nm, es) => {
        es.reverse.map (e => compileExpr (e, env, fenv)).mkString +
        "\tcall\t%s\n".format (nm) + 
        "\taddq\t$%d, %%rsp\n".format (es.length * 8) +
        "\tpushq\t%rax\n"
      }
      case NewArray (sz) => {
        compileExpr (sz, env, fenv) +
        "\tpopq\t%rdi\n" +
        "\timulq\t$8, %rdi\n" +
        "\tcall\tmalloc\n" +
        "\tpushq\t%rax\n"
      }
      case ReadElt (arr, idx) => {
        compileExpr (arr, env, fenv) +
        compileExpr (idx, env, fenv) +
        "\tpopq\t%rbx\n" +
        "\tpopq\t%rax\n" +
        "\tmovq\t(%rax,%rbx,8), %rax\n" +
        "\tpushq\t%rax\n"
      }
      case WriteElt (arr, idx, e) => {
        compileExpr (arr, env, fenv) +
        compileExpr (idx, env, fenv) +
        compileExpr (e, env, fenv) +
        "\tpopq\t%rcx\n" +
        "\tpopq\t%rbx\n" +
        "\tpopq\t%rax\n" +
        "\tmovq\t%rcx, (%rax,%rbx,8)\n" +
        "\tpushq\t$0\n"
      }
    }
  }

  def compileAll (prog : Procedure, env : Env, fenv : FuncEnv) : String = {
    header () + 
    compileFunc (FuncDef ("main", Nil, prog.main), env, fenv) + 
    "\n" +
    prog.funs.map (fd => compileFunc (fd, env, fenv)).mkString ("\n") + 
    footer (env)
  }

  def header () : String = {
    ""
  }

  def print_string () : String = {
    "\t.text\n" +
    "\t.globl\tprint_string\n" +
    "\t.type\tprint_string, @function\n" +
    "print_string:\n" +
    ".LFB0:\n" +
    "\tpushq\t%rbp\n" +
    "\tmovq\t%rsp, %rbp\n" +
    "\tsubq\t$16, %rsp\n" +
    "\tmovq\t%rdi, -8(%rbp)\n" +
    "\tjmp\t.L2\n" +
    ".L3:\n" +
    "\tmovq\t-8(%rbp), %rax\n" +
    "\tmovq\t(%rax), %rax\n" +
    "\tmovsbl\t%al, %eax\n" +
    "\tmovl\t%eax, %edi\n" +
    "\tcall\tputchar\n" +
    "\taddq\t$8, -8(%rbp)\n" +
    ".L2:\n" +
    "\tmovq\t-8(%rbp), %rax\n" +
    "\tmovq\t(%rax), %rax\n" +
    "\ttestq\t%rax, %rax\n" +
    "\tjne\t.L3\n" +
    "\tleave\n" +
    "\tret\n"
  }
  
  def footer (env : Env) : String = {
    "\n" +
    print_string () +
    "\n" +
    "\t.section .rodata\n" + 
    ".output:\n" + 
    "\t.string \"%d\\n\"\n" +
    "\n" +
    (for ((nm1, _) <- env) yield {
      "\t.globl\t%s\n".format (nm1) +
      "\t.data\n".format (nm1) +
      "\t.align\t8\n" +
      "\t.size\t%s, 8\n".format (nm1) +
      "%s:\n".format (nm1) +
      "\t.quad\t0\n" +
      "\n"
    }).mkString
  }

  def compileFunc (func : FuncDef, env : Env, fenv : FuncEnv) : String = {
    val header = {
      "\t.text\n" +
      "\t.globl\t%s\n".format (func.nm) +
      "\t.type\t%s, @function\n".format (func.nm) +
      "%s:\n".format (func.nm) + 
      "\tpushq\t%rbp\n" + 
      "\tmovq\t%rsp, %rbp\n" 
    }
    val footer = {
      "\tpopq\t%rbp\n" + 
      "\tret\n"
    }
    var env2 : Env = env
    for ((param, i) <- func.params.zipWithIndex) {
      env2 = env2 + ( (param, "%d(%%rbp)".format ((i + 2) * 8)) ) 
    }
    header + 
    compileStmt (func.body, env2, fenv) + 
    footer
  }

  def compileStmt (s : Stmt, env : Env, fenv : FuncEnv) : String = {
    s match {
      case Asgn (nm, e)            => {
        env.get (nm) match {
          case None => throw new RuntimeException ("unable to find variable %s in environment".format (nm))
          case Some (lab) => 
            ppStmt ("// ", s) + 
            compileExpr (e, env, fenv) + 
            "\tpopq\t%rax\n" +
            "\tmovq\t%%rax, %s\n".format (lab)
        }
      }
      case If (e, s1, s2)          => 
        val label1 = newLabel ()
        val label2 = newLabel ()
        val label3 = newLabel ()
        "// %s\n".format (ppExpr (e)) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" + 
        "\ttestq\t%rax, %rax\n" + 
        "\tjne\t%s\n".format (label1) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s1, env, fenv) +
        "\tjmp\t%s\n".format (label3) +
        "%s:\n".format (label2) +
        compileStmt (s2, env, fenv) +
        "%s:\n".format (label3) 
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : String = {
          ss2 match {
            case Nil       => ""
            case s2 :: ss3 => compileStmt (s2, env, fenv) + loop (ss3)
          }
        }
        loop (ss)
      }
      case For (nm, low, high, s)  => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "// for (%s := %s to %s)\n".format (nm, ppExpr (low), ppExpr (high)) +
        compileExpr (low, env, fenv) + 
        "\tpopq\t%rax\n" + 
        "\tmovq\t%%rax, (%s)\n".format (nm) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "\tmovq\t(%s), %%rax\n".format (nm) +
        "\taddq\t$1, %rax\n" +
        "\tmovq\t%%rax, (%s)\n".format (nm) +
        "%s:\n".format (label2) +
        compileExpr (high, env, fenv) + 
        "\tpopq\t%rbx\n" + 
        "\tmovq\t(%s), %%rax\n".format (nm) +
        "\tcmpq\t%rbx, %rax\n" + 
        "\tjle\t%s\n".format (label1)
      }
      case While (e, s)            => {
        val label1 = newLabel ()
        val label2 = newLabel ()
        "// while (%s)\n".format (ppExpr (e)) +
        "\tjmp\t%s\n".format (label2) +
        "%s:\n".format (label1) +
        compileStmt (s, env, fenv) +
        "%s:\n".format (label2) +
        compileExpr (e, env, fenv) + 
        "\tpopq\t%rax\n" + 
        "\ttestq\t%rax, %rax\n" + 
        "\tjne\t%s\n".format (label1)
      }
      case Print (e)               => {
        ppStmt ("// ", s) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rsi\n" +
        "\tmovl\t$.output, %edi\n" + 
        "\tmovl\t$0, %eax\n" +
        "\tcall\tprintf\n"
      }
      case PrintString (e)               => {
        ppStmt ("// ", s) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rdi\n" +
        "\tcall\tprint_string\n"
      }
      case Return (e)               => {
        ppStmt ("// ", s) + 
        compileExpr (e, env, fenv) +
        "\tpopq\t%rax\n" +
        "\tpopq\t%rbp\n" + 
        "\tret\n"
      }
    }
  }

  def findVarsExpr (e : Expr) : List[String] = {
    e match {
      case CstI (i)               => Nil
      case Var (x)                => List (x)
      case Prim (op, e1, e2)      => findVarsExpr (e1) ::: findVarsExpr (e2)
      case Call (nm, es)          => es.flatMap (findVarsExpr)
      case NewArray (sz)          => findVarsExpr (sz)
      case ReadElt (arr, idx)     => findVarsExpr (arr) ::: findVarsExpr (idx) 
      case WriteElt (arr, idx, e) => findVarsExpr (arr) ::: findVarsExpr (idx) ::: findVarsExpr (e) 
    }
  }

  def findVarsStmt (s : Stmt) : List[String] = {
    s match {
      case Asgn (nm, e)            => nm :: findVarsExpr (e)
      case If (e, s1, s2)          => findVarsExpr (e) ::: findVarsStmt (s1) ::: findVarsStmt (s2)
      case Block (ss)              => {
        def loop (ss2 : List[Stmt]) : List[String] = {
          ss2 match {
            case Nil       => Nil
            case s2 :: ss3 => findVarsStmt (s2) ::: loop (ss3)
          }
        }
        loop (ss)
      }
      case For (nm, low, high, s)  => {
        nm :: findVarsExpr (low) ::: findVarsExpr (high) ::: findVarsStmt (s)
      }
      case While (e, s)            => {
        findVarsExpr (e) ::: findVarsStmt (s)
      }
      case Print (e)               => {
        findVarsExpr (e)
      }
      case PrintString (e)         => {
        findVarsExpr (e)
      }
      case Return (e)              => {
        findVarsExpr (e)
      }
    }
  }

  def findVars (s : Stmt) : List[String] = {
    findVarsStmt (s).toSet.toList.sortWith ((s1,s2) => s1 < s2)
  }


 ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Testing
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  import fastparse.all.{Parsed,Parser}



  def readFile (filename : String) : String = {
    val source : scala.io.BufferedSource = io.Source.fromFile (filename)
    try source.getLines.mkString ("\n") finally source.close ()
  }

  def invokeAssemblerLinker (asmFilename : String) : Unit = {
    import scala.sys.process.{Process}
    val pb = Process (List ("gcc", "-o", asmFilename.replace (".s", ""), asmFilename))
    import scala.language.postfixOps
    val result : String = (pb !!)
    println ("Running assembler: %s".format (result))
  }

  def compile (prog : Procedure, filename: String) : Unit = {
    val fenv : FuncEnv = (for (fd <- prog.funs) yield (fd.nm, fd)).toMap
    val vars : List[String] = for (stmt <- (prog.main :: prog.funs.map (f => f.body)); v <- findVars (stmt)) yield v
    val env : Env = (for (v <- vars) yield (v, "(%s)".format (v))).toMap
    println ("Variables: %s".format (env.mkString (", ")))
    println ("Compiling:")
    val asm : String = compileAll (prog, env, fenv)
    val asmFilename = filename.replace (".adb", ".s")
    val fw = new java.io.FileWriter (asmFilename)
    fw.write (asm)
    fw.close
    println ("Wrote to %s".format (asmFilename))
    invokeAssemblerLinker (asmFilename)
    // println (asm)
  }

  def test (p : Parser[Procedure], filename : String) : Unit = {
    val input : String = readFile (filename)
    val result : fastparse.core.Parsed[Procedure, Char, String] = p.parse (input) 
    result match {
      case Parsed.Success (prog, successIndex) => {
        println ("Successfully parsed file \"%s\".\nResult is %s.\nIndex is %d.".format (filename, prog, successIndex))
        // println ("Pretty printing:")
        // print (ppStmt ("  ", stmt))
         compile (prog, filename)
      }
      case Parsed.Failure (lastParser, index, extra) => {
        println ("Failed to parse file \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (filename, lastParser, index, extra))
      }
    }
  }

  // // Testing with out reading File.
  // def test (p : Parser[Procedure], s : String) : Unit = {
  //   println("length of string = " + s.length)

  //   val result : fastparse.core.Parsed[Procedure, Char, String] = p.parse (s) 
  //   result match {
  //     case Parsed.Success (stmt, successIndex) => {
  //       println ("Successfully parsed \"%s\".  Result is %s.  Index is %d.".format (s, stmt, successIndex))
  //     }
  //     case Parsed.Failure (lastParser, index, extra) => {
  //       println ("Failed to parse \"%s\".  Last parser is %s.  Index is %d.  Extra is %s".format (s, lastParser, index, extra))
  //     }
  //   }
  // }


  def main (args : Array[String]) {
    println ("=" * 80)

    import java.io.File
    for (f <- new File ("./input/project/printStrArr").listFiles.toList.sortWith ((f1, f2) => f1.getName < f2.getName);
         if (f.getName.endsWith (".adb"))) {
      test (MyParsers.start2, f.getPath)
      println ("=" * 80)
    }
  }

     // val p01 : Parser[Stmt] = MyParsers.start
     // val p02 : Parser[FuncDef] = MyParsers.funcdef
// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//   //                Hello World Test
//   ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
     // test(MyParsers.procedure, "procedure Hello is beginTop function Factorial(N: Integer) return Interger is Result := N; Counter := Counter + 1;begin for I in reverse 1..Counter loop Result := Result * I; end loop; return Result; end; begin i := 0; while i<10 loop Put_Line(Factorial(i)); i := i+1; end loop; end; endTop;")
     // test(MyParsers.print,"Put_Line(Factorial(i));")
     // test(MyParsers.block,"begin Put_Line(Factorial(i)); i := i+1; end;")
     // test(MyParsers.WhileStmt, "while i<10 loop begin Put_Line(Factorial(i)); i := i+1; end; end loop;")
     // test(MyParsers.block, "begin i := 0; while i<10 loop begin Put_Line(Factorial(i)); i := i+1; end; end loop; end;")
      // test(MyParsers.procedure, 
        // """procedure Hello is 
        //    beginTop 
        //        function Factorial(N)
        //        begin 
        //           if N>1 
        //           then return N * Factorial(N-1); 
        //           else return 1; 
        //           end if;
        //        end;
        //        block
        //        begin 
        //           i := 0; 
        //           while i<10 loop 
        //             begin 
        //               Put_Line(Factorial(i)); 
        //               i := i+1; 
        //             end; end loop; 
        //        end; 
        //    endTop;""")


  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
                 // Factorial Test
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////
    //test(p02,"function Factorial(N : Integer) return Integer is Result := N; Counter := Counter +1; begin for I in reverse 1..Counter loop Result := Result * I; end loop; return Result; end;")
      // test(MyParsers.params, "(N: Integer) return Interger is Result := N; Counter := Counter + 1;")
      // test(MyParsers.block,"begin for I in reverse 1..Counter loop Result := Result * I; end loop; return Result; end;")
    //   test(MyParsers.funcdef, "function Factorial(N: Integer) return Interger is Result := N; Counter := Counter + 1;begin for I in reverse 1..Counter loop Result := Result * I; end loop; return Result; end;")
    // test(MyParsers.funcdef,"function Factorial(N)begin for I in reverse 1..Counter loop Result := Result * I; end loop;  return Result; end;")
    // test(MyParsers.IfStmt, "if N>1 then return N * Factorial(N-1); else return 1; end if;")
    // test(MyParsers.funcdef,"function Factorial(N) begin if N>1 then Result := N * Factorial(N-1); end if; return Result; else return 1 end;")
      

    // println ("=" * 80)


  //}
}