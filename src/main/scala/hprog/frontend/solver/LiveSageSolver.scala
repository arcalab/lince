package hprog.frontend.solver

import hprog.ast.SageExpr.{SExpr, SExprFun}
import hprog.ast._
import hprog.backend.Show
import hprog.common.ParserException
import hprog.frontend.Semantics.SageSolution
import hprog.frontend.Utils

import scala.sys.process._


class LiveSageSolver(path:String) extends StaticSageSolver {

  protected val lockRcv = new Object
  protected val lockSnd = new Object
  protected var last: Option[String] = None
  protected val proc = LiveSageSolver.createSageProcess(path,addUpdate)

  // wait for process to be created (and first output out)
  lockRcv.synchronized{
    //print("(rcv) waiting")
    if (last==None) // if I'm the first, then wait
      lockRcv.wait(10000)
    //debug(()=>" - done(r)")
  }
  // allow sender to continue after startup msg
  lockSnd.synchronized{
    //print("(rcv) unlocking sender")
    last = None
    lockSnd.notify()
    //debug(()=>" - done(r)")
  }

  def addUpdate(s:String): Unit = {
    //    last = Some(s)
    lockRcv.synchronized{
      debug(()=>s"(sender) sage reply: '$s'")
      last = Some(s)
      lockRcv.notify()
      //debug(()=>" - done(s)")
      //    lockRcv.synchronized(lockRcv.notifyAll())
    }
    lockSnd.synchronized{
      //print(s"(sender) waiting for permission")
      if (last!=None) // if I'm the first, then wait
        lockSnd.wait(10000)
      //debug(()=>s" - done(s)")
    }
    //debug(()=>s"(sender) continuing")
    //    l.notify() // null
  }

  // still unexpectedly locking...
  def close(): Int = {
    last = None
    var r = proc._2() // will trigger outputs
    lockRcv.synchronized{
      //print("(rcv) waiting")
      if (last==None)
        lockRcv.wait(5000)
      //debug(()=>" - done(r)")
    }
    // allow sender to continue after startup msg
    lockSnd.synchronized{
      //print("(rcv) unlocking sender")
      last = None
      lockSnd.notify()
      //debug(()=>" - done(r)")
    }
    r
  }

  def closeWithoutWait(): Unit = proc._3()

  def askSage(s:String): Option[String] = {
    last = None // clear last answer
    proc._1(s) // put string in input queue of Sage process
    waitForReply()
  }

  private def waitForReply(): Option[String] = {
    var prev = ""
    var ok = false
    while (!ok) {
    // wait for the notification (new value returned)
      lockRcv.synchronized{
        if (last == None) // I'm the first - wait
          lockRcv.wait(5000)
        //debug(()=>s"> '${last}'")
      }
      last match {
        case Some("'ok'") =>
          //debug(()=>s"moving prev '$prev' to last")
          last = Some(prev)
          ok = true
          lockSnd.synchronized{
            last = None
            lockSnd.notify()
          }
        case Some(v) =>
          //debug(()=>s"adding $v to prev")
          prev = v.drop(6)
          lockSnd.synchronized{
            last = None
            lockSnd.notify()
          }
        case None =>
          return None
      }
    }
    //debug(()=>s"returning '${prev}' if '$ok'")
    if (ok) Some(prev) else None
  }


  ///
  def askSage(eqs: List[DiffEq]): Option[String] = {
    val instructions = LiveSageSolver.genSage(eqs) // + "; print(\"§\")"
//    debug(()=>s"instructions to solve: '$instructions'")
    debug(()=>s"solving: ${Show(eqs)}")
    val rep = askSage(instructions)
    debug(()=>s"reply: '$rep'")
    rep
  }

  def askSage[E<:SExprFun](expr: E): Option[String] = {
    val vars = getVars(expr)
    val instructions = //s"print(\"${Show(expr)}\"); \"ok\""
      vars.map(v=>s"$v = var('$v'); ").mkString +
      "print(" + Show(expr) + "); \"ok\""
    debug(()=>s"expression to solve: '$instructions'")
    val rep = askSage(instructions)
    debug(()=>s"reply: '$rep'")
    rep
  }

  private def getVars(expr: SExprFun): List[String] = expr match {
    case SVal(v)       => Nil
    case _:SArg        => Nil
    case SVar(v)       => List(v)
    case SFun(f, args) => args.flatMap(getVars) //if (args == List(SVal(0)))
    case SDiv(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SMult(e1, e2) => getVars(e1) ++ getVars(e2)
    case SPow(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SAdd(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SSub(e1, e2)  => getVars(e1) ++ getVars(e2)
  }




  /**
    * precompute a system of equations using a system call to Sage
    * @param eqs
    */
  override def +=(eqs:List[DiffEq]): Unit =
    if (!cache.contains(eqs)) {
      askSage(eqs) match {
        case Some(reply) => importDiffEqs(List(eqs), List(reply))
        case None =>
          throw new ParserException(s"Timed out when solving ${
            eqs.map(Show(_)).mkString(", ")}.")

      }//SageSolver.callSageSolver(eqs, path)
    }

  override def +=(expr: SExpr): Unit =
    if (!cacheVal.contains(expr)) {
      askSage(expr) match {
        case Some(reply) => importExpr(expr,reply)
        case None =>
          throw new ParserException(s"Timed out when reducing ${
            Show(expr)}.")
      }
    }

  //      val instr = SageSolver.genSage(eqs)
  //      cache += (eqs -> (s"$path/sage -c $instr".!!))
  //      println(s"- adding ${eqs.map(Show(_)).mkString(",")} -> ${cache(eqs)}")



  //  /**
//    * Precompute and cache several system of equations with a single system call to Sage
//    * @param systems systems of equations to be precomupted
//    */
//  override def ++=(systems: List[List[DiffEq]]): Unit = {
//    val filtered = systems.filterNot(cache.contains)
//    if (filtered.nonEmpty) {
//      val reply = SageSolver.callSageSolver(filtered,path)
//      addToCache(filtered,reply)

//      val instructions = filtered.map(SageSolver.genSage).mkString("; print(\"§\"); ")
//      val results = s"$path/sage -c $instructions".!!
//      val parsed = results.split('§')
//      for ((eqs, res) <- filtered.zip(reply)) {
//        //println(s"- adding  ${eqs} -> $res")
//        val resParsed = SageParser.parse(res) match {
//          case SageParser.Success(SolVars(s,sol), _) if s.isEmpty && sol.keySet == Set("") =>
//            val vars = Solver.getVars(eqs).filterNot(_.startsWith("_"))
//            vars match {
//              case List(variable) => Map(variable -> sol(""))
//              case _ => throw new ParserException(s"Failed to parse $res - " +
//                s"only one variable expected, but found ${vars.mkString(",")}.")
//            }
//          case SageParser.Success(result, _) => result.sol
//          case _: SageParser.NoSuccess => throw new ParserException(s"Failed to parse $res")
//        }
//        cache += eqs -> resParsed
//      }
//    }
//  }


//  /** Gets a solution of a system of equations, using system calls to Sage,
//    * checking first in its cache.
//    * @param eqs System of equations to be retrived
//    * @return Result from Sage
//    */
//  def evalFun(eqs:List[DiffEq]): Solution = {
//    solveSymb(eqs)
//      .mapValues(expr => t => x => Eval(expr,t,x))
//  }


//  private def cached: Map[List[DiffEq],SageSolution] = cache - Nil


}


///////////////////////////////////////
////////// Static functions ///////////
///////////////////////////////////////
object LiveSageSolver {


  /**
    * Experimental: generating code to be sent to Sage
    * @param eqs
    * @return
    */
  def genSage(eqs:List[DiffEq]): String = {
    var res = "_t_ = var('_t_'); "
    val undefinedVars = Utils.getUsedVars(eqs) -- Utils.getDefVars(eqs)
    val eqs2 = eqs ::: undefinedVars.map(v => DiffEq(Var(v),Value(0))).toList

    for (e <- eqs2)
      res += s"${e.v.v} = function('${e.v.v}')(_t_); "
    for ((e,i) <- eqs2.zipWithIndex)
      res += s"_de${i}_ = diff(${e.v.v},_t_) == ${Show(e.e)}; "
    res += s"print(expand(desolve_system([${(for(i<-eqs2.indices)yield s"_de${i}_").mkString(",")}]," +
      s"[${eqs2.map(_.v.v).mkString(",")}])))"
    res += ";\"ok\""
    res
  }


  // DEPRECATED - when calling all in one go. Now calling one eqs at a time
  def callSageSolver(systems: List[List[DiffEq]], path: String, timeout:Int = 10): List[String] = {
    if (systems.filter(_.nonEmpty).nonEmpty) {
      //println(s"solving Sage with ${systems}")
      val instructions = systems.map(LiveSageSolver.genSage).mkString("; print(\"§\"); ")

      println(s"instructions to solve: ${instructions}")

      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val status = s"timeout $timeout $path/sage -c $instructions" !
                   ProcessLogger(stdout append _, stderr append _)
      if (status == 0)
        stdout.split('§').toList
      else
        throw new LiveSageSolver.SolvingException(stderr.toString)
    }
    else
      Nil
  }

  //// running sage the 2nd time
  private def genSage(c:Cond): String = c match {
    case BVal(b) => b.toString
    case And(c1, c2) => s"(${genSage(c1)} & ${genSage(c2)})"
    case Or(c1, c2) => s"(${genSage(c1)} | ${genSage(c2)})"
    case Not(c) =>s"not(${genSage(c)})"
    case EQ(v, l) => s"(__${v.v} == ${genSage(l)})"
    case GT(v, l) => s"(__${v.v} >  ${genSage(l)})"
    case LT(v, l) => s"(__${v.v} <  ${genSage(l)})"
    case GE(v, l) => s"(__${v.v} >= ${genSage(l)})"
    case LE(v, l) => s"(__${v.v} <= ${genSage(l)})"
  }
  private def genSage(lin: Lin): String = lin match {
    case Var(v) => v
    case Value(v) => v.toString
    case Add(l1, l2) => s"(${genSage(l1)} + ${genSage(l2)}"
    case Mult(v, l) => s"(${v.v} * ${genSage(l)}"
  }
  private def genSage(e: SExprFun): String = e match {
    case SVal(v) => v.toString
    case _:SArg => "_t_"
    case SVar(v) => "__"+v
    case SFun(f,args) => s"$f(${args.map(genSage).mkString(",")})"
    case SDiv(e1, e2) => s"(${genSage(e1)} / ${genSage(e2)})"
    case SMult(e1, e2) =>s"(${genSage(e1)} * ${genSage(e2)})"
    case SPow(e1, e2) => s"(${genSage(e1)} ^ ${genSage(e2)})"
    case SAdd(e1, e2) => s"(${genSage(e1)} + ${genSage(e2)})"
    case SSub(e1, e2) => s"(${genSage(e1)} - ${genSage(e2)})"
  }
  private def genSage(sol:SageSolution): String =
    sol.map(kv => s"__${kv._1} = ${genSage(kv._2)};").mkString
  def genSage(c:Cond,s:SageSolution): String =
    s"${genSage(s)} print(${genSage(c)})"



  def createSageProcess(path:String, callback: String => Unit):
  (String=>Unit, ()=>Int, ()=>Unit) = {
    var writer: java.io.PrintWriter = null

    // limit scope of any temporary variables
    // locally {
    val sage = s"$path/sage"
    // strings are implicitly converted to ProcessBuilder
    // via scala.sys.process.ProcessImplicits.stringToProcess(_)
    val io = new ProcessIO(
      // Handle subprocess's stdin
      // (which we write via an OutputStream)
      in => {
        writer = new java.io.PrintWriter(in)
        //debug(()=>"in stream created")
        // later do writer.println(..); writer.flush; writer.close()
      },
      // Handle subprocess's stdout
      // (which we read via an InputStream)
      out => {
        val src = scala.io.Source.fromInputStream(out)
        //debug(()=>"out stream created")
        for (line <- src.getLines()) {
          //debug(()=>"calling callback")
          callback(line)
          //debug(()=>"Answer: " + line)
        }
        src.close()
      },
      // We don't want to use stderr, so just close it.
      _.close()
//      err => {
//        val src = scala.io.Source.fromInputStream(err)
//        debug(()=>"err stream created")
//        for (line <- src.getLines()) {
//          debug(()=>"error: "+line)
//        }
//        src.close()
//      }
    )
    //println("/ calling Sage")
    val calcProc = sage.run(io)

    // Using ProcessBuilder.run() will automatically launch
    // a new thread for the input/output routines passed to ProcessIO.
    // We just need to wait for it to finish.

    def put(value:String): Unit = {
      writer.println(value)
      writer.flush()
    }
    def finished(): Int = {
      //println("\\_closing Sage")
      writer.close()
      val code = calcProc.exitValue()
      //debug(()=>s"Subprocess exited with code $code.")
      code
    }
    def finished2(): Unit = {
      //println("\\_closing Sage")
      writer.close()
    }

    (s=>put(s),()=>finished(),()=>finished2())
  }



  class SolvingException(s:String) extends RuntimeException(s)
}



