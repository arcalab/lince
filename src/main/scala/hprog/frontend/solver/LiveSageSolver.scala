package hprog.frontend.solver

import hprog.ast.SymbolicExpr.{SyExpr, SyExprAll}
import hprog.ast._
import Syntax._
import hprog.backend.Show
import hprog.common.{ParserException, TimeoutException}
import hprog.frontend.CommonTypes.{SySolution, Valuation}
import hprog.frontend.Utils

import scala.sys.process._

/**
  * Runs a Sage process in the background and interacts with it
  * to solve expressions symbolically.
  *
  * @param path Folder where the 'sage' binary can be found.
  */
class LiveSageSolver(path:String) extends StaticSageSolver {

  protected val lockRcv = new Object
  protected val lockSnd = new Object
  protected var last: Option[String] = None
  debug(()=>s" - Sage process: creating (last=$last)")
  protected val proc = LiveSageSolver.createSageProcess(path,addUpdate)

  // wait for process to be created (and first output out)
  var count = 10
  while (last!=Some("sage: started") && count>0) { // if not started yet, then wait
    lockRcv.synchronized{
      debug(()=>s"Initial run: waiting to start (last=$last, count=$count)")
      lockRcv.wait(3000) // tempo de espera para o sage coemeçar
      count -= 1
    }
    lockSnd.synchronized {
      //last=None
      lockSnd.notify() // in case it is waiting to send more stuff.
      lockSnd.notify() // in case it is waiting to send more stuff.
    }
  }
  debug(()=>s" - Sage process: should be created (count=$count)")
  // allow sender to continue after startup msg
  lockSnd.synchronized{
    debug(()=>"(lince) unlocking (sage)")
    last = None
    lockSnd.notify()
    //debug(()=>" - done(r)")
  }

  def addUpdate(s:String): Unit = {
    //    last = Some(s)
    lockRcv.synchronized{
      debug(()=>s"(sage) sage reply: '$s'")
      last = Some(s)
      lockRcv.notify()
      //debug(()=>" - done(s)")
      //    lockRcv.synchronized(lockRcv.notifyAll())
    }
    lockSnd.synchronized{
      debug(()=>"(sage) waiting up to 20s for (lince) to ack my reply")
      if (last!=None) // if I'm the first, then wait
        lockSnd.wait(20000)
      debug(()=>"(sage) done")
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
      debug(()=>"(lince) waiting up to 5s for last msg before closing")
      if (last==None)
        lockRcv.wait(5000)
      debug(()=>"(lince) done")
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

  def closeWithoutWait(): Unit = {
    proc._3()
    lockSnd.synchronized{
      last = None
      lockSnd.notify()
    }

  }

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
        debug(()=>"(lince) waiting up to 40s for reply")
        if (last == None) // I'm the first - wait
          lockRcv.wait(40000)
        debug(()=>"(lince) done")
        //debug(()=>s"> '${last}'")
      }
      last match {
        case Some("'ok'") =>
          //debug(()=>s"moving prev '$prev' to last")
          last = Some(prev)
          ok = true
          lockSnd.synchronized{
            debug(()=>"(lince) notifying (sender)")
            last = None
            lockSnd.notify()
          }
        case Some(v) =>
          //debug(()=>s"adding $v to prev")
          prev = v.drop(6)
          lockSnd.synchronized{
            debug(()=>"(lince) notifying (sender)")
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
    val instructions = genSage(eqs) // + "; print(\"§\")"
//    debug(()=>s"instructions to solve: '$instructions'")
    debug(()=>s"solving: ${Show(eqs)}")
    val rep = askSage(instructions)
    debug(()=>s"reply: '$rep'")
    rep
  }

  /**
    * Generate code to be sent to Sage
    * @param eqs
    * @return
    */
  private def genSage(eqs:List[DiffEq]): String = {
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


  def askSage[E<:SyExprAll](expr: E): Option[String] = {
    val vars = getVars(expr)
    val instructions = //s"print(\"${Show(expr)}\"); \"ok\""
      vars.map(v=>s"$v = var('$v'); ").mkString +
      "print(" + Show(expr) + "); \"ok\""
    debug(()=>s"expression to solve: '$instructions'")
//    println(s"expression to solve: '$instructions'")
    val rep = askSage(instructions)
    debug(()=>s"reply: '$rep'")
    rep
  }

  def askSage(c:Cond,vl:Valuation): Option[String] = {
    //val vars = getVars(c)
    val instructions =
      "bool(" + Show(c,vl) + "); \"ok\""
//      "x = var('x'); "+
//        "solve([" + Show(c,vl) + " , x==1],x) != []; \"ok\""
    debug(()=>s"expression to solve: '$instructions'")
    val rep = askSage(instructions)
    debug(()=>s"reply: '$rep'")
    rep

  }

  private def getVars(expr: SyExprAll): List[String] = expr match {
    case SVal(_)       => Nil
    case SArg()        => Nil
    case s:SVar        => List(s.v)
    case SFun(_, args) => args.flatMap(getVars) //if (args == List(SVal(0)))
    case SDiv(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SRes(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SMult(e1, e2) => getVars(e1) ++ getVars(e2)
    case SPow(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SAdd(e1, e2)  => getVars(e1) ++ getVars(e2)
    case SSub(e1, e2)  => getVars(e1) ++ getVars(e2)
  }
//  private def getVars(c: Cond): List[String] = c match {
//    case BVal(b) => Nil
//    case And(c1, c2) => getVars(c1) ++ getVars(c2)
//    case Or(c1, c2)  => getVars(c1) ++ getVars(c2)
//    case Not(c2)     => getVars(c2)
//    case EQ(l1, l2) => getVars(l1) ++ getVars(l2)
//    case GT(l1, l2) => getVars(l1) ++ getVars(l2)
//    case LT(l1, l2) => getVars(l1) ++ getVars(l2)
//    case GE(l1, l2) => getVars(l1) ++ getVars(l2)
//    case LE(l1, l2) => getVars(l1) ++ getVars(l2)
//  }
//
//  private def getVars(lin: Lin): List[String] = lin match {
//    case Var(v)      => List(v)
//    case Value(v)    => Nil
//    case Add(l1, l2) => getVars(l1) ++ getVars(l2)
//    case Mult(v, l)  => getVars(l)
//  }




  /**
    * precompute a system of equations using a system call to Sage
    * @param eqs
    */
  override def +=(eqs:List[DiffEq]): Unit =
    if (!cacheDE.contains(eqs)) {
      askSage(eqs) match {
        case Some(reply) => importDiffEqs(List(eqs), List(reply))
        case None =>
          throw new TimeoutException(s"Timed out while solving ${
            eqs.map(Show(_)).mkString(", ")}.")

      }//SageSolver.callSageSolver(eqs, path)
    }

  override def +=(expr: SyExprAll): Unit =
    if (!cacheVal.contains(expr)) {
      askSage(expr) match {
        case Some(reply) => importExpr(expr,reply)
        case None =>
          throw new TimeoutException(s"Timed out while reducing ${
            Show(expr)}.")
      }
    }

  override def +=(cond:Cond, valua:Valuation): Unit = {
    if (!cacheBool.contains(cond,valua)) {
      askSage(cond,valua) match {
        case Some(reply) => importBool(cond, valua, reply)
        case None =>
          throw new TimeoutException(s"Timed out while solving condition ${
            Show(cond,valua)
          }.")
      }
    }
  }

}


///////////////////////////////////////
////////// Static functions ///////////
///////////////////////////////////////
object LiveSageSolver {




  def createSageProcess(path:String, callback: String => Unit):
  (String=>Unit, ()=>Int, ()=>Unit) = {
    var writer: java.io.PrintWriter = null

    // limit scope of any temporary variables
    // locally {
    //val sage = s"$path/sage"
    val sage = "\"C:\\Users\\Ricardo Correia\\AppData\\Local\\SageMath 9.3\\runtime\\bin\\mintty.exe\" -t 'SageMath 9.3 Console' -i sagemath.ico /bin/bash --login -c '/opt/sagemath-9.3/sage'"
    // strings are implicitly converted to ProcessBuilder
    // via scala.sys.process.ProcessImplicits.stringToProcess(_)
    val io = new ProcessIO(
      // Handle subprocess's stdin
      // (which we write via an OutputStream)
      in => {
        writer = new java.io.PrintWriter(in)
        writer.println("print('started')")
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

    // Artificially add waiting time for Sage to be ready to receive its first input.
    //Thread.sleep(2000)
    //put("print('started')")
    writer.flush()

    // return: how to make requests, how to ask to end (with and without waiting for confirmation)
    (s=>put(s),()=>finished(),()=>finished2())
  }


  class SolvingException(s:String) extends RuntimeException(s)


  // DEPRECATED - when calling all in one go. Now calling one eqs at a time
  //  def callSageSolver(systems: List[List[DiffEq]], path: String, timeout:Int = 10): List[String] = {
  //    if (systems.filter(_.nonEmpty).nonEmpty) {
  //      //println(s"solving Sage with ${systems}")
  //      val instructions = systems.map(LiveSageSolver.genSage).mkString("; print(\"§\"); ")
  //
  //      println(s"instructions to solve: ${instructions}")
  //
  //      val stdout = new StringBuilder
  //      val stderr = new StringBuilder
  //      val status = s"timeout $timeout $path/sage -c $instructions" !
  //                   ProcessLogger(stdout append _, stderr append _)
  //      if (status == 0)
  //        stdout.split('§').toList
  //      else
  //        throw new LiveSageSolver.SolvingException(stderr.toString)
  //    }
  //    else
  //      Nil
  //  }

  //// running sage the 2nd time
  //  private def genSage(c:Cond): String = c match {
  //    case BVal(b) => b.toString
  //    case And(c1, c2) => s"(${genSage(c1)} & ${genSage(c2)})"
  //    case Or(c1, c2) => s"(${genSage(c1)} | ${genSage(c2)})"
  //    case Not(c) =>s"not(${genSage(c)})"
  //    case EQ(l1, l2) => s"(__${genSage(l1)} == ${genSage(l2)})"
  //    case GT(l1, l2) => s"(__${genSage(l1)} >  ${genSage(l2)})"
  //    case LT(l1, l2) => s"(__${genSage(l1)} <  ${genSage(l2)})"
  //    case GE(l1, l2) => s"(__${genSage(l1)} >= ${genSage(l2)})"
  //    case LE(l1, l2) => s"(__${genSage(l1)} <= ${genSage(l2)})"
  //  }
  //  private def genSage(lin: Lin): String = lin match {
  //    case Var(v) => v
  //    case Value(v) => v.toString
  //    case Add(l1, l2) => s"(${genSage(l1)} + ${genSage(l2)}"
  //    case Mult(v, l) => s"(${v.v} * ${genSage(l)}"
  //  }
  //  private def genSage(e: SyExprAll): String = e match {
  //    case SVal(v) => v.toString
  //    case _:SArg => "_t_"
  //    case s:SVar => "__"+s.v
  //    case SFun(f,args) => s"$f(${args.map(genSage).mkString(",")})"
  //    case SDiv(e1, e2) => s"(${genSage(e1)} / ${genSage(e2)})"
  //    case SMult(e1, e2) =>s"(${genSage(e1)} * ${genSage(e2)})"
  //    case SPow(e1, e2) => s"(${genSage(e1)} ^ ${genSage(e2)})"
  //    case SAdd(e1, e2) => s"(${genSage(e1)} + ${genSage(e2)})"
  //    case SSub(e1, e2) => s"(${genSage(e1)} - ${genSage(e2)})"
  //  }
  //  private def genSage(sol:SySolution): String =
  //    sol.map(kv => s"__${kv._1} = ${genSage(kv._2)};").mkString
  //  def genSage(c:Cond,s:SySolution): String =
  //    s"${genSage(s)} print(${genSage(c)})"


}



