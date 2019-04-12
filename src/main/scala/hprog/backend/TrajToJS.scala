package hprog.backend

import hprog.frontend.Semantics.Valuation
import hprog.frontend.Traj

object TrajToJS {

  def apply(traj:Traj[Valuation],range:Option[(Double,Double)]=None): String = {

    val max: Double = traj.dur.getOrElse(10)
    //      val x0 = traj(0)
    var colorID: Map[String,Int] =
      traj(0).keys.zipWithIndex.toMap
    // traces are mappings from variables t0 lists of "y" values
    //      var traces =  (x0.keys zip List[List[Double]]())
    //        .toMap.withDefaultValue(List())
    var traces = Map[String,Map[Double,Option[Double]]]().withDefaultValue(Map())
    var boundaries =  Map[String,Map[Either[Double,Double],(Double,String)]]().withDefaultValue(Map())
    //      var ends   = Map[String,List[(Double,Double)]]()
    //(traj.vars zip List[List[Double]]()).toMap.withDefaultValue(List())
    //      println(s"c - max=$max")
    /////
    // Generate sampling values for time - 100 values from 0 to max
    val (start,end) = range match {
      case Some(x) => x
      case None => if (max<=0) (0.0,0.0) else (0.0,max)
    }
    val samples = if ((end-start)<=0) List(start) else start to end by ((end-start) / 100)


    for (t: Double <- samples)
      for ((variable, value) <- traj(t))
        traces += variable -> (traces(variable) + (t->Some(value)))
    //      println("d")
    ////
    // Add starting/ending points with notes when available
    for ((t,valt) <- traj.ends if t>=start && t<=end; (variable,value) <- valt) {
      val oldNote = boundaries(variable).getOrElse[(Double,String)](Right(t),(0,""))._2
      boundaries += variable -> (boundaries(variable) + (Right(t) ->(value, oldNote)))
      traces += variable -> (traces(variable) + (t->None))
    }
    for ((t,valt) <- traj.inits if t>=start && t<=end; (variable,value) <- valt) {
      val oldNote = boundaries(variable).getOrElse[(Double,String)](Left(t),(0,""))._2
      val newNote = traj.notes.get(t) match {
        case Some(str) if str.nonEmpty => str
        case _                         => oldNote
      }
      boundaries += variable -> (boundaries(variable) + (Left(t) ->(value, newNote)))
//      boundaries += variable -> ((t, value,txt) :: boundaries(variable))
      traces += variable -> (traces(variable) + (t->None))
    }
    /////
    /// Build the JavaScript code to generate graph
    var js = "var colors = Plotly.d3.scale.category10();\n"

    //      val rangeTxt = "x: "+samples.mkString("[",",","]")
    //      println("e")
    for ((variable, values) <- traces) {
      val (xs,ys) = values.toList.sorted.unzip
      js +=
        s"""var t_${variable} = {
           |   x: ${xs.mkString("[",",","]")},
           |   y: ${ys.map(_.map(_.toString()).getOrElse("null"))
          .mkString("[", ",", "]")},
           |   mode: 'lines',
           |   line: {color: colors(${colorID.getOrElse(variable,0)})},
           |   legendgroup: 'g_${variable}',
           |   name: '$variable'
           |};
             """.stripMargin
    }
    for ((variable, values) <- boundaries) {
//      val sortedVals = values.sorted
      val (ins,outs) = values.toList.partition(pair=>pair._1.isLeft)
      js += mkMarkers(variable,"out",outs,
              s"""{color: 'rgb(255, 255, 255)',
                 | size: 10,
                 | line: {
                 |   color: colors(${colorID.getOrElse(variable, 0)}),
                 |   width: 2}}""".stripMargin)
      js += mkMarkers(variable,"in",ins,
              s"""{color: colors(${colorID.getOrElse(variable, 0)}),
                 | size: 10,
                 | line: {
                 |   color: colors(${colorID.getOrElse(variable, 0)}),
                 |   width: 2}}""".stripMargin)
    }
    val traceNames = traces.keys.map("t_"+_).toList ++
                     boundaries.keys.map("b_out_"+_).toList ++
                     boundaries.keys.map("b_in_"+_).toList


    js += s"var data = ${traceNames.mkString("[",",","]")};" +
      s"\nvar layout = {hovermode:'closest'};" +
      s"\nPlotly.newPlot('graphic', data, layout, {showSendToCloud: true});"

    js
  }


  private def mkMarkers(variable:String,
                        inout:String,
                        data:List[(Either[Double,Double],(Double,String))],
                        style: String): String =
    s"""var b_${inout}_$variable = {
       |   x: ${data.map(_._1.fold(x=>x,x=>x)).mkString("[", ",", "]")},
       |   y: ${data.map(_._2._1).mkString("[", ",", "]")},
       |   text: ${data.map("'"+_._2._2.replaceAll("\\\\","\\\\\\\\")+"'").mkString("[",",","]")},
       |   mode: 'markers',
       |   marker: $style,
       |   type: 'scatter',
       |   legendgroup: 'g_$variable',
       |   name: 'boundary of $variable',
       |   showlegend: false
       |};""".stripMargin

//  marker: {color: colors(${colorID.getOrElse(variable, 0)})},

}
