package hprog.frontend

import hprog.ast.Cond
import hprog.frontend.CommonTypes.Point

trait Deviator {
  def closest(p:Point, cond:Cond): Option[Point]
}

object Deviator {
  def dummy: Deviator =
    (p: Point, cond: Cond) => None
}

