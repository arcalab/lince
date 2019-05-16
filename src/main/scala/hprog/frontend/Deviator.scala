package hprog.frontend

import hprog.ast.Cond
import hprog.frontend.Deviator.Point
import hprog.frontend.Semantics.Valuation

trait Deviator {
  def closest(p:Point, cond:Cond): Option[Point]
}

object Deviator {
  type Point = Valuation

  def dummy: Deviator =
    (p: Point, cond: Cond) => None
}

