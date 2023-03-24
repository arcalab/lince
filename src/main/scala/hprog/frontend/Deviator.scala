package hprog.frontend

import hprog.ast._
import Syntax._
import hprog.frontend.CommonTypes.Point

trait Deviator {
  def closest(p:Point, cond:Cond): Option[Point]
}

object Deviator {
  def dummy: Deviator =
    (p: Point, cond: Cond) => None
}

