package hprog.common

class TimeOutOfBoundsException(max:String, tried:String) extends RuntimeException(
  s"Reached the end of trajectory at $max when searching for $tried.")
