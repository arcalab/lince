package hprog.common

class TimeoutException(s:String) extends RuntimeException(s"Stopped waiting for Sage. This timeout is normal in the first execution of Lince, because the associated systems need to be started. Please try again. $s")
