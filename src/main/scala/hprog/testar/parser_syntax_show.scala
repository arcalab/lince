package hprog.testar

import hprog.ast._
import Syntax._

import hprog.lang.Parser
import Parser._


import hprog.backend.Show
import Show._



object parser_syntax_show {

	def testing_parser(s:String):Syntax={ //ParseResult[Syntax]
       var parser_p=parse(s)
       return parser_p.get

       /*
       var parser_p=parse(s).get
       //var parser_s=(parser_p).toString
       println("Parsing:"+ parser_p)
       return parser_p

       //var show_s=apply(parser_s.asInstanceOf[Syntax])
       //println("Show:"+show_s)
       */
	}

/*
	def testing_show(s:Syntax):String={
		var show_s=apply(s)
		println("Showing:"+show_s)
		return show_s
	}
    
    // serve para verificar se o parsing está bem feito
	def testing(s:String): Unit={
		var p1=testing_parser(s)
		var s1=testing_show(p1)
		var p2=testing_parser(s1)
		var s2=testing_show(p2)

		if (s1==s2){
			println("Sucefull")
		} else {
			println("Not Sucefull")
		}
	}

*/
}
