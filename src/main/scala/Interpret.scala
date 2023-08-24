/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2023 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

// https://www.scala-lang.org/api/2.12.8/scala/util/matching/Regex.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/List.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/StringLike.html
// https://www.scala-lang.org/api/2.12.8/scala/collection/immutable/Map.html

package org.mq.interpret

import scala.util.matching._

object Interpret {

  sealed abstract class CObject
  case class CSymbol(sym:String) extends CObject
  case class CNumber(num:Int) extends CObject
  case class CList(head:CObject, tail:CObject) extends CObject
  case class CVector(vec:Vector[CObject]) extends CObject
  case class CBuiltInFun(name:String) extends CObject
  case class CUserFun(name:String) extends CObject
  case class CAnonFun(fnList:CList) extends CObject

  val nil = CSymbol("nil")
  val cTrue = CSymbol("true")
  val cFalse = CSymbol("false")
  val error = CSymbol("ERROR")

  val builtInFuns = List("+", "-", "*", "/", "=", "inc", "dec", "first",
                         "rest", "cons", "nth", "nil?", "zero?", "odd?",
                         "even?", "map", "filter", "eval")

  val pat = "TO DO: recognise all the language tokens".r

  def quoting(a:List[String]):List[String] =
  {
    // TO DO: convert any ' something to (quote something)
    a
  }

  def hashFning(a:List[String]):List[String] =
  {
    // TO DO: convert any #( ... ) to (fn ... )
    a
  }

  def matchPat(line:String):List[String] =
                                  pat.findAllIn(line.toLowerCase).toList

  def reader(line:String):List[String] = hashFning(quoting(matchPat(line)))

  // excludes brackets
  def strToCObj(s:String):CObject =
  {
    // TO DO: convert a string token to an CObject; or error
    error
  }

  def tokensToCObj(a:List[String]):Option[CObject] = 
    // TO DO: convert a list of token strings to an CObject
    // NOTE: anywhere () is seen, it should be interpreted as nil
  {
    None
  }

  // for testing if convenient
  def lineToCObj(line:String):CObject = tokensToCObj(reader(line)) match
  {
  case Some(s) => s
  case None    => error
  }

  // var variables = ???  // hold user-defined variable names and their values
  // var functions = ???  // hold user-defined functions

  def resetEnv:Unit =
  {
    // TO DO: reset (i.e. remove) all variable and user function bindings
  }

  def setValue(varName:String, value:CObject):Unit =
  {
    // TO DO: assign a value to a variable
    // check stack first
  }

  def getValue(varName:String):CObject =
  {
    // TO DO: get the value of a variable; or error if variable not defined
    // check stack first
    error
  }

  def add(a:CObject, b:CObject):CObject = 
  {
    // TO DO: add two numbers
    CNumber(0)
  }

  def sub(a:CObject, b:CObject):CObject =
  {
    // TO DO: subtract two numbers
    CNumber(0)
  }

  def mul(a:CObject, b:CObject):CObject =
  {
    // TO DO: multiply two numbers
    CNumber(0)
  }

  def div(a:CObject, b:CObject):CObject =
  {
    // TO DO: divide two numbers
    CNumber(0)
  }

  def inc(a:CObject):CObject =
  {
    // TO DO: increment a number
    CNumber(0)
  }

  def dec(a:CObject):CObject =
  {
    // TO DO: decrement number
    CNumber(0)
  }

  def first(a:CObject):CObject =
  {
    // TO DO: get the first element of a list or vector
    error
  }

  def rest(a:CObject):CObject =
  {
    // TO DO: get the rest element of a list or vector
    error
  }

  def cons(a:CObject, b:CObject):CObject =
  {
    // TO DO: adds an element at the start of a list or vector
    error
  }

  def nth(a:CObject, b:CObject):CObject =
  {
    // TO DO: get the n-th element of a list or vector
    error
  }

  def cequal(a:CObject, b:CObject):CObject =
  {
    // TO DO: compare two objects
    error
  }

  def nilQ(a:CObject):CObject =
  {
    // TO DO: determines whether an object is nil
    error
  }

  def zeroQ(a:CObject):CObject =
  {
    // TO DO: determines whether an object is zero
    error
  }

  def oddQ(a:CObject):CObject =
  {
    // TO DO: determines whether a number is odd
    error
  }

  def evenQ(a:CObject):CObject =
  {
    // TO DO: determines whether a number is even
    error
  }

  def defVar(v:String, b:CObject):CObject =
  {// assigns a value to a variable
    setValue(v, b)
    b
  }

  def cif(cond:CObject, ifThen:CObject, ifElse:CObject):CObject =
  {
    // TO DO: if ... then ... else ...
    error
  }

  def defn(name:String, args:Vector[CObject], body:CObject) =
  {
    // TO DO: define a user function
    // the function definition source should look like:
    //      (defn name [args] body)
    // somehow store the args and body associated with the function name
    CUserFun(name)
  }

  def userFunCall(name:String, arg:CObject):CObject =
  {
    // TO DO: evaluate user function name called with a parameter arg
    error
  }

  def anonFunCall(fun:CObject, arg:CObject):CObject =
  {
    // TO DO: evaluate function call with anonymous function
    // fun looks like (fn [...] ...)
    error
  }

  def cmap(fun:CObject, collection:CObject):CObject =
  {
    // TO DO: implement map
    error
  }

  def cfilter(fun:CObject, collection:CObject):CObject =
  {
    // TO DO: implement filter
    error
  }

  def eval(a:CObject):CObject = a match
  {
  case CSymbol("nil") => nil
  case CSymbol("true") => cTrue
  case CSymbol("false") => cFalse
  // TO DO: fill in all the other cases
  // Note: unless you are doing the distinction stuff with complicated
  // function calls, you don't need another match anywhere here - every
  // line in the big eval table in the spec can be done in the same order
  // here with a case for each line; see Scala Intro document for clues
  // for how to handle this
  case _           => error
  }

  def showLine(s:CObject):Unit = { show(s);  println() }

  def show(s:CObject):Unit = s match
  {
  case CList(h, t)    => print("(")
                         show(h)
                         showList(t)
                         print(")")
  case CSymbol(a)     => print(a)
  case CNumber(a)     => print(a)
  case CVector(v)     => showVector(v)
  case CBuiltInFun(f) => print("#object[Function " + f + "]")
  case CUserFun(f)    => print("#object[Function " + f + "]")
  case CAnonFun(f)    => print("#object[Function ")
                         show(f)
                         print("]")
  }

  def showList(s:CObject):Unit = s match
  {
  case CSymbol("nil") =>
  case a:CList => print(" ")
                  show(a.head)
                  a.tail match
                  {
                  case b:CList => showList(b)
                  case _ =>
                  }
  case _ => print(" . ")
            show(s)
  }

  def showVector(v:Vector[CObject]) = v match
  {
  case Vector()  => print("[]")
  case _         => print("[")
                    v.dropRight(1).foreach (a => { show(a); print(" ") })
                    show(v.last)
                    print("]")
  }
}
