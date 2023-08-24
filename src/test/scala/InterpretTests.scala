/*
 * This file is part of COMP3000 assignment 1.
 *
 * Copyright (C) 2023 Kym Haines, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Uses the ScalaTest `FlatSpec` style for writing tests. See
 *
 *      http://www.scalatest.org/user_guide
 *
 * For more info on writing ScalaTest tests.
 */

package org.mq.interpret

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class TokensTests extends FlatSpec with Matchers {

  import Interpret._

  "PARSING: pattern match" should "handle simple list" in {
    assert(matchPat("(a b c)") == List("(", "a", "b", "c", ")"))
  }

  it should "handle numbers" in {
    assert(matchPat("(14 25 -7 834 9)") ==
                          List("(", "14", "25", "-7", "834", "9", ")"))
  }

  it should "handle arithmetic functions" in {
    assert(matchPat("(+ 3 (* 4 5))") ==
                          List("(", "+", "3", "(", "*", "4", "5", ")", ")"))
  }

  it should "handle reader characters" in {
    assert(matchPat(" ' # ") == List("'", "#"))
  }

  "quoting" should "handle a symbol" in {
    assert(quoting(List("'", "a")) == List("(", "quote", "a", ")"))
  }

  it should "handle a list" in {
    assert(quoting(List("'", "(", "a", "b", "c", ")")) ==
                     List("(", "quote", "(", "a", "b", "c", ")", ")"))
  }

  "hashFning" should "handle a simple anonymous function" in {
    assert(hashFning(List("#", "(", "+", "%", "1", ")")) == 
           List("(", "fn", "[", "arg", "]", "(", "+", "arg", "1", ")", ")"))
  }

  "strToCObj" should "handle a name" in {
    assert(strToCObj("joe43") == CSymbol("joe43"))
  }

  it should "handle a number" in {
    assert(strToCObj("2021") == CNumber(2021))
  }

  it should "handle a negative number" in {
    assert(strToCObj("-53") == CNumber(-53))
  }

  it should "handle a function symbol: +" in {
    assert(strToCObj("+") == CSymbol("+"))
  }

  "tokensToCObj" should "handle a single name" in {
    assert(tokensToCObj(List("abc")) == Some(CSymbol("abc")))
  }

  it should "handle a single number" in {
    assert(tokensToCObj(List("42")) == Some(CNumber(42)))
  }

  it should "handle an empty list" in {
    assert(tokensToCObj(List("(", ")")) == Some(CSymbol("nil")))
  }

  it should "handle a simple list" in {
    assert(tokensToCObj(List("(", "a", "b", "g", ")")) ==
            Some(CList(CSymbol("a"),
                       CList(CSymbol("b"),
                             CList(CSymbol("g"),
                                   CSymbol("nil"))))))
  }

  it should "handle a more-complex list" in {
    assert(tokensToCObj(List("(", "a", "(", "b", ")", "g", ")")) ==
            Some(CList(CSymbol("a"),
                       CList(CList(CSymbol("b"), CSymbol("nil")),
                             CList(CSymbol("g"),
                                   CSymbol("nil"))))))
  }

  "FUNCTIONS: add" should "add two numbers" in {
    assert(add(CNumber(4), CNumber(3)) == CNumber(7))
  }

  it should "detect non-numbers" in {
    assert(add(CNumber(4), CSymbol("a")) == CSymbol("ERROR"))
  }

  "sub" should "subtract two numbers" in {
    assert(sub(CNumber(4), CNumber(3)) == CNumber(1))
  }

  "mul" should "multiply two numbers" in {
    assert(mul(CNumber(4), CNumber(3)) == CNumber(12))
  }

  "div" should "divide two numbers" in {
    assert(div(CNumber(12), CNumber(3)) == CNumber(4))
  }

  "inc" should "increment a number" in {
    assert(inc(CNumber(7)) == CNumber(8))
  }

  "dec" should "decrement a number" in {
    assert(dec(CNumber(7)) == CNumber(6))
  }

  "first" should "get the head of a list" in {
    // first of (a b g) is a
    assert(first(CList(CSymbol("a"),
                     CList(CSymbol("b"),
                           CList(CSymbol("g"),
                                 CSymbol("nil"))))) == CSymbol("a"))
  }

  "rest" should "get the tail of a list" in {
    // rest of (a b g) is (b g)
    assert(rest(CList(CSymbol("a"),
                     CList(CSymbol("b"),
                           CList(CSymbol("g"),
                                 CSymbol("nil")))))
               == CList(CSymbol("b"),
                        CList(CSymbol("g"),
                              CSymbol("nil"))))
  }

  it should "do first and rest for vector" in {
    // first of rest of ['a 'b 'c]
    assert(first(rest(CVector(Vector(CSymbol("a"), CSymbol("b"), CSymbol("c")))
                             )) == CSymbol("b"))
  }

  "cons" should "add the first arg to the front of the second arg list" in {
    // cons of a and (b c) is (a b c)
    assert(cons(CSymbol("a"), CList(CSymbol("b"),
                                    CList(CSymbol("c"),CSymbol("nil"))))
           == CList(CSymbol("a"),
                    CList(CSymbol("b"),CList(CSymbol("c"),CSymbol("nil")))))
  }

  "nth" should "get the second element of a list" in {
    // nth of (a b c) and 1
    assert(nth(CList(CSymbol("a"), CList(CSymbol("b"),
                                    CList(CSymbol("c"),CSymbol("nil")))),
               CNumber(1))
           == CSymbol("b"))
  }

  it should "get the second element of a vector" in {
    // nth of [4 5 6] and 1
    assert(nth(CVector(Vector(CNumber(4), CNumber(5), CNumber(6))),
               CNumber(1))
           == CNumber(5))
  }

  "nilQ" should "check for nil" in {
    assert(nilQ(CSymbol("nil")) == CSymbol("true"))
  }

  "zeroQ" should "check for zero" in {
    assert(zeroQ(CNumber(5)) == CSymbol("false"))
  }

  "oddQ" should "check for odd" in {
    assert(oddQ(CNumber(5)) == CSymbol("true"))
  }

  "evenQ" should "check for even" in {
    assert(evenQ(CNumber(5)) == CSymbol("false"))
  }

  "cequal" should "give true if equal" in {
    assert(cequal(CSymbol("a"), CSymbol("a")) == CSymbol("true"))
  }

  it should "give false if not equal" in {
    assert(cequal(CSymbol("a"), CSymbol("b")) == CSymbol("false"))
  }

  "cif" should "do 2nd arg if first arg is true" in {
    // if of true, (+ 3 4), (- 5 2) is 7
    assert(cif(CSymbol("true"),
                CList(CSymbol("+"),
                      CList(CNumber(3), CList(CNumber(4), CSymbol("nil")))),
                CList(CSymbol("-"),
                      CList(CNumber(5), CList(CNumber(2), CSymbol("nil")))))
           == CNumber(7))
  }

  it should "do 3rd arg if first arg is false" in {
    // if of false, (+ 3 4), (- 5 2) is 3
    assert(cif(CSymbol("false"),
                CList(CSymbol("+"),
                      CList(CNumber(3), CList(CNumber(4), CSymbol("nil")))),
                CList(CSymbol("-"),
                      CList(CNumber(5), CList(CNumber(2), CSymbol("nil")))))
           == CNumber(3))
  }

  "anonymous functions" should "be called with anonFunCall" in {
             // (#(+ % 2) 5) which becomes: ((fn [arg] (+ arg 2)) 5)
    assert(anonFunCall(CList(CSymbol("fn"),
                            CList(CVector(Vector(CSymbol("arg"))),
                              CList(CList(CSymbol("+"),
                                CList(CSymbol("arg"),CList(CNumber(2),
                                  CSymbol("nil")))),CSymbol("nil")))),
                       CNumber(5))
              == CNumber(7))
  }

  "user functions" should "be called with userFunCall" in {
             // (#(+ % 2) 5) which becomes: ((fn [arg] (+ arg 2)) 5)
    assert({
             resetEnv
             // (defn add2 [n] (+ 2 n))
             defn("add2", Vector(CSymbol("n")),
                  CList(CSymbol("+"),CList(CNumber(2),CList(CSymbol("n"),
                    CSymbol("nil")))))
             // (add2 6)
             userFunCall("add2", CNumber(6))
           } == CNumber(8))
  }

  "EVALUATING: arithmetic" should "handle +: (+ 3 4)" in {
    assert(eval(CList(CSymbol("+"),
                      CList(CNumber(3),CList(CNumber(4),CSymbol("nil")))))
           == CNumber(7))
  }

  it should "handle -: (- (+ 7 2) (- 4 1))" in {
    assert(eval(CList(CSymbol("-"),
                      CList(CList(CSymbol("+"),
                                  CList(CNumber(7),
                                        CList(CNumber(2),CSymbol("nil")))),
                            CList(CList(CSymbol("-"),
                                        CList(CNumber(4),
                                              CList(CNumber(1),
                                                    CSymbol("nil")))),
                                  CSymbol("nil")))))
           == CNumber(6))
  }

  it should "handle complex *,/: (* (first (rest (quote (u 4 v)))) (/ (/ 27 3) 3))" in {
    assert(eval(CList(CSymbol("*"),
                      CList(CList(CSymbol("first"),
                                  CList(CList(CSymbol("rest"),
                                              CList(CList(CSymbol("quote"),
                                                CList(CList(CSymbol("u"),
                                                  CList(CNumber(4),
                                                    CList(CSymbol("v"),
                                                      CSymbol("nil")))),
                                                CSymbol("nil"))),
                                            CSymbol("nil"))),
                                        CSymbol("nil"))),
                                    CList(CList(CSymbol("/"),
                                      CList(CList(CSymbol("/"),
                                        CList(CNumber(27),
                                          CList(CNumber(3),
                                            CSymbol("nil")))),
                                      CList(CNumber(3),CSymbol("nil")))),
                                  CSymbol("nil")))))
           == CNumber(12))
  }

  it should "handle inc: (inc 99)" in {
    assert(eval(CList(CSymbol("inc"),CList(CNumber(99),CSymbol("nil"))))
           == CNumber(100))
  }

  it should "handle dec: (dec 1000)" in {
    assert(eval(CList(CSymbol("dec"),CList(CNumber(1000),CSymbol("nil"))))
           == CNumber(999))
  }

  "list functions" should "handle first: (first (quote (a b c)))" in {
    assert(eval(CList(CSymbol("first"),
                      CList(CList(CSymbol("quote"),
                                  CList(CList(CSymbol("a"),
                                              CList(CSymbol("b"),
                                                    CList(CSymbol("c"),
                                                          CSymbol("nil")))),
                                        CSymbol("nil"))),
                            CSymbol("nil"))))
           == CSymbol("a"))
  }

  it should "handle rest: (rest (quote (a b c)))" in {
    assert(eval(CList(CSymbol("rest"),
                      CList(CList(CSymbol("quote"),
                                  CList(CList(CSymbol("a"),
                                              CList(CSymbol("b"),
                                                    CList(CSymbol("c"),
                                                          CSymbol("nil")))),
                                        CSymbol("nil"))),
                            CSymbol("nil"))))
           == CList(CSymbol("b"),CList(CSymbol("c"),CSymbol("nil"))))
  }

  it should "handle cons: (cons (quote a) (quote (b c)))" in {
    assert(eval(CList(CSymbol("cons"),
                      CList(CList(CSymbol("quote"),
                                  CList(CSymbol("a"),
                                        CSymbol("nil"))),
                            CList(CList(CSymbol("quote"),
                                        CList(CList(CSymbol("b"),
                                                    CList(CSymbol("c"),
                                                          CSymbol("nil"))),
                                              CSymbol("nil"))),
                                  CSymbol("nil")))))
           == CList(CSymbol("a"),
                    CList(CSymbol("b"),
                          CList(CSymbol("c"),
                                CSymbol("nil")))))
  }

  it should "handle nth: (nth [3 4 5 6 7] 2)" in {
    assert(eval(CList(CSymbol("nth"),
                      CList(CVector(Vector(CNumber(3), CNumber(4),
                                        CNumber(5), CNumber(6), CNumber(7))),
                            CList(CNumber(2),CSymbol("nil")))))
           == CNumber(5))
  }

  it should "handle arithmetic in a vector: [3 (+ 5 4) 8]" in {
    assert(eval(CVector(Vector(CNumber(3),
                               CList(CSymbol("+"),
                                     CList(CNumber(5),
                                           CList(CNumber(4),CSymbol("nil")))),
                               CNumber(8))))
           == CVector(Vector(CNumber(3), CNumber(9), CNumber(8))))
  }

  "conditions" should "handle = for false condition: (= 5 8)" in {
    assert(eval(CList(CSymbol("="),
                      CList(CNumber(5),
                            CList(CNumber(8), CSymbol("nil")))))
           == CSymbol("false"))
  }

  it should "handle = for true condition: (= (quote a) (quote a))" in {
    assert(eval(CList(CSymbol("="),
                      CList(CList(CSymbol("quote"),
                                  CList(CSymbol("a"),CSymbol("nil"))),
                            CList(CList(CSymbol("quote"),
                                        CList(CSymbol("a"),CSymbol("nil"))),
                                  CSymbol("nil")))))
           == CSymbol("true"))
  }

  it should "handle nil?: (nil? '(a b))" in {
    assert(eval(CList(CSymbol("nil?"),
                      CList(CList(CSymbol("quote"),
                                  CList(CList(CSymbol("a"),
                                              CList(CSymbol("b"),
                                                    CSymbol("nil"))),
                            CSymbol("nil"))),CSymbol("nil"))))
           == CSymbol("false"))
  }

  it should "handle zero?: (zero? -4)" in {
    assert(eval(CList(CSymbol("zero?"),CList(CNumber(-4),CSymbol("nil"))))
           == CSymbol("false"))
  }

  it should "handle odd?: (odd? 34)" in {
    assert(eval(CList(CSymbol("odd?"),CList(CNumber(34),CSymbol("nil"))))
           == CSymbol("false"))
  }

  it should "handle even?: (even? 34)" in {
    assert(eval(CList(CSymbol("even?"),CList(CNumber(34),CSymbol("nil"))))
           == CSymbol("true"))
  }

  it should "handle if when condition false: (if false 3 4)" in {
    assert(eval(CList(CSymbol("if"),
                      CList(CSymbol("false"),
                            CList(CNumber(3),
                                  CList(CNumber(4),CSymbol("nil"))))))
           == CNumber(4))
  }

  it should "handle if when condition true: (if true 3 4)" in {
    assert(eval(CList(CSymbol("if"),
                      CList(CSymbol("true"),
                            CList(CNumber(3),
                                  CList(CNumber(4),CSymbol("nil"))))))
           == CNumber(3))
  }

  it should "handle if with expressions: (if (= 3 3) (+ 2 3) (* 4 5))" in {
    assert(eval(CList(CSymbol("if"),
                      CList(CList(CSymbol("="),
                                  CList(CNumber(3),
                                        CList(CNumber(3),CSymbol("nil")))),
                            CList(CList(CSymbol("+"),
                                        CList(CNumber(2),
                                              CList(CNumber(3),
                                                    CSymbol("nil")))),
                                  CList(CList(CSymbol("*"),
                                              CList(CNumber(4),
                                                    CList(CNumber(5),
                                                          CSymbol("nil")))),
                                        CSymbol("nil"))))))
           == CNumber(5))
  }

  "variables" should "assign to a variable" in {
    assert({
             resetEnv
             // (def x 12)
             eval(CList(CSymbol("def"),
                        CList(CSymbol("x"),
                              CList(CNumber(12),CSymbol("nil")))))
             // x
             eval(CSymbol("x"))
           }   == CNumber(12))
  }

  it should "use a variable in an expression" in {
    assert({
             resetEnv
             // (def s (quote (g h k)))
             eval(CList(CSymbol("def"),
                        CList(CSymbol("s"),
                              CList(CList(CSymbol("quote"),
                                          CList(CList(CSymbol("g"),
                                            CList(CSymbol("h"),
                                              CList(CSymbol("k"),
                                                CSymbol("nil")))),
                                          CSymbol("nil"))),
                                    CSymbol("nil")))))
             // (first s)
             eval(CList(CSymbol("first"),CList(CSymbol("s"),CSymbol("nil"))))
           }   == CSymbol("g"))
  }

  "user functions" should "define and use a function" in {
    assert({
             resetEnv
             // (defn foo[y](+ y 2))
             eval(CList(CSymbol("defn"),
                        CList(CSymbol("foo"),
                              CList(CVector(Vector(CSymbol("y"))),
                                    CList(CList(CSymbol("+"),
                                                CList(CSymbol("y"),
                                                      CList(CNumber(2),
                                                            CSymbol("nil")))),
                                          CSymbol("nil"))))))
             // (foo 4)
             eval(CList(CSymbol("foo"),CList(CNumber(4),CSymbol("nil"))))
           }   == CNumber(6))
  }

  "recursive user functions" should "define and use a recursive function" in {
    assert({
             resetEnv
             // (defn sum[y](if (= y 0) 0 (+ y (sum (- y 1)))))
             eval(CList(CSymbol("defn"),
                        CList(CSymbol("sum"),
                              CList(CVector(Vector(CSymbol("y"))),
                                CList(CList(CSymbol("if"),
                                  CList(CList(CSymbol("="),
                                    CList(CSymbol("y"),CList(CNumber(0),
                                      CSymbol("nil")))),CList(CNumber(0),
                                  CList(CList(CSymbol("+"),CList(CSymbol("y"),
                                    CList(CList(CSymbol("sum"),
                                      CList(CList(CSymbol("-"),
                                        CList(CSymbol("y"),CList(CNumber(1),
                                          CSymbol("nil")))),
                                    CSymbol("nil"))),
                                CSymbol("nil")))),
                          CSymbol("nil"))))),
                    CSymbol("nil"))))))
             // (sum 4)
             eval(CList(CSymbol("sum"),CList(CNumber(4),CSymbol("nil"))))
           }   == CNumber(10))
  }

  "functional programming" should "handle built-in function as data" in {
    // ((nth [+ *] 0) 4 7)
    assert(eval(CList(CList(CSymbol("nth"),
                            CList(CVector(Vector(CSymbol("+"), CSymbol("*"))),
                                  CList(CNumber(0),
                                        CSymbol("nil")))),
                      CList(CNumber(4),CList(CNumber(7),CSymbol("nil")))))
           == CNumber(11))
  }

  it should "handle anonymous function as data" in {
    // ((nth [+ #(* 5 %)] 1) 4)
    assert(eval(CList(CList(CSymbol("nth"),
                            CList(CVector(Vector(CSymbol("+"),
                                                 CList(CSymbol("fn"),
                                                   CList(CVector(Vector(
                                                     CSymbol("arg"))),
                                                 CList(CList(CSymbol("*"),
                                                   CList(CNumber(5),
                                                     CList(CSymbol("arg"),
                                                       CSymbol("nil")))),
                                                CSymbol("nil")))))),
                                  CList(CNumber(1),CSymbol("nil")))),
                        CList(CNumber(4),CSymbol("nil"))))
           == CNumber(20))
  }

  it should "handle map: (map inc [3 5 8])" in {
    assert(eval(CList(CSymbol("map"),
                      CList(CSymbol("inc"),
                            CList(CVector(Vector(CNumber(3), CNumber(5),
                                                 CNumber(8))),
                                  CSymbol("nil")))))
           == CList(CNumber(4),
                    CList(CNumber(6),CList(CNumber(9),CSymbol("nil")))))
  }

  it should "handle filter: (filter odd? '(3 4 5 6))" in {
    assert(eval(CList(CSymbol("filter"),
                      CList(CSymbol("odd?"),
                            CList(CList(CSymbol("quote"),
                                      CList(CList(CNumber(3),
                                         CList(CNumber(4),CList(CNumber(5),
                                           CList(CNumber(6),CSymbol("nil"))))),
                                  CSymbol("nil"))),
                              CSymbol("nil")))))
           == CList(CNumber(3),CList(CNumber(5),CSymbol("nil"))))
  }

  "eval" should "work: (eval (quote (+ 12 45)))" in {
    assert(eval(CList(CSymbol("eval"),
                      CList(CList(CSymbol("quote"),
                                  CList(CList(CSymbol("+"),
                                              CList(CNumber(12),
                                                    CList(CNumber(45),
                                                          CSymbol("nil")))),
                                        CSymbol("nil"))),
                            CSymbol("nil"))))
           == CNumber(57))
  }

  ///////////////////////////////////////////////////////////////////
  //
  // TO DO     put your test cases below
  //
  ///////////////////////////////////////////////////////////////////

}
