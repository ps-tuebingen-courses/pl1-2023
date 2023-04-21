/**
Homework 02
============
Note: For some tasks, test examples are already provided.
Be sure to provide tests for all tasks and check your solution with them.
From now on, the tasks will not explicitly require tests any more,
but I advise you to nevertheless use tests for all programming tasks.

Deadline: Monday, May 01, 10:00h
*/

/**
Task 1: Visitors (1 subtask)
------
*/

object Hw02Task1 {
/**
Consider the definition for the count visitor and the print visitor for AE from the lecture
(see https://ps-tuebingen-courses.github.io/pl1-lecture-notes/03-arithmetic-expressions/arithmetic-expressions.html).
*/
case class Visitor[T](num: Int => T, add: (T, T) => T)

enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
import Exp._

// Fold using visitors
def foldExp[T](v: Visitor[T], e: Exp): T =
  e match {
    case Num(n)    => v.num(n)
    case Add(l, r) => v.add(foldExp(v, l), foldExp(v, r))
  }

val countVisitor = Visitor[Int]( _=>1, _+_)
val printVisitor = Visitor[String](_.toString, "("+_+"+"+_+")")

/**
Subtasks:

      1) Translate `countVisitor` and `printVisitor` to a definition using pattern matching.
      Example: Translating the `eval` visitor in this way leads to the `eval` method for object AE from the lecture.
      (https://ps-tuebingen-courses.github.io/pl1-lecture-notes/04-desugaring/desugaring.html)
*/
}


/**
Task 2: Desugaring to Nand (1 subtask)
------
*/

import scala.language.implicitConversions

object Hw02Task2 {
/**
Consider again the language of propositional logic formulae from the previous homework:
*/
enum Exp:
  case  True()  // constant true
  case  False() // constant false
  case  And(lhs: Exp, rhs: Exp)
  case  Or(lhs: Exp, rhs: Exp)
  case  Not(e: Exp)
  case  Impl(lhs: Exp, rhs: Exp)
import Exp._

def eval(e: Exp) : Boolean = e match {
  case True()     => true
  case False()    => false
  case _          => sys.error("not yet implemented")
}

/**
Subtasks:

      1) Introduce a new kind of expression `Nand` (not both ... and ...).
      Eliminate `And`, `Or`, `Not`, and `Impl` by defining them as syntactic sugar for `Nand`.
*/
}
