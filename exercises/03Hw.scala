/**
Homework 03
============
Deadline: Monday, May 08, 10:00h
*/

import scala.language.implicitConversions

/**
Task 1: Binding constructs (2 subtasks, plus 1 optional subtask)
------
*/
object Hw03Task1 {
/**
Consider the language of arithmetic expressions with `With`,
(see https://ps-tuebingen-courses.github.io/pl1-lecture-notes/05-name-binding/name-binding.html)
extended with two new binding constructs as illustrated by the following abstract syntax:
*/
enum Exp:
  case Num(n: Int)
  case Add(lhs: Exp, rhs: Exp)
  case Mul(lhs: Exp, rhs: Exp)
  case Id(x: String)
  case With(x: String, xdef: Exp, body: Exp)
  /** New binding constructs `Let` and `LetStar` */
  case Let(defs: List[(String, Exp)], body: Exp)
  case LetStar(defs: List[(String, Exp)], body: Exp)

object Exp:
  /** We use implicits again to make example programs less verbose. */
  implicit def num2exp(n: Int): Exp = Num(n)
  implicit def sym2exp(x: String): Exp = Id(x)

import Exp._

/**
Your task is to extend the language with the following new binding
construct `Let` (for `LetStar` see below).
*/
/**
Note: The names "Let" and "LetStar" have been choosen in analogy to the
"let" and "let*" binding constructs in Scheme and Racket.
*/

/**
The purpose of the `Let` construct is to bind a list of identifiers in such a way
that the scope of the bound variables is only the body, but not any of the right-hand
sides of definitions. In particular, there is no shadowing between the definitions.
For instance, the following test case should evaluate to 7 and not to 11:
*/

val test1 =
  With("x", 1,
   Let(List("x" -> 5, "y" -> Add("x", 1)), Add("x", "y")))

/**
Subtasks:

      1) Implement the missing part of the `eval` and `subst` function
      to support `Let`. Only change the parts currently filled with an error!

      2) There is some redundancy in the binding constructs of this
      language. Eliminate the construct `With` by defining it as
      syntactic sugar.

      3) Optional third exercise: See below.
*/

def subst(e: Exp, i: String, v : Num): Exp = e match {
  case Num(n) => e
  case Id(x) => if (x == i) v else e
  case Add(l, r) => Add(subst(l, i, v), subst(r, i, v))
  case Mul(l, r) => Mul(subst(l, i, v), subst(r, i, v))
  case With(x, xdef, body) => With(x,
                                subst(xdef, i, v),
                                if (x == i) body else subst(body, i, v))
  case Let(defs, body) =>
    val replBody = sys.error("not yet implemented")
    val replDefs = defs map {
      //This is an anonymous function that decomposes the argument pair into `x` and `xDef`.
      case (x, xDef) => (x, subst(xDef, i, v))
    }
    Let(replDefs, replBody)
  case LetStar(defs, body) =>
    val replBody = sys.error("not yet implemented")
    val replDefs = sys.error("not yet implemented")
    LetStar(replDefs, replBody)
}

def eval(e: Exp): Int = e match {
  case Num(n) => n
  case Id(x) => sys.error("unbound variable: " + x)
  case Add(l, r) => eval(l) + eval(r)
  case Mul(l, r) => eval(l) * eval(r)
  case With(x, xdef, body) => eval(subst(body, x, Num(eval(xdef))))
  case Let(defs, body) => sys.error("not yet implemented")
  case LetStar(defs, body) => sys.error("not yet implemented")
}

/**
Optional third exercise (3)
 */
/**
The `LetStar` construct is similar to let, but the scope of a definition
contains all right hand sides of definitions that follow the current one.
The following test case should hence evaluate to 11.
*/

val test2 =
     With("x", 1,
      LetStar(List("x" -> 5, "y" -> Add("x", 1)), Add("x", "y")))

/**
Your task: Implement the missing parts of `subst` and `eval` to support `LetStar`.
(Again, only change the parts currently filled with an error!)
Then eliminate `LetStar` by defining it as syntactic sugar.
*/
}


/**
Task 2: Static Scoping vs. Dynamic Scoping
------
*/
object Hw03Task2 {
/**
Consider the environment-based interpreter for the language `F1WAE` from
the lecture notes (https://ps-tuebingen-courses.github.io/pl1-lecture-notes/06-first-order-functions/first-order-functions.html):
*/

object Syntax {
  enum Exp:
    case Num(n: Int)
    case Add(lhs: Exp, rhs: Exp)
    case Mul(lhs: Exp, rhs: Exp)
    case Id(x: String)
    case With(x: String, xdef: Exp, body: Exp)

    /** The new language constructs for first-order functions: */
    case Call(f: String, args: List[Exp]) // functions are called by name

  object Exp:
    /** The new language constructs for first-order functions: */
    case class FunDef(args: List[String], body: Exp)
    type Funs = Map[String, FunDef]

    /** We use implicits again to make example programs less verbose. */
    implicit def num2exp(n: Int): Exp = Num(n)
    implicit def string2exp(x: String): Exp = Id(x)
}
import Syntax._
import Exp._

type Env = Map[String, Int]

def evalWithEnv(funs: Funs, env: Env, e: Exp): Int = e match {
  case Num(n) => n
  case Id(x) => env(x) // look up in repository of deferred substitutions
  case Add(l, r) => evalWithEnv(funs, env, l) + evalWithEnv(funs, env, r)
  case Mul(l, r) => evalWithEnv(funs, env, l) * evalWithEnv(funs, env, r)
  case With(x, xdef, body) => evalWithEnv(funs, env + ((x,evalWithEnv(funs, env, xdef))), body)
  case Call(f, args) => {
     val fd = funs(f) // lookup function definition
     val vargs = args.map(evalWithEnv(funs, env, _)) // evaluate function arguments
     if (fd.args.size != vargs.size) sys.error("number of paramters in call to " + f + " does not match")
     // We construct the environment by associating each formal argument to its actual value.
     val newenv = Map() ++ fd.args.zip(vargs)
     evalWithEnv(funs, newenv, fd.body)
  }
}

def evalDynScope(funs: Funs, env: Env, e: Exp): Int = e match {
  case Num(n) => n
  case Id(x) => env(x)
  case Add(l, r) => evalDynScope(funs, env, l) + evalDynScope(funs, env, r)
  case Mul(l, r) => evalDynScope(funs, env, l) * evalDynScope(funs, env, r)
  case With(x, xdef, body) => evalDynScope(funs, env + ((x,evalDynScope(funs, env, xdef))), body)
  case Call(f, args) => {
     val fd = funs(f)
     val vargs = args.map(evalDynScope(funs, env, _))
     if (fd.args.size != vargs.size) sys.error("number of paramters in call to "+ f + " does not match")
     val newenv = env ++ fd.args.zip(vargs) // extending env instead of Map() !!
     evalDynScope(funs, newenv, fd.body)
  }
}

/**
The following example from the lecture notes yields a result with the dynamically scoped
interpreter, but fails with the statically scoped interpreter.
*/

val funnyFun = Map("funny" -> FunDef(List("a"), Add("a", "b")))
val callFunnyFun = With("b", 3, Call("funny", List(4)))
val evalDynFunnyFun = evalDynScope(funnyFun, Map.empty, callFunnyFun)
assert(evalDynFunnyFun == 7)

// val evalStatFunnyFun = evalWithEnv(funnyFun, Map.empty, callFunnyFun) // fails

/**
Your task: Explain why this is the case. What is the difference between static
scoping and dynamic scoping? What is the problem with dynamic scoping?
*/
}
