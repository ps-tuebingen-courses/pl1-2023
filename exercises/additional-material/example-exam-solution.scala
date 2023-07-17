// Programmieraufgabe 1

// def eval(e: Exp, env: Env, k: Value => Nothing): Nothing = e match {
//    ...
//    case Letcc(param,body) => eval(body, env + (param -> ContV(k)), k)
//  }

// Programmieraufgabe 2

def map(xs: List[Int])(f: Int => (Int, Int)): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }

def flatMap(xs: List[Int])(f: Int => List[(Int, Int)]): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => f(x) ++ flatMap(xs)(f)
  }

def product(l1: List[Int], l2: List[Int]): List[(Int, Int)] =
  flatMap(l1)(x =>
    map(l2)(y =>
      (x, y)))

// Lambda-Lifted:

def f2(x: Int)(y: Int): (Int, Int) = (x, y)
def f1(l2: List[Int])(x: Int): List[(Int, Int)] = map(l2)(f2(x))

def product_ll(l1: List[Int], l2: List[Int]): List[(Int, Int)] =
  flatMap(l1)(f1(l2))


// Defunctionalized using one GADT:

object DefunGADT {

enum FunctionInt[T]:
  case F2(x: Int) extends FunctionInt[(Int, Int)]
  case F1(l2 : List[Int]) extends FunctionInt[List[(Int, Int)]]
import FunctionInt._

def apply[T](f: FunctionInt[T], arg: Int): T =
  f match {
    case F2(x) => (x, arg)
    case F1(l2) => map_defun(l2)(F2(arg))
  }

def map_defun(xs: List[Int])(f: FunctionInt[(Int, Int)]): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => apply(f, x) :: map_defun(xs)(f)
  }

def flatMap_defun(xs: List[Int])(f: FunctionInt[List[(Int, Int)]]): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => apply(f, x) ++ flatMap_defun(xs)(f)
  }

def product_defun(l1: List[Int], l2: List[Int]): List[(Int, Int)] =
  flatMap_defun(l1)(F1(l2))

}

assert(product(List(1, 2, 3), List(3, 4, 5)) == DefunGADT.product_defun(List(1, 2, 3), List(3, 4, 5)))


// Defunctionalized using two ADTs:

object DefunTwo {

enum FunctionIntPair:
  case F2(x: Int)
import FunctionIntPair._

def apply_2(f: FunctionIntPair, arg: Int): (Int, Int) =
  f match {
    case F2(x) => (x, arg)
  }

enum FunctionListIntPair:
  case F1(l2 : List[Int])
import FunctionListIntPair._

def apply_1(f: FunctionListIntPair, arg: Int): List[(Int, Int)] =
  f match {
    case F1(l2) => map_defun(l2)(F2(arg))
  }

def map_defun(xs: List[Int])(f: FunctionIntPair): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => apply_2(f, x) :: map_defun(xs)(f)
  }

def flatMap_defun(xs: List[Int])(f: FunctionListIntPair): List[(Int, Int)] =
  xs match {
    case Nil     => Nil
    case x :: xs => apply_1(f, x) ++ flatMap_defun(xs)(f)
  }

def product_defun(l1: List[Int], l2: List[Int]): List[(Int, Int)] =
  flatMap_defun(l1)(F1(l2))

}

assert(product(List(1, 2, 3), List(3, 4, 5)) == DefunTwo.product_defun(List(1, 2, 3), List(3, 4, 5)))


// Multiple Choice

// Richtige Antworten:

// 1. ... der möglicherweise zuletzt ausgeführte Funktionsaufruf
//    in einer Funktion zur Laufzeit.

// 2. ... `(AddressV(1), Map(1 -> NumV(42)))` sein.

// 3. Ein Monadentransformer kann häufig verwendet werden,
//    um Monaden miteinander zu kombinieren.

// 4. Jedes Programm, welches unter call-by-value terminiert,
//    terminiert auch unter call-by-need.
