package cs320

import cs320._

package object ex03 extends Exercise03 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(r,l)
      rs.map(f) ++ binOp(op, rest, rs)
  }

  def interp(expr: Expr, env: Env): List[Int] = expr match{
    case Num(nums) => nums
    case Add(left, right) => binOp(_ + _, interp(left, env), interp(right, env))
    case Sub(left, right) => binOp( _ - _, interp(right, env), interp(left, env))
    case Val(name, expr, body) => interp(body, Map(name -> interp(expr,env)) ++ env) 
    case Id(id) => env.getOrElse(id,Nil)
    case Max(left, mid, right) => binOp(math.max, binOp(math.max, interp(left, env), interp(mid, env)) , interp(right, env))
    case Min(left, mid, right) => binOp(math.min, binOp(math.min, interp(left, env), interp(mid, env)) , interp(right, env))
  }

  def tests: Unit = {
    test(run("(3 + 7)"), List(10))
    test(run("(10 - (3, 5))"), List(7, 5))
    test(run("{ val x = (5 + 5); (x + x) }"), List(20))
    test(run("min(3, 4, 5)"), List(3))
    test(run("max((1 + 2), 4, 5)"), List(5))
    test(run("min((1, 4), (2, 9), 3)"), List(1, 1, 2, 3))
    test(run("max((1, 6), (2, 5), (3, 4))"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
    test(run("(3 + x)"), Nil)
    //test(run("(3 + 7)"), List(10))
    //test(run("(3 + 7)"), List(10))

  }
}
