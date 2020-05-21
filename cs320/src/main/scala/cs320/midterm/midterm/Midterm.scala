package cs320

trait Midterm extends Homework {
    // Expr type
  trait Expr
  case class Num(num: Int) extends Expr                               // e ::= n
  case class Add(left: Expr, right: Expr) extends Expr                //     | (e + e)
  case class Sub(left: Expr, right: Expr) extends Expr                //     | (e - e)
  case class Val(name: String, value: Expr, body: Expr) extends Expr  //     | {val x=e;e}
  case class Id(name: String) extends Expr                            //     | x
  case class App(func: Expr, args: List[Expr], assigned: Map[String, Expr]) extends Expr           //     | e(e,...,x=e)
  case class Rec(rec: Map[String, Expr]) extends Expr                 //     | {x=e,...,x=e}
  case class Acc(expr: Expr, name: String) extends Expr               //     | e.x
  case class Fun(params: List[String], default_vals : Map[String, Expr], body : Expr) extends Expr 
  // Record map
  type RecMap = Map[String, Value]

  // Value type
  trait Value
  case class NumV(n: Int) extends Value
  case class CloV(params: List[String], body: Expr, env: Env) extends Value
  case class RecV(map: RecMap) extends Value

  // Check duplicated string values in a given string list.
  def dupCheck(ss: List[String]): Boolean = ss.distinct.length != ss.length
  type Env = Map[String, Value]
  def interp(expr: Expr, env: Env): Value
}