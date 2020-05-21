package cs320

package object proj01 extends Project01 {

  def intVop(op: (Int, Int) => Int): (Value, Value) => IntV = (_, _) match {
    case (IntV(x), IntV(y)) => IntV(op(x, y))
    case (x, y) => error(s"not both numbers: $x, $y")
  }
  val intVAdd = intVop(_ + _)
  val intVMul = intVop(_ * _)
  val intVDiv = intVop(_ / _)
  val intVMod = intVop(_ % _)
  def typematch(a : Type, b : Type) : BooleanV ={
    if (a == b){
      BooleanV(true)
    }else{
      BooleanV(false)
    }
  }
  def interp(e: Expr, env: Env): Value = e match {
    case  IntE(n) => IntV(n) // integer
    case  Add(l, r) => (interp(l, env), interp(r, env)) match{
      case (IntV(n1), IntV(n2)) => intVAdd(IntV(n1), IntV(n2))
      case _ => error("values must be integers")
    }// addition
    case  Mul(l, r) => (interp(l, env), interp(r, env)) match{
      case (IntV(n1), IntV(n2)) => intVMul(IntV(n1), IntV(n2))
      case _ => error("values must be integers")
    }// multiplication
    case  Div(l, r) => (interp(l, env), interp(r, env)) match{
      case (IntV(n1), IntV(n2)) => 
      if (n2 == 0){
        error("Cannot divide by 0")
      }else{
        intVDiv(IntV(n1), IntV(n2))
      }
      case _ => error("values must be integers")
    }// division
    case  Mod(l, r) => (interp(l, env), interp(r, env)) match{
      case (IntV(n1), IntV(n2)) => 
      if (n2 == 0){
        error("Cannot modulo by 0")
      }else{
        intVMod(IntV(n1), IntV(n2))
      }
      case _ => error("values must be integers")
    }// modulo
    case BooleanE(b) => BooleanV(b)
    case Eq(l, r) => (interp(l, env), interp(r,env)) match {
      case (IntV(n1), IntV(n2)) => 
        if (n1 == n2){
          BooleanV(true) 
        }else{
          BooleanV(false)
        }
      case _ => error("values must be integers")
    }
    case Lt(l, r) => (interp(l, env), interp(r,env)) match {
      case (IntV(n1), IntV(n2)) => 
        if (n1 < n2){
          BooleanV(true) 
        }else{
          BooleanV(false)
        }
      case _ => error("values must be integers")
    }
    case TupleE(es) => TupleV(es.map(interp(_, env)))
    case Proj(t, i) => interp(t, env) match {
      case TupleV(vs) => vs.apply(i - 1)
      case v => error(s"$v is not a tuple")
    } 
    case NilE => NilV
    case ConsE(h, t) => 
      val head = interp(h, env)
      val tail = interp(t, env) 
      tail match{
        case NilV => ConsV(head, tail)
        case ConsV(h, t) => ConsV(head, tail)
        case v => error(s"$v is not a list")
      }
    case Empty(l) => interp(l, env) match {
      case NilV => BooleanV(true)
      case ConsV(h, t) => BooleanV(false)
      case v => error(s"$v is not a list")
    }
    case Head(l) => interp(l, env) match {
      case ConsV(h, t) => h
      case NilV => NilV
      case v => error(s"$v is not a list")
    }
    case Tail(l) => interp(l, env) match {
      case ConsV(h, t) => t
      case NilV => NilV
      case v => error(s"$v is not a list")
    }
    case Id(x) => env.getOrElse(x, error(s"$x is free variable"))
    case Val(x, e, b) => interp(b, env + (x -> interp(e, env)))
    case Fun(ps, b) =>  CloV(ps, b, env)
    case App(f, as) => interp(f, env) match {
      case CloV(ps, b, fenv) => 
        if (ps.size == as.size){
          val args = as.map(interp(_, env))
          interp(b, fenv ++ (ps zip args))
        }else{
          error("number of arguments and parameters don't match")
        }
      case v => error(s"Not a closure: $v")
    }
    case Test(e, t) => 
      val val_e = interp(e, env)
      val_e match {
        case BooleanV(b) => typematch(BooleanT, t)
        case TupleV(vs) => typematch(TupleT, t)
        case NilV => typematch(ListT, t)
        case ConsV(hd, tl ) => typematch(ListT, t)
        case IntV(n) => typematch(IntT, t)
        case CloV(ps, b, fenv) => typematch(FunctionT, t) 
        case v => error(s"$v not a value")
      }
      
    case If(c, t, f) => interp(c, env) match{
      case BooleanV(true) => interp(t, env)
      case BooleanV(false) => interp(f, env)
      case v => error(s"$v is not a boolean")  
    } 
    case RecFuns(ds, b) => 
      val closures = ds.map(d => d match {
        case FunDef(n, ps, bd) => CloV(ps, bd, env)
        case _ => error("not a function definition")
      } )
      val func_names = ds.map(d => d match {
        case FunDef(n, ps, bd) => n
        case _ => error("not a function definition")
      } )
      val nenv = env ++ (func_names zip closures)
      closures.map(cl => cl.env = nenv)
      interp(b, nenv)
    case v => error(s"WHAT IS $v ?")
  }

  def tests: Unit = {
    // test-int
    test(run("42"), "42")
    // test-add
    test(run("1 + 2"), "3")
    // test-sub
    test(run("7 - 2"), "5")
    // test-mul
    test(run("2 * 4"), "8")
    // test-div
    test(run("5 / 2"), "2")
    // test-mod
    test(run("13 % 5"), "3")
    // test-neg
    test(run("1 - -1"), "2")
    // test
    test(run("1 - 134 % 23 * 67 + 2"), "-1270")

    // test-boolean
    test(run("true"), "true")
    // test-eq
    test(run("1 == 3 - 2"), "true")
    // test-lt
    test(run("1 < 3 - 2"), "false")

    // test-tuple1
    test(run("(1, 2 + 3, true)"), "(1, 5, true)")
    // test-tuple2
    test(run("((42, 3 * 2), false)"), "((42, 6), false)")
    
    // test-proj1
    test(run("(1, 2 + 3, true)._1"), "1")
    // test-proj2
    test(run("((42, 3 * 2), false)._1._2"), "6")

    // test-nil
    test(run("Nil"), "Nil")
    // test-cons
    test(run("1 :: 1 + 1 :: Nil"), "(1 :: (2 :: Nil))")
    //test-cons
    testExc(run("2 :: 3"), "3 is not a list")
    // test-isempty1
    test(run("Nil.isEmpty"), "true")
    // test-isempty2
    test(run("(1 :: Nil).isEmpty"), "false")
    // test-head
    test(run("(1 :: Nil).head"), "1")
    // test-tail
    test(run("(1 :: Nil).tail"), "Nil")
    // test-tail
    testExc(run("(1 :: 2 :: 3).tail.tail"), "3 is not a list")
    // test-tail-head
    test(run("(1 :: 2 :: Nil).tail.head"), "2")
    
    // test-local1
    test(run("""
      val x = 1 + 2;
      val y = x * 4 + 1;
      y / (x - 1)
    """), "6")
    // test-local2
    test(run("""
      val (x, y) = (1 + 2, 3 + 4);
      val z = x * y;
      val (a, b, c) = (z, z + z, z + z + z);
      c - b
    """), "21")
    // test -local3
    test(run("val x = 345; val x = 456; -1 + x + x"), "911")
    // local 4
    test(run("val x = 344; val x = x + 456; val y = x * 8; -1 + x + x - y"),"-4801")


    // test-fun
    test(run("x => x + x"), "<function>")
    // test-app1
    test(run("(x => x + x)(1)"), "2")
    // test-app2
    test(run("(x => y => x + y)(1)(2)"), "3")
    // test-app3
    test(run("((x, y) => x + y)(1, 2)"), "3")

    // test-type1
    test(run("1.isInstanceOf[Int]"), "true")
    // test-type2
    test(run("1.isInstanceOf[Boolean]"), "false")
    // test-type3
    test(run("(1 :: Nil).isInstanceOf[List]"), "true")
    // test-type4
    test(run("(x => x + x).isInstanceOf[Function]"), "true")

    // test-if
    test(run("if (true) 1 else 2"), "1")
    // test-not
    test(run("!true"), "false")
    // test-and
    test(run("true && false"), "false")
    // test-or
    test(run("true || false"), "true")
    // test-neq
    test(run("1 != 2"), "true")
    // test-lte
    test(run("1 <= 1"), "true")
    // test-gt
    test(run("1 > 1"), "false")
    // test-gte
    test(run("1 >= 1"), "true")
    // test-nonempty
    test(run("Nil.nonEmpty"), "false")

    // test-rec1
    test(run("""
      def f(x) = x - 1;
      f(2)
    """), "1")
    // test-rec2
    test(run("""
      def f(x) = if (x < 1) 0 else x + f(x - 1);
      f(10)
    """), "55")
    //test-rec3
    test(run("""
      def f(x) = if (x < 1) 0 else x + f(x - 1) +g(x);
      def g(x) = if (x < 1) 0 else 1;
      f(10)+g(2)
    """), "66")
    //test-rec4
     test(run("""
      def f(x) = if (x < 1) 0 else g(x-1) + 3;
      def g(x) = if (x < 1) 0 else f(x-1) + 5;
      f(5)
      """), "19")
    //test-rec5
    test(run("""
      def f(x) = if (x < 0) f(x)+ -2 else 3 + g(x);
      def g(x) = if (x > 8) c(x) else 2 + g(x - 1);
      def c(x) = if (x < -1) x else c(x - 2) + x;
      val x = 347;
      f(x) - g(x) + c(x)
    """), "30275")

  }
}
