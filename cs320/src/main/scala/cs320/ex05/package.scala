package cs320

package object ex05 extends Exercise05 {

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(num) => NumV(num)                            // e ::= n
    case  Add(left, right) => (interp(left,env), interp(right, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _ => error("Both values must be NumV")
    }              //     | (e + e)
    case  Sub(left, right) =>  (interp(left,env), interp(right, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
      case _ => error("Both values must be NumV")
    }            //     | (e - e)
    case  Val(name, value, body) => interp(body, Map(name -> interp(value,env)) ++ env) //     | {val x=e;e}
    case  Id(name) => env.getOrElse(name, error(s"free identifier $name"))                         //     | x
    case  App(func, args) => interp(func, env) match{
      case CloV(params, body, fenv) => 
        if (params.size == args.size){
          val new_env = args.map(interp(_, env))
          interp(body, fenv ++ (params zip new_env))
        }else{
          val params_without_args = params.drop(args.length)
          if (params_without_args.count(l => fenv.contains(l)) < params_without_args.length){
            error("wrong arity")
          }else{
            val new_params = params.dropRight(params.length - args.length)
            val argument_vals = args.map(interp(_, env))
            interp(body, fenv ++ (new_params zip argument_vals))
          }
        }
      case v => error(s"Not a closure: $v")
    }         //     | e(e,...,e)
    case  AppUnordered(func, args, assigned) => interp(func, env) match {
      case CloV(params, body, fenv) => 
        val free_variables = assigned.keySet.filter(a => !params.contains(a))
        if (!(free_variables.isEmpty)){
          error("not a function parameter")
        }
        val assignedV = assigned.mapValues(interp(_, env))
        val updated_fenv = fenv ++ assignedV 
        val remaining_params = params.filter(l => !(updated_fenv.contains(l)))
        if (remaining_params.length != args.length){
          error("wrong arity")
        }else{
          interp(body, updated_fenv ++ (remaining_params zip args.map(interp(_, env))))
        }
      case v => error(s"Not a closure: $v")
    } // |e(e,.., x = e)
    case  Fun(params, body) => CloV(params, body, env)      //     | {(x,...,x)=>e}
    case  FunDef(params, default_vals, body) => CloV(params, body, env ++ default_vals.transform((k,v) => interp(v, env))) //     | {(x,...,x=e)=>e}
    case  Rec(rec) => RecV(rec.transform((k,v) => interp(v, env)))             //     | {x=e,...,x=e}
    case  Acc(expr, name) => interp(expr, env) match{
      case RecV(map) => map.getOrElse(name, error("no such field"))
      case _ => error("Not a record")
    }
  }

  def tests: Unit = {
    test(run("{ a = 10, b = (1 + 2) }"), "record")
    test(run("{ a = 10, b = (1 + 2) }.b"), "3")
    test(run("{ val g = { r => r.c }; g({ a = 0, c = 12, b = 7 }) }"), "12")
    test(run("{ r = { z = 0 } }.r"), "record")
    test(run("{ r = { z = 0 } }.r.z"), "0")
    test(run("{ val f = { (a, b) => (a + b) }; { val g = { x => (x - 5) }; { val x = f(2, 5); g(x) } } }"), "2")
    test(run("{ val f = { (x, y) => (x + y) }; f(1, 2) }"), "3")
    test(run("{ val f = { () => 5 }; (f() + f()) }"), "10")
    test(run("{ val h = { (x, y, z, w) => (x + w) }; h(1, 4, 5, 6) }"), "7")
    test(run("{ val f = { () => 4 }; { val g = { x => (x + x) }; { val x = 10; ((x + f()) - g(4)) } } }"), "6")
    test(run("{ a = 10, b = (1 + 2) }"), "record")
    test(run("{ val x = 3; { val y = 5; { a = x, b = y }.a } }"), "3")
    test(run("{ val f = { (a, b) => (a.a + b) }; { val g = { x => (5 + x) }; { val x = f({ a = 10, b = 5 }, 2); g(x) } } }"), "17")
    test(run("{ val f = { (a, b, c, d, e) => { a = a, b = b, c = c, d = d, e = e } }; f(1, 2, 3, 4, 5).c }"), "3")
    test(run("{ val f = { (a, b, c) => { a = a, b = b, c = c } }; f(1, 2, 3).b }"), "2")
    test(run("{ val f = { (a, b, c) => { x = a, y = b, z = c, d = 2, e = 3 } }; f(1, 2, 3).y }"), "2")
    test(run("{ val f = { (a, b, c) => { x = a, y = b, z = c, d = 2, e = 3 } }; f(1, 2, 3).d }"), "2")
    test(run("{ val f = { x => (5 + x) }; f({ a = { a = 10, b = (5 - 2) }, b = { x = 50 }.x }.a.b) }"), "8")
    test(run("{ a = 10 }"), "record")
    test(run("{ a = 10 }.a"), "10")
    test(run("{ a = (1 + 2) }.a"), "3")
    test(run("{ x => x }"), "function")
    test(run("{ a = { b = 10 } }.a"), "record")
    test(run("{ a = { a = 10 } }.a.a"), "10")
    test(run("{ a = { a = 10, b = 20 } }.a.a"), "10")
    test(run("{ a = { a = 10, b = 20 } }.a.b"), "20")
    test(run("({ a = 10 }.a + { a = 20 }.a)"), "30")
    test(run("{ a = (2 - 1) }"), "record")
    test(run("{ a = (2 - 1) }.a"), "1")
    test(run("{ val y = { x = 1, y = 2, z = 3 }; y.y }"), "2")
    test(run("{ val y = { x = 1, y = 2, z = 3 }; y.z }"), "3")
    test(run("{ val g = { r => r.c }; g({ a = 0, c = 12, b = 7 }) }"), "12")
    testExc(run("{ a = 10 }.b"), "no such field")
    testExc(run("{ z = { z = 0 }.y }"), "no such field")
    /* Write your own tests */
     test(interp(App(FunDef(List("x","y"),Map("x" -> Num(1),"y" -> Num(2)),Add(Id("x"),Id("y"))), Nil), Map()),NumV(3))
     test(interp(AppUnordered(FunDef(List("a", "b", "c"), Map("b"-> Val("x", Num(10), Add(Id("x"), Id("x")))), Sub(Add(Id("a"), Id("b")),Id("c"))), List(Num(6)), Map("c" -> Acc(Rec(Map("z"-> Num(10))),"z"))),Map()),NumV(16) )
  }
}
