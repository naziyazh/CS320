package cs320

package object midterm extends Midterm {

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
    case  App(func, args, assigned) => interp(func, env) match{
      case CloV(params, body, fenv) => 
        if (assigned.size == 0){
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
        }else{
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
        }
      case v => error(s"Not a closure: $v")
    }         //     | e(e,...,e)
    case  Fun(params, default_vals, body) => CloV(params, body, env ++ default_vals.transform((k,v) => interp(v, env))) //     | {(x,...,x=e)=>e}
    case  Rec(rec) => RecV(rec.transform((k,v) => interp(v, env)))             //     | {x=e,...,x=e}
    case  Acc(expr, name) => interp(expr, env) match{
      case RecV(map) => map.getOrElse(name, error("no such field"))
      case _ => error("Not a record")
    }
  }

  def tests: Unit = {
    test(interp(App(Fun(List("x","y"),Map("x" -> Num(1),"y" -> Num(2)),Add(Id("x"),Id("y"))), Nil, Map()),Map()),NumV(3))
    test(interp(App(Fun(List("a", "b", "c"), Map("b"-> Val("x", Num(10), Add(Id("x"), Id("x")))), Sub(Add(Id("a"), Id("b")),Id("c"))), List(Num(6)), Map("c" -> Acc(Rec(Map("z"-> Num(10))),"z"))),Map()),NumV(16))
    test(interp(Fun(List("a1","a2","a3","a4"), Map(), Val("x", Sub(Add(Id("a1"),Id("a4")),Id("a2")),Sub(Id("x"),Id("a3")))), Map()), CloV(List("a1","a2","a3","a4"), Val("x", Sub(Add(Id("a1"),Id("a4")),Id("a2")),Sub(Id("x"),Id("a3"))),Map()))
    testExc(interp(App(Fun(List("a","b","c"), Map("c"->Num(3), "b"-> Num(2)), Add(Add(Id("a"),Id("b")), Id("c"))), List(Num(1),Num(3)), Map("b"->Num(5))), Map()), "wrong arity")
    test(interp(Val("h", Fun(List("x","y","z","w"), Map(), Add(Add(Id("x"), Id("y")),Add(Id("z"), Id("w")))), App(Id("h"), List(Num(1),Num(4),Num(5),Num(6)), Map())), Map()),NumV(16))
    test(interp(Val("h", Fun(List("x","y","z","w"), Map("x"-> Num(1),"y"-> Num(4),"z"-> Num(5),"w"-> Num(6)), Add(Add(Id("x"), Id("y")),Add(Id("z"), Id("w")))), App(Id("h"), List(), Map())), Map()),NumV(16))
    test(interp(Val("h", Fun(List("x","y","z","w"), Map(), Add(Add(Id("x"), Id("y")),Add(Id("z"), Id("w")))), App(Id("h"), List(), Map("y"-> Num(4), "x" -> Num(1), "w" -> Num(6), "z" -> Num(5)))), Map()),NumV(16))
  }
}
