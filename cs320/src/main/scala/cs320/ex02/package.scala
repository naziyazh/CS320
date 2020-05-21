package cs320

package object ex02 extends Exercise02 {
  
  // Problem 1
  def freeIds(expr: Expr): Set[String] ={
    def freeIdFinder(expr:Expr, env: Set[String]): Set[String]= expr match{
      case Val(name,expr,body) => freeIdFinder(body, Set(name) ++ env) ++ freeIdFinder(expr, env)
      case Id(id) =>  if (env(id)) Set() else Set(id)
      case Add(left, right) => freeIdFinder(left,env) ++ freeIdFinder(right,env)
      case Sub(left, right) => freeIdFinder(left,env) ++ freeIdFinder(right,env)
      case Num(num) => Set()
    }
    freeIdFinder(expr,Set())
  }

  // Problem 2
  def bindingIds(expr: Expr): Set[String] = expr match {
    case Val(name, expr, body) => Set(name) ++ bindingIds(expr) ++ bindingIds(body)
    case Add(left, right) => bindingIds(left) ++ bindingIds(right)
    case Sub(left, right) => bindingIds(left) ++ bindingIds(right)
    case Num(num) => Set()
    case Id(id) => Set()
  }

  // Problem 3
  def boundIds(expr: Expr): Set[String] = {
    def boundIdsHelper(expr: Expr, env: Set[String]): Set[String]= expr match {
      case Val(name, expr, body) => boundIdsHelper(expr,env) ++ boundIdsHelper(body,env + name)
      case Add(left, right) => boundIdsHelper(left,env) ++ boundIdsHelper(right,env)
      case Sub(left, right) => boundIdsHelper(left,env) ++ boundIdsHelper(right,env)
      case Num(num) => Set()
      case Id(id) => if (env(id)) Set(id) else Set()
    }
    boundIdsHelper(expr,Set())

  }

  // Tests
  def tests: Unit = {
    test(freeIds(Expr("{ val x = 1; (x + y) }")), Set("y"))
    test(freeIds(Expr("{ val z = 2; 1 }")), Set())
    test(bindingIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(bindingIds(Expr("{ val z = 2; 1 }")), Set("z"))
    test(boundIds(Expr("{ val x = 1; (x + y) }")), Set("x"))
    test(boundIds(Expr("{ val z = 2; 1 }")), Set())

    /* Write your own tests */
    test(boundIds(Expr("{ val x = (x + { val y = 3; (y - z) }); (y + { val z = x; (1 + z) }) }")), Set("x","y","z"))
    test(freeIds(Expr("{ val x = (x + { val y = 3; (y - z) }); (y + { val z = x; (1 + z) }) }")), Set("x","y","z"))
    test(bindingIds(Expr("{ val x = (x + { val y = 3; (y - z) }); (y + { val z = x; (1 + z) }) }")), Set("x","y","z"))
  }
}
