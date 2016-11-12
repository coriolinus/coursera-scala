def show(e: Expr): String =  e match {
  case Var(x) => x
  case Number(n) => n.toString
  case Sum(e1, e2) => e1.show + " + " + e2.show
  case Prod(Sum(_, _) as s, r) => '(' + s.show + ") * " + r.show
  case Prod(l, r) => l.show + " * " + r.show
}
