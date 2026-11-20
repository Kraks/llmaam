package llmaam.frontend.scm

extension (e: Expr)
  def desugar: Expr = new SchemeASTDesugar().apply(e)

class SchemeASTDesugar:
  // Generated identifiers contain a reader-reserved prefix and cannot capture source names.
  private def bindOnce(name: String, value: Expr)(body: Var => Expr): Expr =
    Let(List(Bind(name, apply(value))), body(Var(name))).toApp

  def apply(expr: Expr): Expr = expr match
    case App(f, args) => App(apply(f), args.map(apply))
    case Lam(params, body) => Lam(params, apply(body))
    case RestLam(params, rest, body) => RestLam(params, rest, apply(body))
    case If(c, t, e) => If(apply(c), apply(t), apply(e))
    case Cond(branches) => branches.foldRight[Expr](Void()) {
      case (CondBr(c, t), rest) => If(apply(c), apply(t), rest)
      case (CondTest(c), rest) => bindOnce("#:cond", c)(v => If(v, v, rest))
      case (CondProcBr(c, proc), rest) => bindOnce("#:cond", c)(v => If(v, App(apply(proc), List(v)), rest))
    }
    case Case(key, branches, otherwise) => bindOnce("#:case", key) { v =>
      branches.foldRight(apply(otherwise)) { (branch, rest) =>
        val thn = apply(branch.thn)
        branch.cases.foldRight(rest)((k, xs) => If(App(Var("eqv?"), List(v, apply(k))), thn, xs))
      }
    }
    case SetVar(x, rhs) => SetVar(x, apply(rhs))
    case Begin(es) => Begin(es.map(apply))
    case Define(x, rhs) => Define(x, apply(rhs))
    case _ => expr
