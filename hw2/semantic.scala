package semantic
import scala.meta._

object `package` {
	def subs(node: Term, x: Term.Name, v: Term): Term = node match {
		// case 1: n
		case e: Lit.Int => e
		// case 2: y
		case e: Term.Name => if (e.value == x.value) v else e
		// case 3: e1 + e2
		case Term.ApplyInfix(lhs, Term.Name("+"), targs, arg::Nil) => 
			Term.ApplyInfix(subs(lhs, x, v), Term.Name("+"), targs, List(subs(arg, x, v)))
		// case 4: lambda
		case lambda: Term.Function => {
			if (lambda.params(0).name.value == x.value) {
				// x == y
				return lambda
			} else {
				// x != y
				return lambda.copy(body = subs(lambda.body, x, v))
			}
		}
		// case 5: e1 e2
		case e: Term.Apply => e.copy(fun = subs(e.fun, x, v), args = List(subs(e.args(0), x, v)))
		// stuck
		case _ => node
	}
	def step(node: Term): Term = {
		node match {
			case n: Lit.Int => n
			case lambda: Term.Function => lambda
			// ADD1: e1 + e2
			case Term.ApplyInfix(lhs: Term.ApplyInfix, Term.Name("+"), targs, args) => 
				Term.ApplyInfix(step(lhs), Term.Name("+"), targs, args)
			// ADD2: v + e2
			case Term.ApplyInfix(lhs: Lit.Int, Term.Name("+"), targs, (rhs:Term.ApplyInfix)::Nil) => 
				Term.ApplyInfix(lhs, Term.Name("+"), targs, List(step(rhs)))
			// ADD2: v1 + v2
			case Term.ApplyInfix(Lit.Int(lv), Term.Name("+"), targs, Lit.Int(rv)::Nil) => 
				Lit.Int(lv + rv)
			// APP1: e1 e2
    		case Term.Apply(fun: Term.Apply, args) => 
    			Term.Apply(step(fun), args)
    		// APP2: v e2
    		case Term.Apply(fun: Term.Function, (param: Term.ApplyInfix)::Nil) => 
    			Term.Apply(step(fun), List(step(param)))
    		case Term.Apply(fun: Term.Function, (param: Term.Apply)::Nil) => 
    			Term.Apply(step(fun), List(step(param)))
    		// APP3: v v
    		case Term.Apply(fun: Term.Function, (param:Term.Function)::Nil) =>
    			subs(fun.body, fun.params(0).name.asInstanceOf[Term.Name], param)
    		case Term.Apply(fun: Term.Function, (param: Lit.Int)::Nil) => 
    			subs(fun.body, fun.params(0).name.asInstanceOf[Term.Name], param)
    		// stuck
    		case _ => node
    	}
    }
}