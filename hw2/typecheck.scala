package typecheck
import scala.meta._

object `package` {
	def isWrong(node: Type): Boolean = node match {
		case x: Type.Function => false
		case x: Type.Name => node.syntax == "Wrong Type"
	}
    def getType(node: Term, env: List[Term.Param]): Type = node match {
    	// n
		case x: Lit.Int => Type.Name("Int")
		// var 
		case x: Term.Name => env.find(param => param.name.value == x.value) match {
			case Some(i) => i.decltpe match {
				case None => Type.Name("Wrong Type")
				case Some(v) => v
			}
			case None => Type.Name("Wrong Type")
		}
		// ADD
		case Term.ApplyInfix(lhs, Term.Name("+"), targs, rhs::Nil) => {
			var lt = getType(lhs, env)
			var rt = getType(rhs, env)
			if ( lt.asInstanceOf[Type.Name].value == "Int" && rt.asInstanceOf[Type.Name].value == "Int") {
				return Type.Name("Int")
			} else {
				return Type.Name("Wrong Type")
			}
		}
		// lambda
		case x: Term.Function => {
			// t1 => t2
			var t1 = x.params(0).decltpe match {
				case None => Type.Name("wrong Type")
				case Some(v) => v
			}
			var t2 = getType(x.body, List(x.params(0)) ++ env)
			// println("t1: " + t1 + "  " + t1.structure)
			// println("t2: " + t2 + "  " + t2.structure)

			if (isWrong(t1) || isWrong(t2)) {
				return Type.Name("Wrong Type")
			} else {
				return Type.Function(params = List(t1), res = t2)
			}
		}
		// v v
		case Term.Apply(fun, arg::Nil) => {
			var lambdaType = getType(fun, env) // t1 => t2
			lambdaType match {
				case Type.Function((param:Type)::Nil, res) => {
					var t1_f = getType(arg, env) // t1
					var t1_p = param // t1
					// println(t1_f.syntax + " ===? " + lambdaType.syntax + "====" + t1_p.syntax)
					if (!isWrong(t1_f) && t1_f.syntax.replaceAll("\\s", "") == t1_p.syntax.replaceAll("\\s", "")) res else Type.Name("Wrong Type")
				}
				case _ => Type.Name("Wrong Type")
			}
		}
		case _          => {
			return Type.Name("Wrong Type")
		}
	}
}