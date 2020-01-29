import scala.meta._

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.{universe => u}


object Main {
	implicit class ColonTypeExtender [T : u.TypeTag] (x : T) {
	  def colonType = u.typeOf[T].toString
	}
	def eval(s: String) = {
		val toolbox = currentMirror.mkToolBox()
    	val tree = toolbox.parse(s)
    	toolbox.eval(tree)
	}
	def subs(node: Term, x: Term.Name, v: Term): Term = node match {
		// case 1: n
		case e: Lit.Int => e
		// case 2: y
		case e: Term.Name => {
			if (e.value == x.value) {
				return v
			} else {
				return e
			}
		}
		// case 3: e1 + e2
		case e: Term.ApplyInfix => e.copy(lhs = subs(e.lhs, x, v), args = List(subs(e.args(0), x, v)))
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
		// exception
		case _ => {
			???
		}
	}
	def step(node: Term): Term = {
		node match {
			case n: Lit.Int => n
			case lambda: Term.Function => lambda
			case e: Term.ApplyInfix => {
				var left:Term = e.lhs
				var right:Term = e.args(0)
				if (left.isInstanceOf[Term.ApplyInfix]) {
					// ADD1: e1 + e2
					return e.copy(lhs = step(left))
				}
				else if (right.isInstanceOf[Term.ApplyInfix]) {
					// ADD2: v + e2
					return e.copy(args = List(step(right)))
				}
				else if (left.isInstanceOf[Lit.Int] && right.isInstanceOf[Lit.Int]) {
					// ADD3: n1 + n2
					var leftV = left.asInstanceOf[Lit.Int].value
					var rightV = right.asInstanceOf[Lit.Int].value
					if (e.op.value == "+") {
						return Lit.Int(leftV + rightV)
					}
					else {
						throw new Exception("unknown operator" + e.op.value)
					}
				}
				else {
					throw new Exception("incorrect type")
					// return "Wrong Type"
				}
			}
    		case e: Term.Apply => {
    			e.fun match {
    				// APP1: e1 e2
    				case lambda: Term.Apply => e.copy(fun = step(lambda))
    				case lambda: Term.Function => e.args(0) match {
						// APP3: lambda v ==> e[x -> v]
						case arg: Lit.Int => subs(lambda.body, lambda.params(0).name.asInstanceOf[Term.Name], arg)
						case arg: Term.Function => subs(lambda.body, lambda.params(0).name.asInstanceOf[Term.Name], arg)
						// APP2: v e2
						case _ => e.copy(args = List(step(e.args(0))))
					}
    			}
    		}
    		case _          => {
    			throw new Exception("unknown AST node")
    		}
    	}
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
		case x: Term.ApplyInfix => {
			if (x.op.value == "+") {
				var left:Term = x.lhs
				var right:Term = x.args(0)
				var leftType = getType(left, env)
				var rightType = getType(right, env)
				if ( leftType.isInstanceOf[Type.Name] 
					&& leftType.asInstanceOf[Type.Name].value == "Int" 
					&& rightType.isInstanceOf[Type.Name] 
					&& rightType.asInstanceOf[Type.Name].value == "Int"
				) {
					return Type.Name("Int")
				} else {
					return Type.Name("Wrong Type")
				}
			}
			else {
				throw new Exception("unknown operator" + x.op.value)
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
			if (t2.isInstanceOf[Type.Name] && t2.asInstanceOf[Type.Name].value == "Wrong Type") {
				return Type.Name("Wrong Type")
			} else {
				return Type.Function(params = List(t1), res = t2)
			}
		}
		// e1 e2
		case x: Term.Apply => {
			var e1 = x.fun  	// t1 => t2
			var e2 = x.args(0) 	// t1
			var lambdaType = getType(e1, env).asInstanceOf[Type.Function]
			var t1 = getType(e2, env)
			println("lambdaType:" + lambdaType.syntax)
			println("t1:" + t1.syntax)
			println("t111:" + lambdaType.params(0).syntax)

			if (t1.isInstanceOf[Type.Name] 
				&& t1.asInstanceOf[Type.Name].value != "Wrong Type" 
				&& lambdaType.params(0).syntax == t1.syntax
			) {
				return lambdaType.res
			} else {
				return Type.Name("Wrong Type")
			}
		}
		case _          => {
			return Type.Name("Wrong Type")
		}
	}

	def main(args: Array[String]): Unit = {
		var testcases1 = Array(
			Array("1", "nat"),
			// bool
			Array("true", "bool"),
			Array("false", "bool"),
			// n1+n2
			Array("1+2", "nat"),
			// n+a2
			Array("1+(2+3)", "nat"),
			Array("1+(2*3)", "nat"),
			// a1+a2
			Array("(1+2)+(3+4)", "nat"),
			Array("(1*2)+(3*4)", "nat"),
			Array("true+(3*4)", "Wrong Type"),
			Array("true+false", "Wrong Type"),
			// n1*n2
			Array("1*2", "nat"),
			// n*a2
			Array("1*2*3", "nat"),
			// a1*a2
			Array("(1+1)*(2+3)", "nat"),
			// n1<=n2
			Array("1<=1", "bool"),
			Array("1<=2", "bool"),
			Array("2<=1", "bool"),
			// n<=a2
			Array("1<=(1+1)", "bool"),
			// a1<=a2
			Array("(2+1)<=(1+1)", "bool"),
			// if true then a2 else a3
			Array("if (true) 1 else 2", "nat"),
			Array("if (true) 1 else false", "Wrong Type"),
			Array("if (true) 2+3 else 4+5", "nat"),
			// if false then a2 else a3
			Array("if (false) 2+3 else 4+5", "nat"),
			// if a1 then a2 else a3
			Array("if (1<=2) 1 else 2", "nat"),
			Array("if (2<=1) 1 else 2", "nat"),
			Array("if (1<=1) 2+3 else 4+5", "nat"),
			Array("if (1<=1) true else false", "bool"),
			// mixed
			Array("1 * (if (1<=2) 1 else 2)", "nat"),
			Array("1 + (if (1<=2) true else false)", "Wrong Type"),
			Array("(if (true) 1 else 2) + (if (1<=2) true else false)", "Wrong Type"),
			Array("(if (true) 1 else 2) + (if (1<=2) 5 else 6)", "nat"),
			Array("true + (if (1<=2) 5 else 6)", "Wrong Type"),
			Array("if (1<=1) (if (1<=1) 2+3 else 4+5) else 6+7", "nat"),
			Array("if (if (1<=1) true else false) (if (1<=1) 2+3 else 4+5) else 6+7", "nat"),
			Array("if (if (if (if (1<=1) true else false) true else false) true else false) (if (1<=1) 2+3 else 4+5) else 6+7", "nat")
		)

		var testcases = Array(
			Array("((x:Int) => x+1)(1)", "lambda"),
			Array("((x:Int) => x+2)", "lambda"),
			Array("((x:Int) => ((z:Int) => x + z)(1))(2)", "lambda"),
			Array("(x:Int) => ((z:Int) => x + z)", "lambda"),
			Array("(x:Int=>Int) => x", "lambda"),
			Array("(x:Int=>Int) => 1", "lambda"),

		)
		var i = 0
		testcases.foreach(c => {
			var program = c(0).asInstanceOf[String]
			var typeResult = c(1).asInstanceOf[String]
			var tree = program.parse[Term].get
			i += 1
			println("-----------------------------------------")
			println("Test Case " + i + " : " + program)
			// while (!(tree.isInstanceOf[Lit.Int] || tree.isInstanceOf[Term.Function])) {
			// 	println("==> " + tree.syntax)
			// 	tree = step(tree)
			// }
			// println("==> " + tree.syntax)
			println("Type: " + getType(tree, List()).syntax)
			var result = eval(program)

			var x:Int = 1
			println("Type Should be: " + ColonTypeExtender(x).colonType)
			println("Value Should Be: " + result.toString())

			// 1. check type
			// println("Type is: " + getType(tree))
			// println("Type check: " + (if (typeResult == getType(tree)) "PASS***" else "FAIL!!!"))
			// if (getType(tree) != "Wrong Type") {
				// 2. get value
			// 	println("Steps are:")
			// 	while (!(tree.isInstanceOf[Lit.Int] || tree.isInstanceOf[Lit.Boolean])) {
			// 		println("==> " + tree.syntax)
			// 		tree = step(tree)
			// 	}
			// 	println("==> " + tree.syntax)
			// 	println("Value check: " + (if (tree.syntax == eval(program).toString()) "PASS***" else "FAIL!!!"))
			// }
		})
	}
}