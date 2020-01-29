import scala.meta._

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
object Main {
	def eval(s: String) = {
		val toolbox = currentMirror.mkToolBox()
    	val tree = toolbox.parse(s)
    	toolbox.eval(tree)
	}
	def step(node: Term): Term = {
		node match {
			case x: Lit.Int => x
    		case x: Lit.Boolean => x
			case x: Term.ApplyInfix => {
				var left:Term = x.lhs
				var right:Term = x.args(0)
				if (left.isInstanceOf[Term.ApplyInfix] || left.isInstanceOf[Term.If]) {
					return x.copy(lhs = step(left))
				}
				else if (right.isInstanceOf[Term.ApplyInfix] || right.isInstanceOf[Term.If]) {
					return x.copy(args = List(step(right)))
				}
				else if (left.isInstanceOf[Lit.Int] && right.isInstanceOf[Lit.Int]) {
					var leftV = left.asInstanceOf[Lit.Int].value
					var rightV = right.asInstanceOf[Lit.Int].value
					if (x.op.value == "+") {
						return Lit.Int(leftV + rightV)
					} 
					else if (x.op.value == "*") {
						return Lit.Int(leftV * rightV)
					} 
					else if (x.op.value == "/") {
						return Lit.Int(leftV / rightV)
					} 
					else if (x.op.value == "<=") {
						return Lit.Boolean(leftV <= rightV)
					} 
					else if (x.op.value == ">") {
						return Lit.Boolean(leftV > rightV)
					} 
					else {
						throw new Exception("unknown operator" + x.op.value)
					}
				}
				else {
					throw new Exception("incorrect type")
					// return "Wrong Type"
				}
			}
    		case x: Term.If => {
    			if (x.cond.isInstanceOf[Term.ApplyInfix] || x.cond.isInstanceOf[Term.If]) {
    				return x.copy(cond = step(x.cond))
    			} else if (x.cond.isInstanceOf[Lit.Boolean]) {
    				if (x.cond.asInstanceOf[Lit.Boolean].value == true) {
    					return x.thenp
    				} else {
    					return x.elsep
    				}
    			} else {
    				throw new Exception("incorrect type for condition")
    			}
    		}
    		case _          => {
    			throw new Exception("unknown AST node")
    		}
    	}
    }

    def getType(node: Term): String = {
    	node match {
    		case x: Lit.Int => "nat"
    		case x: Lit.Boolean => "bool"
			case x: Term.ApplyInfix => {
				if (x.op.value == "+" || x.op.value == "*" || x.op.value == "<=") {
					var left:Term = x.lhs
					var right:Term = x.args(0)
					if (getType(left) == "nat" && getType(right) == "nat") {
						if (x.op.value == "<=") {
							return "bool"
						} else {
							return "nat"
						}
					} else {
						return "Wrong Type"
					}
				}
				else {
					throw new Exception("unknown operator" + x.op.value)
				}
			}
    		case x: Term.If => {
    			if (getType(x.cond) == "bool") {
    				var a2 = getType(x.thenp)
    				var a3 = getType(x.elsep)
    				if (a2 == a3) {
    					return a2
    				} else {
    					return "Wrong Type"
    				}
    			} else {
    				return "Wrong Type"
    			}
    		}
    		case _          => {
    			return "Wrong Type"
    		}
    	}
    }

	def main(args: Array[String]): Unit = {
		var testcases = Array(
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

		var i = 0
		testcases.foreach(c => {
			var program = c(0).asInstanceOf[String]
			var typeResult = c(1).asInstanceOf[String]
			var tree = program.parse[Term].get
			i += 1
			println("-----------------------------------------")
			println("Test Case " + i + " : " + program)
			// 1. check type
			println("Type is: " + getType(tree))
			println("Type check: " + (if (typeResult == getType(tree)) "PASS***" else "FAIL!!!"))
			if (getType(tree) != "Wrong Type") {
				// 2. get value
				println("Steps are:")
				while (!(tree.isInstanceOf[Lit.Int] || tree.isInstanceOf[Lit.Boolean])) {
					println("==> " + tree.syntax)
					tree = step(tree)
				}
				println("==> " + tree.syntax)
				println("Value check: " + (if (tree.syntax == eval(program).toString()) "PASS***" else "FAIL!!!"))
			}
		})
	}
}