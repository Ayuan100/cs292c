import scala.meta._

object Main {
	def getIntValue(node: Term): Int = {
		node match {
			case x: Lit.Int 	=> x.value
			case _				=> throw new Exception("wrong type!")
		}
	}
	def getBoolValue(node: Term): Boolean = {
		node match {
			case x: Lit.Boolean => x.value
			case _				=> throw new Exception("wrong type!")
		}
	}
	def step(node: Term): Term = {
		node match {
			case x: Lit.Int => x
    		case x: Lit.Boolean => x
			case x: Term.ApplyInfix => {
				var left:Term = x.lhs
				var right:Term = x.args(0)
				if (left.isInstanceOf[Term.ApplyInfix]) {
					return x.copy(lhs = step(left))
				}
				else if (right.isInstanceOf[Term.ApplyInfix]) {
					return x.copy(args = List(step(right)))
				}
				else if (left.isInstanceOf[Lit.Int] && right.isInstanceOf[Lit.Int]) {
					var leftV = getIntValue(left)
					var rightV = getIntValue(right)
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
				}
			}
    		case x: Term.If => {
    			if (x.cond.isInstanceOf[Term.ApplyInfix]) {
    				return x.copy(cond = step(x.cond))
    			} else if (x.cond.isInstanceOf[Lit.Boolean]) {
    				if (getBoolValue(x.cond) == true) {
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
    		case x: Lit.Int => "Int"
    		case x: Lit.Boolean => "Boolean"
			case x: Term.ApplyInfix => {
				if (x.op.value == "+" || x.op.value == "*" || x.op.value == "<=") {
					var left:Term = x.lhs
					var right:Term = x.args(0)
					if (getType(left) == "Int" && getType(right) == "Int") {
						if (x.op.value == "<=") {
							return "Boolean"
						} else {
							return "Int"
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
    			if (getType(x.cond) == "Boolean") {
    				var a2 = getType(x.thenp)
    				var a3 = getType(x.elsep)
    				if (a2 == a3) {
    					return a2
    				} else {
    					return "Wrong Type(thenp != elsep)"
    				}
    			} else {
    				return "Wrong Type in condition"
    			}
    		}
    		case _          => {
    			println("dunno")
    			???
    		}
    	}
    }

	def main(args: Array[String]): Unit = {
		var testcase = Array(
			"true",
			"false",
			"1+2+3",
			"if (1<=1) 2+3 else 4+5",
			"if (true) 2+3 else 4+5",
			"if (1<=1) true else false"
		)
		testcase.foreach(program => {
			var tree = program.parse[Term].get
			println("-----------------------")
			println("type:" + getType(tree))
			while (!(tree.isInstanceOf[Lit.Int] || tree.isInstanceOf[Lit.Boolean])) {
				println(tree.syntax)
				tree = step(tree)
			}
			println("result:" + tree.syntax)
		})
	}
}