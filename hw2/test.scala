import scala.meta._

import semantic._
import typecheck._

object Main {
    def test_step(input: String, answer: String): Unit = {
        println("---------------------------------------")
        println("Step Check: " + input)
        var result = step(input.parse[Term].get).syntax
        println("Result: " + result)
        println(if (result.replaceAll("\\s", "") == answer.replaceAll("\\s", "")) "PASS***" else "FAIL!!!")
    }
    def test_type(input: String, answer: String): Unit = {
        println("---------------------------------------")
        println("Type Check: " + input)
        var result = getType(input.parse[Term].get, List()).syntax
        println("Result: " + result)
        println(if (result.replaceAll("\\s", "") == answer.replaceAll("\\s", "")) "PASS***" else "FAIL!!!")
    }
    def main(args: Array[String]): Unit = {
        // some basic lambda
        var int_int_v = "(x:Int) => x+1".parse[Term].get
        var int_int2_v = "(x:Int) => (y:Int) => x+y".parse[Term].get
        var int_int12_v = "(x:Int) => (y:Int) => (z:Int) => x+y+z".parse[Term].get
        var int2_int_v = "(x:Int=>Int) => x(1)".parse[Term].get
        var int2_int2_v = "(x:Int=>Int) => ((y:Int) => y+1)".parse[Term].get

        var int_int21_v = Term.Function(List(param"x:Int"), int2_int_v)
        var int_int22_v = Term.Function(List(param"x:Int"), int2_int2_v)
        // var int_int_e = Term.Apply(int2_int2_v, List(int_int_v))
        var int_int_e = "(((x:Int) => ((y:Int) => y+x))(1))".parse[Term].get
        var int_int2_e = Term.Apply(int_int12_v, List(q"1"))
        var int2_int_e = Term.Apply(int_int21_v, List(q"1"))
        var int2_int2_e = Term.Apply(int_int22_v, List(q"1"))

        // ----------------------- Test Type --------------------------
        // int
        test_type("1", "Int")
        // v + v
        test_type("1+2", "Int")
        // v + e2
        test_type("1+(2+3)", "Int")
        // e1 + e2
        test_type("(1+2)+(3+4)", "Int")
        
        // lambda
        test_type(int_int_v.syntax, "Int => Int")
        test_type(int_int2_v.syntax, "Int => Int => Int")
        test_type(int2_int_v.syntax, "(Int => Int) => Int")
        test_type(int2_int2_v.syntax, "(Int => Int) => Int => Int")

        // v v
        test_type(Term.Apply(int_int_v, List(q"1")).syntax, "Int")
        test_type(Term.Apply(int_int2_v, List(q"1")).syntax, "Int=>Int")
        // test_type(Term.Apply(int2_int_v, List(int_int_v)).syntax, "Int")
        test_type("((x: Int => Int) => x(1)) ( (x: Int) => x + 1 )", "Int")
        // test_type(Term.Apply(int2_int2_v, List(int_int_v)).syntax, "Int=>Int")
        
        // v e2
        test_type(Term.Apply(int_int_v, List(q"1+2")).syntax, "Int")
        test_type(Term.Apply(int_int2_v, List(q"1+2")).syntax, "Int=>Int")
        // test_type(Term.Apply(int2_int_v, List(int_int_e)).syntax, "Int")
        // test_type(Term.Apply(int2_int2_v, List(int_int_e)).syntax, "Int=>Int")

        // e1 e2
        test_type(Term.Apply(int_int_e, List(q"1+2")).syntax, "Int")
        test_type(Term.Apply(int_int2_e, List(q"1+2")).syntax, "Int=>Int")
        // test_type(Term.Apply(int2_int_e, List(int_int_e)).syntax, "Int")
        // test_type(Term.Apply(int2_int2_e, List(int_int_e)).syntax, "Int=>Int")

        // --------------------- Test Step --------------------------
        // int
        test_step("1", "1")
        // v + v
        test_step("1+2", "3")
        // v + e2
        test_step("1+(2+3)", "1+5")
        // e1 + e2
        test_step("(1+2)+(3+4)", "3+(3+4)")
        // lambda
        test_step("(x:Int) => x+1", "(x:Int) => x+1")
        // v v
        test_step("((x:Int) => x+1)(1)", "1+1")
        // v e2
        test_step("((x:Int) => x+1)(1+2)", "((x:Int) => x+1)(3)")
        // e1 e2
        test_step("(((x: Int) => (y: Int) => y + x)(1))(1 + 2)", "((y: Int) => y + 1)(1 + 2)")

    }
}
