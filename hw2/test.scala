import scala.meta._

import semantic._
import typecheck._

object Main {
    def test_step(input: Term, answer: String): Unit = {
        println("---------------------------------------")
        println("Step Check: " + input.syntax)
        var result = step(input).syntax
        println("Result: " + result)
        println(if (result.replaceAll("\\s", "") == answer.replaceAll("\\s", "")) "PASS***" else "FAIL!!!")
    }
    def test_type(input: Term, answer: String): Unit = {
        println("---------------------------------------")
        println("Type Check: " + input.syntax)
        var result = getType(input, List()).syntax
        println("Result: " + result)
        println(if (result.replaceAll("\\s", "") == answer.replaceAll("\\s", "")) "PASS***" else "FAIL!!!")
    }
    def main(args: Array[String]): Unit = {
        // some basic lambda
        var int_int_v = q"(x:Int) => x+1"
        var int_int2_v = q"(x:Int) => (y:Int) => x+y"
        var int_int12_v = q"(x:Int) => (y:Int) => (z:Int) => x+y+z"
        var int2_int_v = q"(x:Int=>Int) => x(1)"
        var int2_int2_v = q"(x:Int=>Int) => ((y:Int) => y+1)"

        var int_int21_v = Term.Function(List(param"x:Int"), int2_int_v)
        var int_int22_v = Term.Function(List(param"x:Int"), int2_int2_v)
        var int_int_e = Term.Apply(int2_int2_v, List(int_int_v))
        var int_int2_e = Term.Apply(int_int12_v, List(q"1"))
        var int2_int_e = Term.Apply(int_int21_v, List(q"1"))
        var int2_int2_e = Term.Apply(int_int22_v, List(q"1"))

        // ----------------------- Test Type --------------------------
        // int
        test_type(q"1", "Int")
        // v + v
        test_type(q"1+2", "Int")
        // v + e2
        test_type(q"1+(2+3)", "Int")
        // e1 + e2
        test_type(q"(1+2)+(3+4)", "Int")
        
        // lambda
        test_type(int_int_v, "Int => Int")
        test_type(int_int2_v, "Int => Int => Int")
        test_type(int2_int_v, "(Int => Int) => Int")
        test_type(int2_int2_v, "(Int => Int) => Int => Int")

        // v v
        test_type(Term.Apply(int_int_v, List(q"1")), "Int")
        test_type(Term.Apply(int_int2_v, List(q"1")), "Int=>Int")
        test_type(Term.Apply(int2_int_v, List(int_int_v)), "Int")
        test_type(Term.Apply(int2_int2_v, List(int_int_v)), "Int=>Int")
        
        // v e2
        test_type(Term.Apply(int_int_v, List(q"1+2")), "Int")
        test_type(Term.Apply(int_int2_v, List(q"1+2")), "Int=>Int")
        test_type(Term.Apply(int2_int_v, List(int_int_e)), "Int")
        test_type(Term.Apply(int2_int2_v, List(int_int_e)), "Int=>Int")

        // e1 e2
        test_type(Term.Apply(int_int_e, List(q"1+2")), "Int")
        test_type(Term.Apply(int_int2_e, List(q"1+2")), "Int=>Int")
        test_type(Term.Apply(int2_int_e, List(int_int_e)), "Int")
        test_type(Term.Apply(int2_int2_e, List(int_int_e)), "Int=>Int")

        // --------------------- Test Step --------------------------
        // int
        test_step(q"1", "1")
        // v + v
        test_step(q"1+2", "3")
        // v + e2
        test_step(q"1+(2+3)", "1+5")
        // e1 + e2
        test_step(q"(1+2)+(3+4)", "3+(3+4)")
        // lambda
        test_step(q"(x:Int) => x+1", "(x:Int) => x+1")
        // v v
        test_step(q"((x:Int) => x+1)(1)", "1+1")
        test_step(q"((x: Int => Int) => x)((x:Int) => x+1)", "(x:Int) => x+1")
        // v e2
        test_step(q"((x:Int) => x+1)(1+2)", "((x:Int) => x+1)(3)")
        // e1 e2
        test_step(q"(((x: Int) => (y: Int) => y + x)(1))(1 + 2)", "((y: Int) => y + 1)(1 + 2)")
        test_step(q"((x: Int) => (y:Int) => y+x)(3)", "(y:Int) => y+3")
    }
}
