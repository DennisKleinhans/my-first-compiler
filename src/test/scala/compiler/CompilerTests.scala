package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import compiler.Expr.*
import compiler.Stmt.*
import lang.parse
import scala.collection.immutable.Stream.Cons

class CompilerTests extends FunSuite {
  test("1 + 1 = 2") {
    assertEquals(1 + 1, 2)
  }

  // helper S-Exps
  val sexpOnePlusOne = Node(
    List(
      Symbol("Binary"),
      Symbol("Add"),
      Node(List(Symbol("Constant"), Number(1))),
      Node(List(Symbol("Constant"), Number(1)))
    )
  )

  val sexpFourMinusTwo = Node(
    List(
      Symbol("Binary"),
      Symbol("Sub"),
      Node(List(Symbol("Constant"), Number(4))),
      Node(List(Symbol("Constant"), Number(2)))
    )
  )

  val sexpMinusEight = Node(
    List(
      Symbol("Unary"),
      Symbol("Neg"),
      Node(List(Symbol("Constant"), Number(8)))
    )
  )

  val sexpInputIntCall = Node(
    List(
      Symbol("Call"),
      Node(List(Symbol("Variable"), Symbol("input_int"))),
      Node(List())
    )
  )

  val onePlusOneAst = BinaryOp(BinaryOperator.Add, Constant(1), Constant(1))

  // helper functions for redundnat test structure
  def testReadExpression(name: String, input: SExp, expected: Expr): Unit = {
    test(s"readExpression - $name") {
      assertEquals(readExpression(input), expected)
    }
  }

  // readExpression tests
  testReadExpression(
    "binary: 1 + 1",
    sexpOnePlusOne,
    onePlusOneAst
  )
  testReadExpression(
    "binary: 4 - 2",
    sexpFourMinusTwo,
    BinaryOp(BinaryOperator.Sub, Constant(4), Constant(2))
  )
  testReadExpression(
    "unary: -8",
    sexpMinusEight,
    UnaryOp(UnaryOperator.USub, Constant(8))
  )
  testReadExpression(
    "call: input_int",
    sexpInputIntCall,
    Call("input_int", Nil)
  )

  // readStatement test
  test("readStatement - binary: 1 + 1") {
    assertEquals(
      readStatement(Node(List(Node(List(Symbol("Expr"), sexpOnePlusOne))))),
      ExprStmt(onePlusOneAst)
    )
  }

  // readModule test
  test("readModule - binary: 1 + 1") {
    assertEquals(
      readModule(
        Node(
          List(
            Symbol("Module"),
            Node(List(Node(List(Symbol("Expr"), sexpOnePlusOne))))
          )
        )
      ),
      Module(List(ExprStmt(onePlusOneAst)))
    )
  }

  // interpreter tests
  test("evalExpr - binary: 1 + 1") {
    assertEquals(evalExpr(readExpression(sexpOnePlusOne)), 2L)
  }

  test("evalExpr - binary: 4 - 2") {
    assertEquals(evalExpr(readExpression(sexpFourMinusTwo)), 2L)
  }

  test("evalExpr - unary: -8") {
    assertEquals(evalExpr(readExpression(sexpMinusEight)), -8L)
  }

  test("evalModule - print statement") {
    val input = "print(1 + 2)"
    val expectedOutput = "3\n"

    val outputStream = new java.io.ByteArrayOutputStream()
    Console.withOut(outputStream) {
      evalModule(readModule(parse(input)))
    }

    val actualOutput = outputStream.toString
    assertEquals(actualOutput, expectedOutput)
  }

  // partial evaluator tests
  test("partialEvalExpr - constant folding") {
    val input = BinaryOp(BinaryOperator.Add, Constant(1), Constant(2))
    val expected = Constant(3)
    assertEquals(partialEvalExpr(input), expected)
  }

  test("partialEvalExpr - nested folding") {
    val input = BinaryOp(
      BinaryOperator.Sub,
      BinaryOp(BinaryOperator.Add, Constant(2), Constant(3)),
      Constant(1)
    )
    val expected = Constant(4)
    assertEquals(partialEvalExpr(input), expected)
  }

  test("partialEvalExpr - symbolic parts remain") {
    val input = BinaryOp(BinaryOperator.Add, BinaryOp(BinaryOperator.Add, Constant(1), Constant(2)), Call("input_int", List.empty))
    val expected = BinaryOp(BinaryOperator.Add, Constant(3), Call("input_int", List.empty))
    assertEquals(partialEvalExpr(input), expected)
  }

  test("partialEvalStatement - print is preserved and partially simplified") {
    val input = PrintStmt(BinaryOp(BinaryOperator.Sub, Constant(5), Constant(3)))
    val expected = PrintStmt(Constant(2))
    assertEquals(partialEvalStatement(input), expected)
  }

  // end-to-end tests
  test("End-to-End - object lang -> Expr: print(1 + 1)") {
    val printCall = "print(1+1)"

    val expected = Module(
      List(
        PrintStmt(
          BinaryOp(BinaryOperator.Add, Constant(1), Constant(1))
        )
      )
    )
    assertEquals(readModule(parse(printCall)), expected)
  }

  test("End-to-End - object lang -> Long") {
    val programm = "1 + (4 - 2) - (-8)"
    assertEquals(evalModule(readModule(parse(programm))), 11L)
  }
}
