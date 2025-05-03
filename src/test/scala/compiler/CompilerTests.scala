package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import compiler.Expr.*
import compiler.Stmt.*
import lang.parse

class CompilerTests extends FunSuite {
  test("1 + 1 = 2") {
    assertEquals(1 + 1, 2)
  }

  // helper S-Exps
  val onePlusOne = Node(
    List(
      Symbol("Binary"),
      Symbol("Add"),
      Node(List(Symbol("Constant"), Number(1))),
      Node(List(Symbol("Constant"), Number(1)))
    )
  )

  val fourMinusTwo = Node(
    List(
      Symbol("Binary"),
      Symbol("Sub"),
      Node(List(Symbol("Constant"), Number(4))),
      Node(List(Symbol("Constant"), Number(2)))
    )
  )

  val minusEight = Node(
    List(
      Symbol("Unary"),
      Symbol("Neg"),
      Node(List(Symbol("Constant"), Number(8)))
    )
  )

  val inputIntCall = Node(
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
    onePlusOne,
    onePlusOneAst
  )
  testReadExpression(
    "binary: 4 - 2",
    fourMinusTwo,
    BinaryOp(BinaryOperator.Sub, Constant(4), Constant(2))
  )
  testReadExpression(
    "unary: -8",
    minusEight,
    UnaryOp(UnaryOperator.USub, Constant(8))
  )
  testReadExpression("call: input_int", inputIntCall, Call("input_int", Nil))

  // readStatement test
  test("readStatement - binary: 1 + 1") {
    assertEquals(
      readStatement(Node(List(Node(List(Symbol("Expr"), onePlusOne))))),
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
            Node(List(Node(List(Symbol("Expr"), onePlusOne))))
          )
        )
      ),
      Module(List(ExprStmt(onePlusOneAst)))
    )
  }

  test("End-to-End - object lang -> Expr: print(1 + 1)") {
    val printCall = "print(1+1)"

    val expected = Module(
      List(
        ExprStmt(
          Call(
            "print",
            List(
              BinaryOp(BinaryOperator.Add, Constant(1), Constant(1))
            )
          )
        )
      )
    )
    assertEquals(readModule(parse(printCall)), expected)
  }
}
