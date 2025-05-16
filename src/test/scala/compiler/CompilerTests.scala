package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import lang.parse
import compiler.CommonOperators.*

import compiler.{LIntReader, LIntInterpreter}

class CompilerTests extends FunSuite {

  // ───────────────────────────────────────────────────────────────
  // ─── Basic Sanity Check ────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────
  test("1 + 1 = 2") {
    assertEquals(1 + 1, 2)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── S-Expression Fixtures ─────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

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

  val onePlusOneAst =
    LVar.BinaryOp(BinaryOperator.Add, LVar.Constant(1), LVar.Constant(1))

  // ───────────────────────────────────────────────────────────────
  // ─── LIntReader Tests ──────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  def testReadExpression(
      name: String,
      input: SExp,
      expected: LVar.Expr
  ): Unit = {
    test(s"LIntReader.fromSExpToExpr - $name") {
      assertEquals(LIntReader.fromSExpToExpr(input), expected)
    }
  }

  testReadExpression("binary: 1 + 1", sexpOnePlusOne, onePlusOneAst)

  testReadExpression(
    "binary: 4 - 2",
    sexpFourMinusTwo,
    LVar.BinaryOp(BinaryOperator.Sub, LVar.Constant(4), LVar.Constant(2))
  )

  testReadExpression(
    "unary: -8",
    sexpMinusEight,
    LVar.UnaryOp(UnaryOperator.USub, LVar.Constant(8))
  )

  testReadExpression(
    "call: input_int",
    sexpInputIntCall,
    LVar.Call(LVar.Identifier("input_int"), Nil)
  )

  test("LIntReader.readStatement - ExprStmt") {
    assertEquals(
      LIntReader.fromSExpToStmt(
        Node(List(Node(List(Symbol("Expr"), sexpOnePlusOne))))
      ),
      LVar.ExprStmt(onePlusOneAst)
    )
  }

  test("LIntReader.readModule - single print expr") {
    assertEquals(
      LIntReader.fromSExpToModule(
        Node(
          List(
            Symbol("Module"),
            Node(List(Node(List(Symbol("Expr"), sexpOnePlusOne))))
          )
        )
      ),
      LVar.Module(List(LVar.ExprStmt(onePlusOneAst)))
    )
  }

  // ───────────────────────────────────────────────────────────────
  // ─── LVarReader Tests ──────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("LVarReader.fromSExpToExpr") {
    assertEquals(
      LVarReader.fromSExpToExpr(Node(List(Symbol("Variable"), Symbol("x")))),
      LVar.Variable(LVar.Identifier("x"))
    )
  }

  test("LVarReader.fromSExpToStmt") {
    assertEquals(
      LVarReader.fromSExpToStmt(
        Node(
          List(
            Node(
              List(
                Symbol("Assign"),
                Symbol("x"),
                Node(List(Symbol("Constant"), Number(1)))
              )
            )
          )
        )
      ),
      LVar.AssignStmt(LVar.Identifier("x"), LVar.Constant(1))
    )
  }

  test("LVarReader.fromSExptoModule") {
    assertEquals(
      LVarReader.fromSExpToModule(
        Node(
          List(
            Symbol("Module"),
            Node(
              List(
                Node(
                  List(
                    Symbol("Assign"),
                    Symbol("x"),
                    Node(List(Symbol("Constant"), Number(1)))
                  )
                )
              )
            )
          )
        )
      ),
      LVar.Module(List(LVar.AssignStmt(LVar.Identifier("x"), LVar.Constant(1))))
    )
  }

  // ───────────────────────────────────────────────────────────────
  // ─── LIntInterpreter Tests ─────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("LIntInterpreter.evalExpr - binary: 1 + 1") {
    assertEquals(
      LIntInterpreter.evalExpr(LIntReader.fromSExpToExpr(sexpOnePlusOne)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - binary: 4 - 2") {
    assertEquals(
      LIntInterpreter.evalExpr(LIntReader.fromSExpToExpr(sexpFourMinusTwo)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - unary: -8") {
    assertEquals(
      LIntInterpreter.evalExpr(LIntReader.fromSExpToExpr(sexpMinusEight)),
      -8L
    )
  }

  test("LIntInterpreter.evalModule - print statement") {
    val input = "print(1 + 2)"
    val expectedOutput = "3\n"

    val outputStream = new java.io.ByteArrayOutputStream()
    Console.withOut(outputStream) {
      LIntInterpreter.evalModule(LIntReader.fromSExpToModule(parse(input)))
    }

    val actualOutput = outputStream.toString
    assertEquals(actualOutput, expectedOutput)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── Partial Evaluation Tests ──────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("LIntInterpreter.partialEvalExpr - constant folding") {
    val input =
      LVar.BinaryOp(BinaryOperator.Add, LVar.Constant(1), LVar.Constant(2))
    val expected = LVar.Constant(3)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - nested folding") {
    val input = LVar.BinaryOp(
      BinaryOperator.Sub,
      LVar.BinaryOp(BinaryOperator.Add, LVar.Constant(2), LVar.Constant(3)),
      LVar.Constant(1)
    )
    val expected = LVar.Constant(4)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - symbolic parts remain") {
    val input = LVar.BinaryOp(
      BinaryOperator.Add,
      LVar.BinaryOp(BinaryOperator.Add, LVar.Constant(1), LVar.Constant(2)),
      LVar.Call(LVar.Identifier("input_int"), List.empty)
    )
    val expected =
      LVar.BinaryOp(
        BinaryOperator.Add,
        LVar.Constant(3),
        LVar.Call(LVar.Identifier("input_int"), List.empty)
      )
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalStatement - partially simplified print") {
    val input =
      LVar.PrintStmt(
        LVar.BinaryOp(BinaryOperator.Sub, LVar.Constant(5), LVar.Constant(3))
      )
    val expected = LVar.PrintStmt(LVar.Constant(2))
    assertEquals(LIntInterpreter.partialEvalStatement(input), expected)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── LMonVar Tests ─────────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("simplify to LMonVar") {
    val expected = LMonVar.Module(
      List(
        LMonVar.AssignStmt(
          LMonVar.Identifier("$tmp$_1"),
          LMonVar.BinaryOp(
            BinaryOperator.Add,
            LMonVar.Constant(2),
            LMonVar.Constant(2)
          )
        ),
        LMonVar.AssignStmt(
          LMonVar.Identifier("$tmp$_2"),
          LMonVar.BinaryOp(
            BinaryOperator.Add,
            LMonVar.Constant(1),
            LMonVar.Variable(LMonVar.Identifier("$tmp$_1"))
          )
        ),
        LMonVar.AssignStmt(
          LMonVar.Identifier("$tmp$_3"),
          LMonVar.BinaryOp(
            BinaryOperator.Sub,
            LMonVar.Variable(LMonVar.Identifier("$tmp$_2")),
            LMonVar.Constant(8)
          )
        ),
        LMonVar.ExprStmt(
          LMonVar.AtomExpr(
            LMonVar.Variable(LMonVar.Identifier("$tmp$_3"))
          )
        )
      )
    )
    val program = "1 + (2+2) -8"

    assertEquals(
      simplifyModule(LVarReader.fromSExpToModule(parse(program))),
      expected
    )
  }

  // ───────────────────────────────────────────────────────────────
  // ─── End-to-End Tests ──────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("End-to-End: object language -> AST - print(1 + 1)") {
    val printCall = "print(1+1)"

    val expected = LVar.Module(
      List(
        LVar.PrintStmt(
          LVar.BinaryOp(BinaryOperator.Add, LVar.Constant(1), LVar.Constant(1))
        )
      )
    )
    assertEquals(LIntReader.fromSExpToModule(parse(printCall)), expected)
  }

  test("End-to-End: object language -> eval result") {
    val program = "1 + (4 - 2) - (-8)"
    assertEquals(
      LIntInterpreter.evalModule(LIntReader.fromSExpToModule(parse(program))),
      11L
    )
  }
}
