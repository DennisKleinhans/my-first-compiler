package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import lang.parse
import compiler.CommonNodes.*

import compiler.{LIntInterpreter}
import munit.Tag
import java.util.jar.Attributes.Name

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
      Node(Nil)
    )
  )

  val onePlusOneAst =
    LVar.BinaryOp(BinaryOperator.Add, LVar.Constant(1), LVar.Constant(1))

  // ───────────────────────────────────────────────────────────────
  // ─── Expression Tests ─────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  def testReadExpression(
      name: String,
      input: SExp,
      expected: LVar.Expr
  ): Unit = {
    test(s"fromSExpToExpr - $name") {
      assertEquals(LVarReader.fromSExpToExpr(input), expected)
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
    LVar.Call(Identifier("input_int"), Nil)
  )

  // ───────────────────────────────────────────────────────────────
  // ─── Statement Tests ──────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("fromSExpToStmt - ExprStmt") {
    val sexp = Node(
      List(
        Symbol("Expr"),
        sexpOnePlusOne
      )
    )
    assertEquals(
      LVarReader.fromSExpToStmt(sexp),
      LVar.ExprStmt(onePlusOneAst)
    )
  }

  test("fromSExpToStmt - AssignStmt") {
    val sexp = Node(
      List(
        Symbol("Assign"),
        Symbol("x"),
        Node(List(Symbol("Constant"), Number(5)))
      )
    )
    assertEquals(
      LVarReader.fromSExpToStmt(sexp),
      LVar.AssignStmt(Identifier("x"), LVar.Constant(5))
    )
  }

  test("fromSExpToStmt - PrintStmt") {
    val sexp = Node(
      List(
        Symbol("Expr"),
        Node(
          List(
            Symbol("Call"),
            Node(List(Symbol("Variable"), Symbol("print"))),
            Node(
              List(
                Node(
                  List(
                    Symbol("Binary"),
                    Symbol("Add"),
                    Node(List(Symbol("Variable"), Symbol("x"))),
                    Node(List(Symbol("Constant"), Number(2)))
                  )
                )
              )
            )
          )
        )
      )
    )
    assertEquals(
      LVarReader.fromSExpToStmt(sexp),
      LVar.PrintStmt(
        LVar.BinaryOp(
          BinaryOperator.Add,
          LVar.Variable(Identifier("x")),
          LVar.Constant(2)
        )
      )
    )
  }

  // ───────────────────────────────────────────────────────────────
  // ─── LIntInterpreter Tests ─────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("LIntInterpreter.evalExpr - binary: 1 + 1") {
    assertEquals(
      LIntInterpreter.evalExpr(LVarReader.fromSExpToExpr(sexpOnePlusOne)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - binary: 4 - 2") {
    assertEquals(
      LIntInterpreter.evalExpr(LVarReader.fromSExpToExpr(sexpFourMinusTwo)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - unary: -8") {
    assertEquals(
      LIntInterpreter.evalExpr(LVarReader.fromSExpToExpr(sexpMinusEight)),
      -8L
    )
  }

  test("LIntInterpreter.evalModule - print statement") {
    val input = "print(1 + 2)"
    val expectedOutput = "3\n"

    val outputStream = new java.io.ByteArrayOutputStream()
    Console.withOut(outputStream) {
      LIntInterpreter.evalModule(LVarReader.fromSExpToModule(parse(input)))
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
      LVar.Call(Identifier("input_int"), List.empty)
    )
    val expected =
      LVar.BinaryOp(
        BinaryOperator.Add,
        LVar.Constant(3),
        LVar.Call(Identifier("input_int"), List.empty)
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
    val program = "1 + (2+2) -8"
    val result = simplifyModule(
      LVarReader.fromSExpToModule(parse(program))
    )

    val expected = LMonVar.Module(
      List(
        LMonVar.AssignStmt(
          Identifier("$tmp$_1"),
          LMonVar.BinaryOp(
            BinaryOperator.Add,
            LMonVar.Constant(2),
            LMonVar.Constant(2)
          )
        ),
        LMonVar.AssignStmt(
          Identifier("$tmp$_2"),
          LMonVar.BinaryOp(
            BinaryOperator.Add,
            LMonVar.Constant(1),
            LMonVar.Variable(Identifier("$tmp$_1"))
          )
        ),
        LMonVar.AssignStmt(
          Identifier("$tmp$_3"),
          LMonVar.BinaryOp(
            BinaryOperator.Sub,
            LMonVar.Variable(Identifier("$tmp$_2")),
            LMonVar.Constant(8)
          )
        ),
        LMonVar.ExprStmt(
          LMonVar.AtomExpr(
            LMonVar.Variable(Identifier("$tmp$_3"))
          )
        )
      )
    )

    assertEquals(result, expected)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── selectInstructions Tests───────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("selectInstructions - simple Add (1+2)") {
    val programm = "1+2"
    val result = selectInstructions(
      simplifyModule(
        LVarReader.fromSExpToModule((parse(programm)))
      )
    )
    val expected = List(
      x86Var.MovQ(x86Var.Immediate(1), x86Var.Variable(Identifier("$tmp$_1"))),
      x86Var.AddQ(x86Var.Immediate(2), x86Var.Variable(Identifier("$tmp$_1")))
    )
    assertEquals(result, expected)
  }

  test("selectInstructions - read_int()") {
    val programm = "1 + read_int()"
    val result = selectInstructions(
      simplifyModule(
        LVarReader.fromSExpToModule((parse(programm)))
      )
    )
    val expected = List(
      x86Var.CallQ("read_int", 0),
      x86Var.MovQ(
        x86.Reg.Rax,
        x86Var.Variable(Identifier("$tmp$_1"))
      ),
      x86Var.MovQ(x86Var.Immediate(1), x86Var.Variable(Identifier("$tmp$_2"))),
      x86Var.AddQ(
        x86Var.Variable(Identifier("$tmp$_1")),
        x86Var.Variable(Identifier("$tmp$_2"))
      )
    )
  }

  test("selectInstructions - complex arithmetic ()") {
    val programm = "1+(3+4)-8"
    val expected = List(
      x86Var.MovQ(x86Var.Immediate(3), x86Var.Variable(Identifier("$tmp$_1"))),
      x86Var.AddQ(x86Var.Immediate(4), x86Var.Variable(Identifier("$tmp$_1"))),
      x86Var.MovQ(x86Var.Immediate(1), x86Var.Variable(Identifier("$tmp$_2"))),
      x86Var.AddQ(
        x86Var.Variable(Identifier("$tmp$_1")),
        x86Var.Variable(Identifier("$tmp$_2"))
      ),
      x86Var.MovQ(
        x86Var.Variable(Identifier("$tmp$_2")),
        x86Var.Variable(Identifier("$tmp$_3"))
      ),
      x86Var.SubQ(x86Var.Immediate(8), x86Var.Variable(Identifier("$tmp$_3")))
    )
    val result = selectInstructions(
      simplifyModule(
        LVarReader.fromSExpToModule(parse(programm))
      )
    )
    assertEquals(result, expected)
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
    assertEquals(LVarReader.fromSExpToModule(parse(printCall)), expected)
  }

  test("End-to-End: object language -> eval result") {
    val program = "1 + (4 - 2) - (-8)"
    assertEquals(
      LIntInterpreter.evalModule(LVarReader.fromSExpToModule(parse(program))),
      11L
    )
  }
}
