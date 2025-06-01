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
      Node(List(Symbol("Variable"), Symbol("read_int"))),
      Node(Nil)
    )
  )

  val onePlusOneAst =
    LIf.BinaryNumericOp(
      BinaryNumericOperator.Add,
      LIf.Constant(1),
      LIf.Constant(1)
    )

  // ───────────────────────────────────────────────────────────────
  // ─── Expression Tests ─────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  def testReadExpression(
      name: String,
      input: SExp,
      expected: LIf.Expr
  ): Unit = {
    test(s"fromSExpToExpr - $name") {
      assertEquals(LVarReader.fromSExpToExpr(input), expected)
    }
  }

  testReadExpression("binary: 1 + 1", sexpOnePlusOne, onePlusOneAst)
  testReadExpression(
    "binary: 4 - 2",
    sexpFourMinusTwo,
    LIf.BinaryNumericOp(
      BinaryNumericOperator.Sub,
      LIf.Constant(4),
      LIf.Constant(2)
    )
  )
  testReadExpression(
    "unary: -8",
    sexpMinusEight,
    LIf.UnaryNumericOp(UnaryNumericOperator.USub, LIf.Constant(8))
  )
  testReadExpression(
    "call: read_int",
    sexpInputIntCall,
    LIf.ReadIntCall
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
      LIf.ExprStmt(onePlusOneAst)
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
      LIf.AssignStmt(Identifier("x"), LIf.Constant(5))
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
      LIf.PrintStmt(
        LIf.BinaryNumericOp(
          BinaryNumericOperator.Add,
          LIf.Variable(Identifier("x")),
          LIf.Constant(2)
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
      LIf.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LIf.Constant(1),
        LIf.Constant(2)
      )
    val expected = LIf.Constant(3)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - nested folding") {
    val input = LIf.BinaryNumericOp(
      BinaryNumericOperator.Sub,
      LIf.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LIf.Constant(2),
        LIf.Constant(3)
      ),
      LIf.Constant(1)
    )
    val expected = LIf.Constant(4)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - symbolic parts remain") {
    val input = LIf.BinaryNumericOp(
      BinaryNumericOperator.Add,
      LIf.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LIf.Constant(1),
        LIf.Constant(2)
      ),
      LIf.ReadIntCall
    )
    val expected =
      LIf.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LIf.Constant(3),
        LIf.ReadIntCall
      )
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalStatement - partially simplified print") {
    val input =
      LIf.PrintStmt(
        LIf.BinaryNumericOp(
          BinaryNumericOperator.Sub,
          LIf.Constant(5),
          LIf.Constant(3)
        )
      )
    val expected = LIf.PrintStmt(LIf.Constant(2))
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

    val expected = LMonIf.Module(
      List(
        LMonIf.AssignStmt(
          Identifier("$tmp$_1"),
          LMonIf.BinaryNumericOp(
            BinaryNumericOperator.Add,
            LMonIf.Constant(2),
            LMonIf.Constant(2)
          )
        ),
        LMonIf.AssignStmt(
          Identifier("$tmp$_2"),
          LMonIf.BinaryNumericOp(
            BinaryNumericOperator.Add,
            LMonIf.Constant(1),
            LMonIf.Variable(Identifier("$tmp$_1"))
          )
        ),
        LMonIf.AssignStmt(
          Identifier("$tmp$_3"),
          LMonIf.BinaryNumericOp(
            BinaryNumericOperator.Sub,
            LMonIf.Variable(Identifier("$tmp$_2")),
            LMonIf.Constant(8)
          )
        ),
        LMonIf.ExprStmt(
          LMonIf.AtomExpr(
            LMonIf.Variable(Identifier("$tmp$_3"))
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
      x86VarIf
        .MovQ(x86VarIf.Immediate(1), x86VarIf.Variable(Identifier("$tmp$_1"))),
      x86VarIf.AddQ(
        x86VarIf.Immediate(2),
        x86VarIf.Variable(Identifier("$tmp$_1"))
      )
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
      x86VarIf.CallQ("read_int", 0),
      x86VarIf.MovQ(
        x86.Reg.Rax,
        x86VarIf.Variable(Identifier("$tmp$_1"))
      ),
      x86VarIf
        .MovQ(x86VarIf.Immediate(1), x86VarIf.Variable(Identifier("$tmp$_2"))),
      x86VarIf.AddQ(
        x86VarIf.Variable(Identifier("$tmp$_1")),
        x86VarIf.Variable(Identifier("$tmp$_2"))
      )
    )
  }

  test("selectInstructions - complex arithmetic") {
    val programm = "1+(3+4)-8"
    val expected = List(
      x86VarIf
        .MovQ(x86VarIf.Immediate(3), x86VarIf.Variable(Identifier("$tmp$_1"))),
      x86VarIf
        .AddQ(x86VarIf.Immediate(4), x86VarIf.Variable(Identifier("$tmp$_1"))),
      x86VarIf
        .MovQ(x86VarIf.Immediate(1), x86VarIf.Variable(Identifier("$tmp$_2"))),
      x86VarIf.AddQ(
        x86VarIf.Variable(Identifier("$tmp$_1")),
        x86VarIf.Variable(Identifier("$tmp$_2"))
      ),
      x86VarIf.MovQ(
        x86VarIf.Variable(Identifier("$tmp$_2")),
        x86VarIf.Variable(Identifier("$tmp$_3"))
      ),
      x86VarIf.SubQ(
        x86VarIf.Immediate(8),
        x86VarIf.Variable(Identifier("$tmp$_3"))
      )
    )
    val result = selectInstructions(
      simplifyModule(
        LVarReader.fromSExpToModule(parse(programm))
      )
    )
    assertEquals(result, expected)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── shrink Tests ──────────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  val andExpr = LIf.BinaryLogicOp(
    BinaryLogicOperator.And,
    LIf.Constant(1),
    LIf.Constant(2)
  )

  val orExpr = LIf.BinaryLogicOp(
    BinaryLogicOperator.Or,
    LIf.Constant(1),
    LIf.Constant(2)
  )

  val nestedAndInPrint = LIf.PrintStmt(andExpr)
  val nestedAndInAssign = LIf.AssignStmt(Identifier("x"), andExpr)

  test("shrinkExpr - and") {
    val expected =
      LIf.IfExpr(LIf.Constant(1), LIf.Constant(2), LIf.ConstantBool(false))
    assertEquals(shrinkExpr(andExpr), expected)
  }

  test("shrinkExpr - or") {
    val expected =
      LIf.IfExpr(LIf.Constant(1), LIf.ConstantBool(true), LIf.Constant(2))
    assertEquals(shrinkExpr(orExpr), expected)
  }

  test("shrinkExpr - nested expression") {
    val input =
      LIf.BinaryLogicOp(BinaryLogicOperator.And, LIf.Constant(3), orExpr)
    val expected = LIf.IfExpr(
      LIf.Constant(3),
      LIf.IfExpr(LIf.Constant(1), LIf.ConstantBool(true), LIf.Constant(2)),
      LIf.ConstantBool(false)
    )
    assertEquals(shrinkExpr(input), expected)
  }

  test("shrinkStmt - print") {
    val expected = LIf.PrintStmt(
      LIf.IfExpr(LIf.Constant(1), LIf.Constant(2), LIf.ConstantBool(false))
    )
    assertEquals(shrinkStmt(nestedAndInPrint), expected)
  }

  test("shrinkStmt - assign") {
    val expected = LIf.AssignStmt(
      Identifier("x"),
      LIf.IfExpr(LIf.Constant(1), LIf.Constant(2), LIf.ConstantBool(false))
    )
    assertEquals(shrinkStmt(nestedAndInAssign), expected)
  }

  test("shrinkModule - assing and print") {
    val input = LIf.Module(nestedAndInAssign :: nestedAndInPrint :: Nil)
    val expected = LIf.Module(
      (LIf.AssignStmt(
        Identifier("x"),
        LIf.IfExpr(LIf.Constant(1), LIf.Constant(2), LIf.ConstantBool(false))
      )) :: LIf.PrintStmt(
        LIf.IfExpr(LIf.Constant(1), LIf.Constant(2), LIf.ConstantBool(false))
      ) :: Nil
    )
    assertEquals(shrinkModule(input), expected)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── End-to-End Tests ──────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("End-to-End: object language -> AST - print(1 + 1)") {
    val printCall = "print(1+1)"

    val expected = LIf.Module(
      List(
        LIf.PrintStmt(
          LIf.BinaryNumericOp(
            BinaryNumericOperator.Add,
            LIf.Constant(1),
            LIf.Constant(1)
          )
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
