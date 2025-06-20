package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import lang.parse
import CommonNodes.*
import CommonNodes.Atom.*
import LIntInterpreter.*
import munit.Tag
import x86Var.Instr
import x86.Reg
import passes.RemoveComplexOperands.*
import passes.Shrink.*
import passes.SelectInstructions.*
import passes.AssignHomesToStack.*
import passes.ExplicateControl.*
import passes.PatchInstructions.*

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
  LCore.BinaryNumericOp(
    BinaryNumericOperator.Add,
    LCore.Constant(1),
    LCore.Constant(1)
  )

class ParserTests extends FunSuite {
  // ───────────────────────────────────────────────────────────────
  // ─── Basic Sanity Check ────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────
  test("1 + 1 = 2") {
    assertEquals(1 + 1, 2)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── Expression Tests ─────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  def testReadExpression(
      name: String,
      input: SExp,
      expected: LCore.Expr
  ): Unit = {
    test(s"fromSExpToExpr - $name") {
      assertEquals(LCoreReader.fromSExpToExpr(input), expected)
    }
  }

  testReadExpression("binary: 1 + 1", sexpOnePlusOne, onePlusOneAst)
  testReadExpression(
    "binary: 4 - 2",
    sexpFourMinusTwo,
    LCore.BinaryNumericOp(
      BinaryNumericOperator.Sub,
      LCore.Constant(4),
      LCore.Constant(2)
    )
  )
  testReadExpression(
    "unary: -8",
    sexpMinusEight,
    LCore.UnaryNumericOp(UnaryNumericOperator.USub, LCore.Constant(8))
  )
  testReadExpression(
    "call: read_int",
    sexpInputIntCall,
    LCore.ReadIntCall
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
      LCoreReader.fromSExpToStmt(sexp),
      LCore.ExprStmt(onePlusOneAst)
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
      LCoreReader.fromSExpToStmt(sexp),
      LCore.AssignStmt(Identifier("x"), LCore.Constant(5))
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
      LCoreReader.fromSExpToStmt(sexp),
      LCore.PrintStmt(
        LCore.BinaryNumericOp(
          BinaryNumericOperator.Add,
          LCore.Variable(Identifier("x")),
          LCore.Constant(2)
        )
      )
    )
  }
}

class LIntInterpreterTests extends FunSuite {

  test("LIntInterpreter.evalExpr - binary: 1 + 1") {
    assertEquals(
      LIntInterpreter.evalExpr(LCoreReader.fromSExpToExpr(sexpOnePlusOne)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - binary: 4 - 2") {
    assertEquals(
      LIntInterpreter.evalExpr(LCoreReader.fromSExpToExpr(sexpFourMinusTwo)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - unary: -8") {
    assertEquals(
      LIntInterpreter.evalExpr(LCoreReader.fromSExpToExpr(sexpMinusEight)),
      -8L
    )
  }

  test("LIntInterpreter.evalModule - print statement") {
    val input = "print(1 + 2)"
    val expectedOutput = "3\n"

    val outputStream = new java.io.ByteArrayOutputStream()
    Console.withOut(outputStream) {
      LIntInterpreter.evalModule(LCoreReader.fromSExpToModule(parse(input)))
    }

    val actualOutput = outputStream.toString
    assertEquals(actualOutput, expectedOutput)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── Partial Evaluation Tests ──────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("LIntInterpreter.partialEvalExpr - constant folding") {
    val input =
      LCore.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LCore.Constant(1),
        LCore.Constant(2)
      )
    val expected = LCore.Constant(3)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - nested folding") {
    val input = LCore.BinaryNumericOp(
      BinaryNumericOperator.Sub,
      LCore.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LCore.Constant(2),
        LCore.Constant(3)
      ),
      LCore.Constant(1)
    )
    val expected = LCore.Constant(4)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - symbolic parts remain") {
    val input = LCore.BinaryNumericOp(
      BinaryNumericOperator.Add,
      LCore.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LCore.Constant(1),
        LCore.Constant(2)
      ),
      LCore.ReadIntCall
    )
    val expected =
      LCore.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LCore.Constant(3),
        LCore.ReadIntCall
      )
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalStatement - partially simplified print") {
    val input =
      LCore.PrintStmt(
        LCore.BinaryNumericOp(
          BinaryNumericOperator.Sub,
          LCore.Constant(5),
          LCore.Constant(3)
        )
      )
    val expected = LCore.PrintStmt(LCore.Constant(2))
    assertEquals(LIntInterpreter.partialEvalStatement(input), expected)
  }
}

class RemoveComplexOperandsTests extends FunSuite {
  // ───────────────────────────────────────────────────────────────
  // ─── LMonVar Tests ─────────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("simplify to LMonVar - nested arithmetic") {
    val program = "1 + (2+2) -8"
    val result = removeComplexOperands(
      LCoreReader.fromSExpToModule(parse(program))
    )

    val expected = LMon.Module(
      List(
        LMon.AssignStmt(
          Identifier("$tmp$_1"),
          LMon.BinaryNumericOp(
            BinaryNumericOperator.Add,
            Atom.Constant(2),
            Atom.Constant(2)
          )
        ),
        LMon.AssignStmt(
          Identifier("$tmp$_2"),
          LMon.BinaryNumericOp(
            BinaryNumericOperator.Add,
            Atom.Constant(1),
            Atom.Variable(Identifier("$tmp$_1"))
          )
        ),
        LMon.AssignStmt(
          Identifier("$tmp$_3"),
          LMon.BinaryNumericOp(
            BinaryNumericOperator.Sub,
            Atom.Variable(Identifier("$tmp$_2")),
            Atom.Constant(8)
          )
        ),
        LMon.ExprStmt(
          LMon.AtomExpr(
            Atom.Variable(Identifier("$tmp$_3"))
          )
        )
      )
    )

    assertEquals(result, expected)
  }

  test("simplify to LMonIf - IfExpr") {
    val input = "if 1==1 then 1+1 else 2+2"
    val output = removeComplexOperands(
      LCoreReader.fromSExpToModule(parse(input))
    )

    val expected = LMon.Module(
      List(
        LMon.ExprStmt(
          LMon.IfExpr(
            LMon.Compare(CompareOperator.Eq, Constant(1), Constant(1)),
            LMon.Begin(
              List(
                LMon.AssignStmt(
                  Identifier("$tmp$_1"),
                  LMon.BinaryNumericOp(
                    BinaryNumericOperator.Add,
                    Constant(1),
                    Constant(1)
                  )
                )
              ),
              LMon.AtomExpr(Variable(Identifier("$tmp$_1")))
            ),
            LMon.Begin(
              List(
                LMon.AssignStmt(
                  Identifier("$tmp$_2"),
                  LMon.BinaryNumericOp(
                    BinaryNumericOperator.Add,
                    Constant(2),
                    Constant(2)
                  )
                )
              ),
              LMon.AtomExpr(Variable(Identifier("$tmp$_2")))
            )
          )
        )
      )
    )

    assertEquals(output, expected)
  }

  test("simplify Module - IfStmt") {

    val expected = LMon.Module(
      List(
        LMon.AssignStmt(
          Identifier("$tmp$_1"),
          LMon
            .Compare(CompareOperator.Eq, Atom.Constant(1), Atom.Constant(1))
        ),
        LMon.IfStmt(
          LMon.AtomExpr(Atom.Variable(Identifier("$tmp$_1"))),
          List(
            LMon.AssignStmt(
              Identifier("$tmp$_2"),
              LMon.BinaryNumericOp(
                BinaryNumericOperator.Add,
                Atom.Constant(1),
                Atom.Constant(1)
              )
            ),
            LMon.PrintStmt(Atom.Variable(Identifier("$tmp$_2")))
          ),
          List(
            LMon.AssignStmt(
              Identifier("$tmp$_3"),
              LMon.BinaryNumericOp(
                BinaryNumericOperator.Add,
                Atom.Constant(2),
                Atom.Constant(2)
              )
            ),
            LMon.PrintStmt(Atom.Variable(Identifier("$tmp$_3")))
          )
        )
      )
    )
  }
}

class selectInstructionsTests extends FunSuite {
  test("selectInstructions - simple Add (1+2)") {
    val programm = "1+2"
    val result = selectInstructions(
      explicateControl(
        removeComplexOperands(
          LCoreReader.fromSExpToModule((parse(programm)))
        )
      )
    )
    val expected = x86Var.Program(
      Map(
        "start" -> List(
          x86Var
            .MovQ(
              x86Var.Immediate(1),
              x86Var.Variable(Identifier("$tmp$_1"))
            ),
          x86Var.AddQ(
            x86Var.Immediate(2),
            x86Var.Variable(Identifier("$tmp$_1"))
          ),
          x86Var.MovQ(x86Var.Immediate(0L), Reg.Rax),
          x86Var.Jmp("conclusion")
        )
      )
    )
    assertEquals(result, expected)
  }

  test("selectInstructions - read_int()") {
    val programm = "1 + read_int()"
    val result = selectInstructions(
      explicateControl(
        removeComplexOperands(
          LCoreReader.fromSExpToModule((parse(programm)))
        )
      )
    )
    val expected = x86Var.Program(
      Map(
        "start" -> List(
          x86Var.CallQ("read_int", 0),
          x86Var.MovQ(
            x86.Reg.Rax,
            x86Var.Variable(Identifier("$tmp$_1"))
          ),
          x86Var
            .MovQ(
              x86Var.Immediate(1),
              x86Var.Variable(Identifier("$tmp$_2"))
            ),
          x86Var.AddQ(
            x86Var.Variable(Identifier("$tmp$_1")),
            x86Var.Variable(Identifier("$tmp$_2"))
          ),
          x86Var.MovQ(x86Var.Immediate(0L), Reg.Rax),
          x86Var.Jmp("conclusion")
        )
      )
    )
  }

  test("selectInstructions - complex arithmetic") {
    val programm = "1+(3+4)-8"
    val expected = x86Var.Program(
      Map(
        "start" -> List(
          x86Var
            .MovQ(
              x86Var.Immediate(3),
              x86Var.Variable(Identifier("$tmp$_1"))
            ),
          x86Var
            .AddQ(
              x86Var.Immediate(4),
              x86Var.Variable(Identifier("$tmp$_1"))
            ),
          x86Var
            .MovQ(
              x86Var.Immediate(1),
              x86Var.Variable(Identifier("$tmp$_2"))
            ),
          x86Var.AddQ(
            x86Var.Variable(Identifier("$tmp$_1")),
            x86Var.Variable(Identifier("$tmp$_2"))
          ),
          x86Var.MovQ(
            x86Var.Variable(Identifier("$tmp$_2")),
            x86Var.Variable(Identifier("$tmp$_3"))
          ),
          x86Var.SubQ(
            x86Var.Immediate(8),
            x86Var.Variable(Identifier("$tmp$_3"))
          ),
          x86Var.MovQ(x86Var.Immediate(0L), Reg.Rax),
          x86Var.Jmp("conclusion")
        )
      )
    )
    val result = selectInstructions(
      explicateControl(
        removeComplexOperands(
          LCoreReader.fromSExpToModule(parse(programm))
        )
      )
    )
    assertEquals(result, expected)
  }
}

class ShrinkTests extends FunSuite {
  val andExpr = LCore.BinaryLogicOp(
    BinaryLogicOperator.And,
    LCore.Constant(1),
    LCore.Constant(2)
  )

  val orExpr = LCore.BinaryLogicOp(
    BinaryLogicOperator.Or,
    LCore.Constant(1),
    LCore.Constant(2)
  )

  val nestedAndInPrint = LCore.PrintStmt(andExpr)
  val nestedAndInAssign = LCore.AssignStmt(Identifier("x"), andExpr)

  test("shrinkExpr - and") {
    val expected =
      LCore.IfExpr(
        LCore.Constant(1),
        LCore.Constant(2),
        LCore.ConstantBool(false)
      )
    assertEquals(shrink(andExpr), expected)
  }

  test("shrinkExpr - or") {
    val expected =
      LCore.IfExpr(
        LCore.Constant(1),
        LCore.ConstantBool(true),
        LCore.Constant(2)
      )
    assertEquals(shrink(orExpr), expected)
  }

  test("shrinkExpr - nested expression") {
    val input =
      LCore.BinaryLogicOp(BinaryLogicOperator.And, LCore.Constant(3), orExpr)
    val expected = LCore.IfExpr(
      LCore.Constant(3),
      LCore.IfExpr(
        LCore.Constant(1),
        LCore.ConstantBool(true),
        LCore.Constant(2)
      ),
      LCore.ConstantBool(false)
    )
    assertEquals(shrink(input), expected)
  }

  test("shrinkStmt - print") {
    val expected = LCore.PrintStmt(
      LCore.IfExpr(
        LCore.Constant(1),
        LCore.Constant(2),
        LCore.ConstantBool(false)
      )
    )
    assertEquals(shrink(nestedAndInPrint), expected)
  }

  test("shrinkStmt - assign") {
    val expected = LCore.AssignStmt(
      Identifier("x"),
      LCore.IfExpr(
        LCore.Constant(1),
        LCore.Constant(2),
        LCore.ConstantBool(false)
      )
    )
    assertEquals(shrink(nestedAndInAssign), expected)
  }

  test("shrinkModule - assing and print") {
    val input = LCore.Module(nestedAndInAssign :: nestedAndInPrint :: Nil)
    val expected = LCore.Module(
      (LCore.AssignStmt(
        Identifier("x"),
        LCore.IfExpr(
          LCore.Constant(1),
          LCore.Constant(2),
          LCore.ConstantBool(false)
        )
      )) :: LCore.PrintStmt(
        LCore.IfExpr(
          LCore.Constant(1),
          LCore.Constant(2),
          LCore.ConstantBool(false)
        )
      ) :: Nil
    )
    assertEquals(shrink(input), expected)
  }
}

class EndToEndTests extends FunSuite {
  test("End-to-End: object language -> AST - print(1 + 1)") {
    val printCall = "print(1+1)"

    val expected = LCore.Module(
      LCore.PrintStmt(
        LCore.BinaryNumericOp(
          BinaryNumericOperator.Add,
          LCore.Constant(1),
          LCore.Constant(1)
        )
      ) :: Nil
    )
    assertEquals(LCoreReader.fromSExpToModule(parse(printCall)), expected)
  }

  test("End-to-End: object language -> AST - IfExpr") {
    val input = "if 1 < 2 then true else false"
    val expected = LCore.Module(
      LCore.ExprStmt(
        LCore.IfExpr(
          LCore.Compare(
            CompareOperator.Lt,
            LCore.Constant(1),
            LCore.Constant(2)
          ),
          LCore.ConstantBool(true),
          LCore.ConstantBool(false)
        )
      ) :: Nil
    )

    assertEquals(LCoreReader.fromSExpToModule(parse(input)), expected)
  }
  test("End-to-End: object language -> AST - IfStmt") {
    val input = "if (1 < 2) {true} else {false}"
    val expected = LCore.Module(
      LCore.IfStmt(
        LCore
          .Compare(CompareOperator.Lt, LCore.Constant(1), LCore.Constant(2)),
        LCore.ExprStmt(LCore.ConstantBool(true)) :: Nil,
        LCore.ExprStmt(LCore.ConstantBool(false)) :: Nil
      ) :: Nil
    )

    assertEquals(LCoreReader.fromSExpToModule(parse(input)), expected)
  }
  test("End-to-End: object language -> AST - BinaryLogicOp") {
    val input = "true && false"
    val expected = LCore.Module(
      LCore.ExprStmt(
        LCore.BinaryLogicOp(
          BinaryLogicOperator.And,
          LCore.ConstantBool(true),
          LCore.ConstantBool(false)
        )
      ) :: Nil
    )

    assertEquals(LCoreReader.fromSExpToModule(parse(input)), expected)
  }
  test("End-to-End: object language -> AST - UnaryLogicOp") {
    val input = "!(1 < 2)"
    val expected = LCore.Module(
      LCore.ExprStmt(
        LCore.UnaryLogicOp(
          UnaryLogicOperator.Not,
          LCore.Compare(
            CompareOperator.Lt,
            LCore.Constant(1),
            LCore.Constant(2)
          )
        )
      ) :: Nil
    )

    assertEquals(LCoreReader.fromSExpToModule(parse(input)), expected)
  }

  test("End-to-End: object language -> eval result") {
    val program = "1 + (4 - 2) - (-8)"
    assertEquals(
      LIntInterpreter.evalModule(LCoreReader.fromSExpToModule(parse(program))),
      11L
    )
  }
}
