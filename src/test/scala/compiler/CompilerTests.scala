package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import lang.parse
import CommonNodes.*
import CommonNodes.Atom.*
import LIntInterpreter.*
import munit.Tag
import java.util.jar.Attributes.Name
import compiler.x86VarIf.Instr
import x86.Reg

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
  LWhile.BinaryNumericOp(
    BinaryNumericOperator.Add,
    LWhile.Constant(1),
    LWhile.Constant(1)
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
      expected: LWhile.Expr
  ): Unit = {
    test(s"fromSExpToExpr - $name") {
      assertEquals(LIfReader.fromSExpToExpr(input), expected)
    }
  }

  testReadExpression("binary: 1 + 1", sexpOnePlusOne, onePlusOneAst)
  testReadExpression(
    "binary: 4 - 2",
    sexpFourMinusTwo,
    LWhile.BinaryNumericOp(
      BinaryNumericOperator.Sub,
      LWhile.Constant(4),
      LWhile.Constant(2)
    )
  )
  testReadExpression(
    "unary: -8",
    sexpMinusEight,
    LWhile.UnaryNumericOp(UnaryNumericOperator.USub, LWhile.Constant(8))
  )
  testReadExpression(
    "call: read_int",
    sexpInputIntCall,
    LWhile.ReadIntCall
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
      LIfReader.fromSExpToStmt(sexp),
      LWhile.ExprStmt(onePlusOneAst)
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
      LIfReader.fromSExpToStmt(sexp),
      LWhile.AssignStmt(Identifier("x"), LWhile.Constant(5))
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
      LIfReader.fromSExpToStmt(sexp),
      LWhile.PrintStmt(
        LWhile.BinaryNumericOp(
          BinaryNumericOperator.Add,
          LWhile.Variable(Identifier("x")),
          LWhile.Constant(2)
        )
      )
    )
  }
}

class LIntInterpreterTests extends FunSuite {

  test("LIntInterpreter.evalExpr - binary: 1 + 1") {
    assertEquals(
      LIntInterpreter.evalExpr(LIfReader.fromSExpToExpr(sexpOnePlusOne)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - binary: 4 - 2") {
    assertEquals(
      LIntInterpreter.evalExpr(LIfReader.fromSExpToExpr(sexpFourMinusTwo)),
      2L
    )
  }

  test("LIntInterpreter.evalExpr - unary: -8") {
    assertEquals(
      LIntInterpreter.evalExpr(LIfReader.fromSExpToExpr(sexpMinusEight)),
      -8L
    )
  }

  test("LIntInterpreter.evalModule - print statement") {
    val input = "print(1 + 2)"
    val expectedOutput = "3\n"

    val outputStream = new java.io.ByteArrayOutputStream()
    Console.withOut(outputStream) {
      LIntInterpreter.evalModule(LIfReader.fromSExpToModule(parse(input)))
    }

    val actualOutput = outputStream.toString
    assertEquals(actualOutput, expectedOutput)
  }

  // ───────────────────────────────────────────────────────────────
  // ─── Partial Evaluation Tests ──────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("LIntInterpreter.partialEvalExpr - constant folding") {
    val input =
      LWhile.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LWhile.Constant(1),
        LWhile.Constant(2)
      )
    val expected = LWhile.Constant(3)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - nested folding") {
    val input = LWhile.BinaryNumericOp(
      BinaryNumericOperator.Sub,
      LWhile.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LWhile.Constant(2),
        LWhile.Constant(3)
      ),
      LWhile.Constant(1)
    )
    val expected = LWhile.Constant(4)
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalExpr - symbolic parts remain") {
    val input = LWhile.BinaryNumericOp(
      BinaryNumericOperator.Add,
      LWhile.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LWhile.Constant(1),
        LWhile.Constant(2)
      ),
      LWhile.ReadIntCall
    )
    val expected =
      LWhile.BinaryNumericOp(
        BinaryNumericOperator.Add,
        LWhile.Constant(3),
        LWhile.ReadIntCall
      )
    assertEquals(LIntInterpreter.partialEvalExpr(input), expected)
  }

  test("LIntInterpreter.partialEvalStatement - partially simplified print") {
    val input =
      LWhile.PrintStmt(
        LWhile.BinaryNumericOp(
          BinaryNumericOperator.Sub,
          LWhile.Constant(5),
          LWhile.Constant(3)
        )
      )
    val expected = LWhile.PrintStmt(LWhile.Constant(2))
    assertEquals(LIntInterpreter.partialEvalStatement(input), expected)
  }
}

class RemoveComplexOperandsTests extends FunSuite {
  // ───────────────────────────────────────────────────────────────
  // ─── LMonVar Tests ─────────────────────────────────────────────
  // ───────────────────────────────────────────────────────────────

  test("simplify to LMonVar - nested arithmetic") {
    val program = "1 + (2+2) -8"
    val result = simplifyModule(
      LIfReader.fromSExpToModule(parse(program))
    )

    val expected = LMonWhile.Module(
      List(
        LMonWhile.AssignStmt(
          Identifier("$tmp$_1"),
          LMonWhile.BinaryNumericOp(
            BinaryNumericOperator.Add,
            Atom.Constant(2),
            Atom.Constant(2)
          )
        ),
        LMonWhile.AssignStmt(
          Identifier("$tmp$_2"),
          LMonWhile.BinaryNumericOp(
            BinaryNumericOperator.Add,
            Atom.Constant(1),
            Atom.Variable(Identifier("$tmp$_1"))
          )
        ),
        LMonWhile.AssignStmt(
          Identifier("$tmp$_3"),
          LMonWhile.BinaryNumericOp(
            BinaryNumericOperator.Sub,
            Atom.Variable(Identifier("$tmp$_2")),
            Atom.Constant(8)
          )
        ),
        LMonWhile.ExprStmt(
          LMonWhile.AtomExpr(
            Atom.Variable(Identifier("$tmp$_3"))
          )
        )
      )
    )

    assertEquals(result, expected)
  }

  test("simplify to LMonIf - IfExpr") {
    val input = "if 1==1 then 1+1 else 2+2"
    val output = simplifyModule(
      LIfReader.fromSExpToModule(parse(input))
    )

    val expected = LMonWhile.Module(
      List(
        LMonWhile.ExprStmt(
          LMonWhile.IfExpr(
            LMonWhile.Compare(CompareOperator.Eq, Constant(1), Constant(1)),
            LMonWhile.Begin(
              List(
                LMonWhile.AssignStmt(
                  Identifier("$tmp$_1"),
                  LMonWhile.BinaryNumericOp(BinaryNumericOperator.Add, Constant(1), Constant(1))
                )
              ),
              LMonWhile.AtomExpr(Variable(Identifier("$tmp$_1")))
            ),
            LMonWhile.Begin(
              List(
                LMonWhile.AssignStmt(
                  Identifier("$tmp$_2"),
                  LMonWhile.BinaryNumericOp(BinaryNumericOperator.Add, Constant(2), Constant(2))
                )
              ),
              LMonWhile.AtomExpr(Variable(Identifier("$tmp$_2")))
            )
          )
        )
      )
    )

    assertEquals(output, expected)
  }

  test("simplify Module - IfStmt") {

    val expected = LMonWhile.Module(
      List(
        LMonWhile.AssignStmt(
          Identifier("$tmp$_1"),
          LMonWhile
            .Compare(CompareOperator.Eq, Atom.Constant(1), Atom.Constant(1))
        ),
        LMonWhile.IfStmt(
          LMonWhile.AtomExpr(Atom.Variable(Identifier("$tmp$_1"))),
          List(
            LMonWhile.AssignStmt(
              Identifier("$tmp$_2"),
              LMonWhile.BinaryNumericOp(
                BinaryNumericOperator.Add,
                Atom.Constant(1),
                Atom.Constant(1)
              )
            ),
            LMonWhile.PrintStmt(Atom.Variable(Identifier("$tmp$_2")))
          ),
          List(
            LMonWhile.AssignStmt(
              Identifier("$tmp$_3"),
              LMonWhile.BinaryNumericOp(
                BinaryNumericOperator.Add,
                Atom.Constant(2),
                Atom.Constant(2)
              )
            ),
            LMonWhile.PrintStmt(Atom.Variable(Identifier("$tmp$_3")))
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
        simplifyModule(
          LIfReader.fromSExpToModule((parse(programm)))
        )
      )
    )
    val expected = x86VarIf.Program(
      Map(
        "start" -> List(
          x86VarIf
            .MovQ(
              x86VarIf.Immediate(1),
              x86VarIf.Variable(Identifier("$tmp$_1"))
            ),
          x86VarIf.AddQ(
            x86VarIf.Immediate(2),
            x86VarIf.Variable(Identifier("$tmp$_1"))
          ),
          x86VarIf.MovQ(x86VarIf.Immediate(0L), Reg.Rax),
          x86VarIf.Jmp("conclusion")
        )
      )
    )
    assertEquals(result, expected)
  }

  test("selectInstructions - read_int()") {
    val programm = "1 + read_int()"
    val result = selectInstructions(
      explicateControl(
        simplifyModule(
          LIfReader.fromSExpToModule((parse(programm)))
        )
      )
    )
    val expected = x86VarIf.Program(
      Map(
        "start" -> List(
          x86VarIf.CallQ("read_int", 0),
          x86VarIf.MovQ(
            x86.Reg.Rax,
            x86VarIf.Variable(Identifier("$tmp$_1"))
          ),
          x86VarIf
            .MovQ(
              x86VarIf.Immediate(1),
              x86VarIf.Variable(Identifier("$tmp$_2"))
            ),
          x86VarIf.AddQ(
            x86VarIf.Variable(Identifier("$tmp$_1")),
            x86VarIf.Variable(Identifier("$tmp$_2"))
          ),
          x86VarIf.MovQ(x86VarIf.Immediate(0L), Reg.Rax),
          x86VarIf.Jmp("conclusion")
        )
      )
    )
  }

  test("selectInstructions - complex arithmetic") {
    val programm = "1+(3+4)-8"
    val expected = x86VarIf.Program(
      Map(
        "start" -> List(
          x86VarIf
            .MovQ(
              x86VarIf.Immediate(3),
              x86VarIf.Variable(Identifier("$tmp$_1"))
            ),
          x86VarIf
            .AddQ(
              x86VarIf.Immediate(4),
              x86VarIf.Variable(Identifier("$tmp$_1"))
            ),
          x86VarIf
            .MovQ(
              x86VarIf.Immediate(1),
              x86VarIf.Variable(Identifier("$tmp$_2"))
            ),
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
          ),
          x86VarIf.MovQ(x86VarIf.Immediate(0L), Reg.Rax),
          x86VarIf.Jmp("conclusion")
        )
      )
    )
    val result = selectInstructions(
      explicateControl(
        simplifyModule(
          LIfReader.fromSExpToModule(parse(programm))
        )
      )
    )
    assertEquals(result, expected)
  }
}

class ShrinkTests extends FunSuite {
  val andExpr = LWhile.BinaryLogicOp(
    BinaryLogicOperator.And,
    LWhile.Constant(1),
    LWhile.Constant(2)
  )

  val orExpr = LWhile.BinaryLogicOp(
    BinaryLogicOperator.Or,
    LWhile.Constant(1),
    LWhile.Constant(2)
  )

  val nestedAndInPrint = LWhile.PrintStmt(andExpr)
  val nestedAndInAssign = LWhile.AssignStmt(Identifier("x"), andExpr)

  test("shrinkExpr - and") {
    val expected =
      LWhile.IfExpr(
        LWhile.Constant(1),
        LWhile.Constant(2),
        LWhile.ConstantBool(false)
      )
    assertEquals(shrinkExpr(andExpr), expected)
  }

  test("shrinkExpr - or") {
    val expected =
      LWhile.IfExpr(
        LWhile.Constant(1),
        LWhile.ConstantBool(true),
        LWhile.Constant(2)
      )
    assertEquals(shrinkExpr(orExpr), expected)
  }

  test("shrinkExpr - nested expression") {
    val input =
      LWhile.BinaryLogicOp(BinaryLogicOperator.And, LWhile.Constant(3), orExpr)
    val expected = LWhile.IfExpr(
      LWhile.Constant(3),
      LWhile.IfExpr(
        LWhile.Constant(1),
        LWhile.ConstantBool(true),
        LWhile.Constant(2)
      ),
      LWhile.ConstantBool(false)
    )
    assertEquals(shrinkExpr(input), expected)
  }

  test("shrinkStmt - print") {
    val expected = LWhile.PrintStmt(
      LWhile.IfExpr(
        LWhile.Constant(1),
        LWhile.Constant(2),
        LWhile.ConstantBool(false)
      )
    )
    assertEquals(shrinkStmt(nestedAndInPrint), expected)
  }

  test("shrinkStmt - assign") {
    val expected = LWhile.AssignStmt(
      Identifier("x"),
      LWhile.IfExpr(
        LWhile.Constant(1),
        LWhile.Constant(2),
        LWhile.ConstantBool(false)
      )
    )
    assertEquals(shrinkStmt(nestedAndInAssign), expected)
  }

  test("shrinkModule - assing and print") {
    val input = LWhile.Module(nestedAndInAssign :: nestedAndInPrint :: Nil)
    val expected = LWhile.Module(
      (LWhile.AssignStmt(
        Identifier("x"),
        LWhile.IfExpr(
          LWhile.Constant(1),
          LWhile.Constant(2),
          LWhile.ConstantBool(false)
        )
      )) :: LWhile.PrintStmt(
        LWhile.IfExpr(
          LWhile.Constant(1),
          LWhile.Constant(2),
          LWhile.ConstantBool(false)
        )
      ) :: Nil
    )
    assertEquals(shrinkModule(input), expected)
  }
}

class EndToEndTests extends FunSuite {
  test("End-to-End: object language -> AST - print(1 + 1)") {
    val printCall = "print(1+1)"

    val expected = LWhile.Module(
      LWhile.PrintStmt(
        LWhile.BinaryNumericOp(
          BinaryNumericOperator.Add,
          LWhile.Constant(1),
          LWhile.Constant(1)
        )
      ) :: Nil
    )
    assertEquals(LIfReader.fromSExpToModule(parse(printCall)), expected)
  }

  test("End-to-End: object language -> AST - IfExpr") {
    val input = "if 1 < 2 then true else false"
    val expected = LWhile.Module(
      LWhile.ExprStmt(
        LWhile.IfExpr(
          LWhile.Compare(
            CompareOperator.Lt,
            LWhile.Constant(1),
            LWhile.Constant(2)
          ),
          LWhile.ConstantBool(true),
          LWhile.ConstantBool(false)
        )
      ) :: Nil
    )

    assertEquals(LIfReader.fromSExpToModule(parse(input)), expected)
  }
  test("End-to-End: object language -> AST - IfStmt") {
    val input = "if (1 < 2) {true} else {false}"
    val expected = LWhile.Module(
      LWhile.IfStmt(
        LWhile
          .Compare(CompareOperator.Lt, LWhile.Constant(1), LWhile.Constant(2)),
        LWhile.ExprStmt(LWhile.ConstantBool(true)) :: Nil,
        LWhile.ExprStmt(LWhile.ConstantBool(false)) :: Nil
      ) :: Nil
    )

    assertEquals(LIfReader.fromSExpToModule(parse(input)), expected)
  }
  test("End-to-End: object language -> AST - BinaryLogicOp") {
    val input = "true && false"
    val expected = LWhile.Module(
      LWhile.ExprStmt(
        LWhile.BinaryLogicOp(
          BinaryLogicOperator.And,
          LWhile.ConstantBool(true),
          LWhile.ConstantBool(false)
        )
      ) :: Nil
    )

    assertEquals(LIfReader.fromSExpToModule(parse(input)), expected)
  }
  test("End-to-End: object language -> AST - UnaryLogicOp") {
    val input = "!(1 < 2)"
    val expected = LWhile.Module(
      LWhile.ExprStmt(
        LWhile.UnaryLogicOp(
          UnaryLogicOperator.Not,
          LWhile.Compare(
            CompareOperator.Lt,
            LWhile.Constant(1),
            LWhile.Constant(2)
          )
        )
      ) :: Nil
    )

    assertEquals(LIfReader.fromSExpToModule(parse(input)), expected)
  }

  test("End-to-End: object language -> eval result") {
    val program = "1 + (4 - 2) - (-8)"
    assertEquals(
      LIntInterpreter.evalModule(LIfReader.fromSExpToModule(parse(program))),
      11L
    )
  }
}
