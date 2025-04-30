package compiler

import munit.FunSuite
import lang.SExp
import lang.SExp.*
import compiler.Expr.*
import compiler.Stmt.*



class CompilerTests extends FunSuite {
  test("1 + 1 = 2") {
    assertEquals(1 + 1, 2)
  }

  // basic helper SExps
  private val const42 = Number(42)
  private val const5 = Number(5)
  private val const3 = Number(3)
  private val inputCall = Node(Symbol("Call"), Symbol("input_int"), Node())

  test("readExpression") {
    val addInner = Node(List(const5, Symbol("+"), const3))
    val negInner = Node(List(Symbol("-"), addInner))
    val sum1 = Node(List(inputCall, Symbol("+"), negInner))
    val fullExpr = Node(List(sum1, Symbol("+"), const42))

    val expr = readExpression(fullExpr)
    val expected = BinaryOp(
      BinaryOp(
        Call("input_int", Nil),
        BinaryOperator.Add,
        UnaryOp(UnaryOperator.USub,
          BinaryOp(Constant(5), BinaryOperator.Add, Constant(3))
        )
      ),
      BinaryOperator.Add,
      Constant(42)
    )
    assertEquals(expr, expected)
  }

  test("readModule"){
    val expr1 = Node(List(Symbol("Call"), Symbol("input_int"), Node()))
    val expr2 = Node(List(Symbol("Call"), Symbol("print"), Node(Node(Number(1)), Node(Number(2)))))

    val stmt1 = Node(List(Symbol("Expr"), expr1))
    val stmt2 = Node(List(Symbol("Expr"), expr2))
    val moduleSexp = Node(Symbol("Module"), stmt1, stmt2)

    val module = readModule(moduleSexp)
    assertEquals(module.stmts, List(
      ExprStmt(Call("input_int", Nil)),
      ExprStmt(Call("print", List(Constant(1), Constant(2))))
    ))
  }

  test("evalExpr: simple arithmetic"){
    val expr = BinaryOp(
      Constant(7),
      BinaryOperator.Sub,
      UnaryOp(UnaryOperator.USub, Constant(2))
    )
    assertEquals(evalExpr(expr), 9L)
  }

  test("partialEval"){
    val expr = BinaryOp(
      Call("input_int", Nil),
      BinaryOperator.Add,
      UnaryOp(UnaryOperator.USub,
        BinaryOp(Constant(5), BinaryOperator.Add, Constant(3))
      )
    )

    val expected = BinaryOp(
      Call("input_int", Nil),
      BinaryOperator.Add,
      Constant(-8)
    )
    
    assertEquals(partialEval(expr), expected)
  }

  test("SExp -> AST -> partialEval"){
    val addInner = Node(List(const5, Symbol("+"), const3))             
    val negInner = Node(List(Symbol("-"), addInner))                   
    val sum1 = Node(List(inputCall, Symbol("+"), negInner))             
    val fullExpr = Node(List(sum1, Symbol("+"), const42))               

    val parsedExpr = readExpression(fullExpr)
    val partiallyEvaluated = partialEval(parsedExpr)

    val expected = BinaryOp(
      BinaryOp(
        Call("input_int", Nil),
        BinaryOperator.Add,
        Constant(-8)
      ),
      BinaryOperator.Add,
      Constant(42)
    )

    assertEquals(partiallyEvaluated, expected)
  }
}
