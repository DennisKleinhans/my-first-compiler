package lang

import lang.SExp
import lang.SExp.{ Node, Number, Symbol }
import scala.collection.mutable

import Token.*

case class ParserError(message: String) extends RuntimeException(message)

object tags {
  val Module = Symbol("Module")

  // Stmt :=
  val Expr = Symbol("Expr")
  val Assign = Symbol("Assign")

  // BinaryOp :=
  val Add = Symbol("Add")
  val Sub = Symbol("Sub")

  // UnaryOp :=
  val Neg = Symbol("Neg")

  // Exp :=
  val Unary = Symbol("Unary")
  val Constant = Symbol("Constant")
  val Binary = Symbol("Binary")
  val Call = Symbol("Call")
  val Variable = Symbol("Variable")
}

class Parser(tokens: Iterator[Token]) {
  import tags.*

  private var currentToken = tokens.next()

  private def peek: Token = currentToken

  private def skip(): Unit =
    currentToken = tokens.next()

  private def next(): Token =
    val curr = peek
    skip()
    curr

  private def exhaust[T](p: => T): T =
    val res = p
    if peek != EOF then {
      throw ParserError("Expected EOF")
    }
    res

  private def consume(tokenType: Token): Unit =
    peek match {
      case t if t == tokenType => skip()
      case t => throw ParserError(s"Expected $tokenType but got $t")
    }

  def parseModule(): SExp = exhaust { Node(Module, stmts()) }
  def parseExp(): SExp = exhaust { exp() }
  def parseStmt(): SExp = exhaust { stmt() }

  private def stmts(): SExp = {
    var statements = List[SExp]()

    while (currentToken != EOF) {
      statements = statements :+ stmt()
    }

    Node(statements)
  }

  private def stmt(): SExp =
    exp() match {
      // lhs
      case Node(Variable :: (sym: Symbol) :: Nil) if peek == EQUAL =>
        consume(EQUAL)
        val rhs = exp()
        Node(Assign, sym, rhs)
      case exp => Node(Expr, exp)
    }

  private def exp(): SExp =
    add()

  private def add(): SExp = infix(unary, PLUS, MINUS)

  private inline def infix(nonTerminal: () => SExp, ops: Token*): SExp =
    var left = nonTerminal()
    while (ops.contains(peek)) {
      val op = binop()
      val right = nonTerminal()
      left = Node(Binary, op, left, right)
    }
    left

  private def binop(): SExp = next() match {
    case PLUS => Add
    case MINUS => Sub
    case t => throw ParserError(s"Unexpected token: $t")
  }

  private def unary(): SExp =
    peek match {
      case MINUS =>
        skip()
        Node(Unary, Neg, call())
      case _ => call()
    }

  private def call(): SExp = {
    val callee = primitive()

    peek match {
      // it's a call! (right now, no arguments)
      case Token.LPAREN =>
        Node(Call, callee, arguments())
      case _ => callee
    }
  }

  private def arguments(): SExp =
    Node(many(exp, LPAREN, COMMA, RPAREN))

  private def primitive(): SExp =
    peek match {
      case LPAREN =>
        parens { exp() }
      case NUMBER(value) =>
        skip()
        Node(Constant, Number(value))
      case i: IDENT =>
        skip()
        Node(Variable, Symbol(i.value))
      case t => throw ParserError(s"Unexpected token: $t")
    }

  // Helpers
  inline def parens[T](p: => T): T =
    consume(LPAREN)
    val res = p
    consume(RPAREN)
    res

  inline def many[T](p: () => T, before: Token, sep: Token, after: Token): List[T] =
    consume(before)
    if (peek == after) {
      consume(after)
      Nil
    } else {
      val components: mutable.ListBuffer[T] = mutable.ListBuffer.empty
      components += p()
      while (peek == sep) {
        consume(sep)
        components += p()
      }
      consume(after)
      components.toList
    }
}

def parse(in: String): SExp = Parser(Lexer(in)).parseModule()
def parseExp(in: String): SExp = Parser(Lexer(in)).parseExp()
def parseStmt(in: String): SExp = Parser(Lexer(in)).parseStmt()
