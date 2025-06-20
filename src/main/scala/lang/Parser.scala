package lang

import SExp.{ Node, Number, Symbol }
import scala.collection.mutable

import Token.*

case class ParserError(message: String) extends RuntimeException(message)

object tags {
  val Module = Symbol("Module")

  // Stmt :=
  val If     = Symbol("If")
  val Expr   = Symbol("Expr")
  val Assign = Symbol("Assign")
  val ArrayAssign = Symbol("ArrayAssign")
  val While = Symbol("While")

  // BinaryOp :=
  val Add = Symbol("Add")
  val Sub = Symbol("Sub")
  val Mult = Symbol("Mult")
  val And = Symbol("And")
  val Or = Symbol("Or")
  val Eq = Symbol("Eq")
  val Neq = Symbol("Neq")
  val Lt = Symbol("Lt")
  val Le = Symbol("Le")
  val Gt = Symbol("Gt")
  val Ge = Symbol("Ge")
  val Is = Symbol("Is")

  // UnaryOp :=
  val Neg = Symbol("Neg")
  val Not = Symbol("Not")

  // Exp :=
  val Unary     = Symbol("Unary")
  val Constant  = Symbol("Constant")
  val Binary    = Symbol("Binary")
  val Call      = Symbol("Call")
  val Variable  = Symbol("Variable")
  val IfExp     = Symbol("IfExp")
  val True      = Symbol("True")
  val False     = Symbol("False")
  val Tuple     = Symbol("Tuple")
  val Subscript = Symbol("Subscript")
  // for LArray
  val Array     = Symbol("Array")
  val ArraySubscript = Symbol("ArraySubscript")

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
      case t                   => throw ParserError(s"Expected $tokenType but got $t")
    }

  def parseModule(): SExp = exhaust { Node(Module, stmts()) }
  def parseExp(): SExp    = exhaust { exp() }
  def parseStmt(): SExp   = exhaust { stmt() }

  private def stmts(): SExp = {
    var statements = List[SExp]()

    val statementDelimiter = List(EOF, RCURLY)

    while (!statementDelimiter.contains(currentToken)) {
      statements = statements :+ stmt()
    }

    Node(statements)
  }

  private def stmt(): SExp =
    peek match {
      case WHILE =>
        consume(WHILE)
        val cond = parens { exp() }
        val body = braces { stmts() }
        Node(While, cond, body)
      case IF =>
        consume(IF)
        peek match {
          // if (EXP) { STMT+ } else { STMT+ }
          case LPAREN =>
            val cond = parens { exp() }
            val thn = braces { stmts() }
            consume(ELSE)
            val els = braces { stmts() }
            Node(If, cond, thn, els)
          // if EXP then EXP else EXP
          case _ =>
            val cond = exp()
            val thn  = { consume(THEN); exp() }
            val els  = { consume(ELSE); exp() }
            Node(Expr, Node(IfExp, cond, thn, els))
        }
      case _ => exp() match {
        // lhs
        case Node(Variable :: (sym: Symbol) :: Nil) if peek == EQUAL =>
          consume(EQUAL)
          val rhs = exp()
          Node(Assign, sym, rhs)
        case Node(ArraySubscript :: e :: index :: Nil ) if peek == EQUAL =>
          consume(EQUAL)
          val rhs = exp()
          Node(ArrayAssign, e, index, rhs)

        case exp => Node(Expr, exp)
      }
    }

  private def exp(): SExp =
    orExpr()

  private def orExpr(): SExp  = infix(andExpr, OR)
  private def andExpr(): SExp = infix(eqExpr, AND)
  private def eqExpr(): SExp  = infix(relExpr, EQ, NEQ, IS)
  private def relExpr(): SExp = infix(addExpr, LT, LE, GT, GE)

  private def addExpr(): SExp = infix(multExpr, PLUS, MINUS)
  private def multExpr(): SExp = infix(accessExpr, MULT)


  private inline def infix(nonTerminal: () => SExp, ops: Token*): SExp =
    var left = nonTerminal()
    while (ops.contains(peek)) {
      val op    = binop()
      val right = nonTerminal()
      left = Node(Binary, op, left, right)
    }
    left

  private def binop(): SExp = next() match {
    case PLUS  => Add
    case MINUS => Sub
    case MULT  => Mult
    case AND   => And
    case OR    => Or
    case EQ    => Eq
    case NEQ   => Neq
    case LT    => Lt
    case LE    => Le
    case GT    => Gt
    case GE    => Ge
    case IS    => Is
    case t     => throw ParserError(s"Unexpected token: $t")
  }

  private def accessExpr() : SExp =
    var left = unary()
    while ((peek == DOT) || (peek == LBRACKET)) {
      if (peek == LBRACKET) {
        consume(LBRACKET)
        val index = exp()
        consume(RBRACKET)
        left = Node(ArraySubscript, left, index)
      } else {
        consume(DOT)
        consume(UNDERSCORE)
        val index = number()
        left = Node(Subscript, left, Number(index))
      }
    }
    left

  private def unary(): SExp =
    peek match {
      case MINUS =>
        skip()
        Node(Unary, Neg, call())
      case NOT =>
        skip()
        Node(Unary, Not, call())
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

  private def number() : Long =
    peek match {
      case NUMBER(value) =>
        skip()
        value
      case t => throw ParserError(s"Number expected, got token: $t")
    }
  private def primitive(): SExp =
    peek match {
      case IF =>
        consume(IF)
        val cond = exp()
        val thn  = { consume(THEN); exp() }
        val els  = { consume(ELSE); exp() }
        Node(IfExp, cond, thn, els)
      case LCURLY =>
        val elements = many(exp, LCURLY, COMMA, RCURLY)
        Node(Tuple, Node(elements))
      case LBRACKET =>
        val elements = many(exp, LBRACKET, COMMA, RBRACKET)
        Node(Array, Node(elements))
      case LPAREN =>
        parens { exp() }
      case NUMBER(value) =>
        skip()
        Node(Constant, Number(value))
      case TRUE =>
        skip()
        Node(True)
      case FALSE =>
        skip()
        Node(False)
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

  inline def braces[T](p: => T): T =
    consume(LCURLY)
    val res = p
    consume(RCURLY)
    res

  inline def many[T](p: () => T, before: Token, sep: Token, after: Token): List[T] =
    consume(before)
    if (peek == after) {
      consume(after)
      Nil
    } else {
      val elements = some(p, sep)
      consume(after)
      elements
    }

  inline def some[T](p: () => T, sep: Token): List[T] =
    val components: mutable.ListBuffer[T] = mutable.ListBuffer.empty
    components += p()
    while (peek == sep) {
      consume(sep)
      components += p()
    }
    components.toList
}

def parse(in: String): SExp = Parser(Lexer(in)).parseModule()
def parseExp(in: String): SExp = Parser(Lexer(in)).parseExp()
def parseStmt(in: String): SExp = Parser(Lexer(in)).parseStmt()
