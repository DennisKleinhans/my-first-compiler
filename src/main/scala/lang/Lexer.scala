package lang

case class LexerError(message: String) extends RuntimeException(message)

enum Token {
  case COMMA, COLON, DOT            // , :, .
  case LPAREN, RPAREN               // ( )
  case LCURLY, RCURLY               // { }
  case LBRACKET, RBRACKET           // [ ]
  case UNDERSCORE
  case PLUS, MINUS, EQUAL, MULT     // Operators: + - = *
  case EQ, NEQ, LT, LE, GT, GE, IS  // Operators == != <= > >= is
  case AND, OR, NOT                 // && || !
  case TRUE, FALSE                  // true false
  case NUMBER(value: Long)          // 123
  case IDENT(value: String)         // input_int print
  case IF, THEN, ELSE
  case WHILE
  case DEF, RETURN                  // function keywords: def return
  case ARR                          // Type operator ->
  case EOF                          // End of input
  case LAMBDA                       // keyword: lambda
}

class Lexer(input: String) extends Iterator[Token] {

  private var pos         = 0
  private var currentChar = if input.isEmpty then '\u0000' else input(0)

  private def skip(): Unit =
    pos += 1
    if pos < input.length then {
      currentChar = input(pos)
    } else {
      currentChar = '\u0000'
    }

  private def skipWhitespace(): Unit =
    while (hasNext && currentChar.isWhitespace) {
      skip()
    }

  private def readNumber() = {
    var result = 0L

    while (currentChar.isDigit) {
      result = result * 10L + currentChar.asDigit
      skip()
    }

    Token.NUMBER(result)
  }

  private def readIdent() =
    val buffer = new StringBuilder
    while (currentChar.isLetterOrDigit || "_".contains(currentChar)) {
      buffer.append(currentChar)
      skip()
    }
    Token.IDENT(buffer.toString)

  def hasNext: Boolean = currentChar != '\u0000'

  def next(): Token =
    skipWhitespace()

    currentChar match {
      case '\u0000' => Token.EOF
      case '(' =>
        skip()
        Token.LPAREN
      case ')' =>
        skip()
        Token.RPAREN
      case '{' =>
        skip()
        Token.LCURLY
      case '}' =>
        skip()
        Token.RCURLY
      case '[' =>
        skip()
        Token.LBRACKET
      case ']' =>
        skip()
        Token.RBRACKET
      case ':' =>
        skip()
        Token.COLON
      case '.' =>
        skip()
        Token.DOT
      case ',' =>
        skip()
        Token.COMMA
      case '_' =>
        skip()
        Token.UNDERSCORE
      case '+' =>
        skip()
        Token.PLUS
      case '-' =>
        skip()
        currentChar match {
          case '>' => skip(); Token.ARR // ->
          case _ => Token.MINUS
        }
      case '*' =>
        skip()
        Token.MULT
      case '!' =>
        skip()
        currentChar match {
          case '=' => skip(); Token.NEQ // !=
          case _ => Token.NOT           // !
        }
      case '&' =>
        skip()
        currentChar match {
          case '&' => skip(); Token.AND // &&
          case _ => throw LexerError("Conjunction uses two &s (that is, &&)")
        }
      case '|' =>
        skip()
        currentChar match {
          case '|' => skip(); Token.OR // &&
          case _ => throw LexerError("Disjunction uses two |s (that is, ||)")
        }
      case '=' =>
        skip()
        currentChar match {
          case '=' => skip(); Token.EQ // ==
          case _ => Token.EQUAL        // =
        }
      case '<' =>
        skip()
        currentChar match {
          case '=' => skip(); Token.LE // <=
          case _ => Token.LT           // <
        }
      case '>' =>
        skip()
        currentChar match {
          case '=' => skip(); Token.GE // >=
          case _ => Token.GT           // >
        }
      case c if c.isDigit =>
        readNumber()
      case c if c.isLetter || c == '_' =>
        readIdent() match {
          case Token.IDENT("if")     => Token.IF
          case Token.IDENT("while")  => Token.WHILE
          case Token.IDENT("then")   => Token.THEN
          case Token.IDENT("else")   => Token.ELSE
          case Token.IDENT("true")   => Token.TRUE
          case Token.IDENT("false")  => Token.FALSE
          case Token.IDENT("is")     => Token.IS
          case Token.IDENT("def")    => Token.DEF
          case Token.IDENT("return") => Token.RETURN
          case Token.IDENT("lambda") => Token.LAMBDA
          case other => other
        }
      case _ => throw LexerError(s"Unexpected character '${currentChar}' at position $pos")
    }
}
