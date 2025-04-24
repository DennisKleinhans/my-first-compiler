package lang

case class LexerError(message: String) extends RuntimeException(message)

enum Token {
  case LPAREN, RPAREN, COMMA // Punctuation: ( ) ,
  case PLUS, MINUS, EQUAL // Operators: + - =
  case NUMBER(value: Long) // 123
  case IDENT(value: String) // input_int print
  case EOF // End of input
}

class Lexer(input: String) extends Iterator[Token] {

  private var pos = 0
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
      case ',' =>
        skip()
        Token.COMMA
      case '+' =>
        skip()
        Token.PLUS
      case '-' =>
        skip()
        Token.MINUS
      case '=' =>
        skip()
        Token.EQUAL
      case c if c.isDigit =>
        readNumber()
      case c if c.isLetter || c == '_' =>
        readIdent()
      case _ => throw LexerError(s"Unexpected character '${currentChar}' at position $pos")
    }
}
