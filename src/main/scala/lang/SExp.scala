package lang

/**
 * S-Expressions
 */
enum SExp {
  case Node(elements: List[SExp])
  case Symbol(name: String)
  case Number(n: Long)
}

object SExp {

  val Empty = SExp.Node(Nil)

  def Node(elements: SExp*): SExp = Node(elements.toList)

}
