package compiler
import collection.mutable
import compiler.CommonNodes.Identifier
import compiler.LMonIf.Expr
import CommonNodes.*
import CommonNodes.Atom.*

object LabelGenerator {
  private var counter = 0

  def freshLabel(name: String = "block"): CIf.Label =
    counter += 1
    name + counter
}

// compile the expr expression, with the result being assigned to id, sequence with continuation
// 1. create a block for the continuation
// 2. compile then-branch with explicate_assign
// 3. compile else-branch with explicate_assign
// 4. compile the condition with explicate_pred, passing in the two branches
def explicateAssign(
    expr: LMonIf.Expr,
    id: Identifier,
    continuation: CIf.BasicBlock,
    basicBlocks: mutable.Map[String, CIf.BasicBlock]
): CIf.BasicBlock = {
  expr match {
    case LMonIf.IfExpr(cond, thn, els) =>
      val contLabel = LabelGenerator.freshLabel()
      basicBlocks(contLabel) = continuation

      val thenBlock = explicateAssign(
        thn,
        id,
        CIf.BasicBlock(List.empty, CIf.Goto(contLabel)),
        basicBlocks
      )

      val elseBlock = explicateAssign(
        els,
        id,
        CIf.BasicBlock(List.empty, CIf.Goto(contLabel)),
        basicBlocks
      )

      explicatePred(cond, thenBlock, elseBlock, basicBlocks)

    case LMonIf.Begin(stmts, expr) =>
      val assignBlock = explicateAssign(expr, id, continuation, basicBlocks)
      stmts.foldRight(assignBlock) { (stmt, cont) =>
        explicateStmt(stmt, cont, basicBlocks)
      }

    case _ =>
      val assignStmt = CIf.AssignStmt(id, convertExprToCIf(expr))
      CIf.BasicBlock(List(assignStmt), continuation.tail)
  }
}

// compile the condition expression, jumping to thn if true and els if not
// 1. create blocks for the branches
// 2. generate an "if" statement with jumps to the two new blocks
def explicatePred(
    condition: LMonIf.Expr,
    thn: CIf.BasicBlock,
    els: CIf.BasicBlock,
    basicBlocks: mutable.Map[String, CIf.BasicBlock]
): CIf.BasicBlock = {
  condition match {
    case LMonIf.Compare(cmp, e1, e2) =>
      val thenLable = LabelGenerator.freshLabel()
      val elseLable = LabelGenerator.freshLabel()

      basicBlocks(thenLable) = thn
      basicBlocks(elseLable) = els

      val compareExpr: CIf.Compare = CIf.Compare(cmp, e1, e2)
      val thenGoto: CIf.Goto = CIf.Goto(thenLable)
      val elseGoto: CIf.Goto = CIf.Goto(elseLable)

      CIf.BasicBlock(List.empty, CIf.If(compareExpr, thenGoto, elseGoto))

    // swap the then and else branch
    case LMonIf.UnaryLogicOp(UnaryLogicOperator.Not, expr) =>
      explicatePred(expr, els, thn, basicBlocks)

    case LMonIf.IfExpr(condExpr, thenExpr, elseExpr) =>
      val innerThenBlock = explicatePred(thenExpr, thn, els, basicBlocks)
      val innerElseBlock = explicatePred(elseExpr, thn, els, basicBlocks)
      explicatePred(condExpr, innerThenBlock, innerElseBlock, basicBlocks)

    // TODO: what happens if atom is not a boolean? Should it be allowed to interpret a number as a boolean e.g. 0 = false else true?
    case LMonIf.AtomExpr(atom) =>
      val compareExpr: CIf.Compare =
        CIf.Compare(CompareOperator.Eq, atom, ConstantBool(true))
      val thenLabel = LabelGenerator.freshLabel()
      val elseLabel = LabelGenerator.freshLabel()

      basicBlocks(thenLabel) = thn
      basicBlocks(elseLabel) = els

      CIf.BasicBlock(
        List.empty,
        CIf.If(compareExpr, CIf.Goto(thenLabel), CIf.Goto(elseLabel))
      )

    // TODO: Was passiert hier? Können überhaupt noch andere Fälle auftreten?
    case _ => ???
  }
}

// compile expr for its side effects, sequence with continuation
def explicateEffect(
    expr: LMonIf.Expr,
    continuation: CIf.BasicBlock,
    basicBlocks: mutable.Map[String, CIf.BasicBlock]
): CIf.BasicBlock = {
  expr match {
    case LMonIf.IfExpr(condExpr, thenExpr, elseExpr) =>
      // both branches only for their side effects
      val thenBlock = explicateEffect(thenExpr, continuation, basicBlocks)
      val elseBlock = explicateEffect(elseExpr, continuation, basicBlocks)
      explicatePred(condExpr, thenBlock, elseBlock, basicBlocks)

    case LMonIf.Begin(stmts, expr) =>
      val effectBlock = explicateEffect(expr, continuation, basicBlocks)
      stmts.foldRight(effectBlock) { (stmt, cont) =>
        explicateStmt(stmt, cont, basicBlocks)
      }

    case _ =>
      // handle all other expressions as a statment
      val stmt = CIf.ExprStmt(convertExprToCIf(expr))
      CIf.BasicBlock(List(stmt), continuation.tail)
  }
}

// compile the given statement stms, sequence with continuation
def explicateStmt(
    stmt: LMonIf.Stmt,
    continuation: CIf.BasicBlock,
    basicBlocks: mutable.Map[String, CIf.BasicBlock]
): CIf.BasicBlock = {
  stmt match {
    case LMonIf.AssignStmt(id, expr) =>
      explicateAssign(expr, id, continuation, basicBlocks)

    case LMonIf.PrintStmt(atom) =>
      val printStmt = CIf.PrintStmt(atom)
      CIf.BasicBlock(List(printStmt), continuation.tail)

    case LMonIf.ExprStmt(expr) =>
      explicateEffect(expr, continuation, basicBlocks)

    case LMonIf.IfStmt(cond, thenBranch, elseBranch) =>
      val contLabel = LabelGenerator.freshLabel()
      basicBlocks(contLabel) = continuation

      val thenBlock =
        thenBranch.foldRight(CIf.BasicBlock(List.empty, CIf.Goto(contLabel))) {
          (stmt, cont) => explicateStmt(stmt, cont, basicBlocks)
        }

      val elseBlock =
        elseBranch.foldRight(
          CIf.BasicBlock(List.empty, CIf.Goto(contLabel))
        ) { (stmt, cont) =>
          explicateStmt(stmt, cont, basicBlocks)
        }

      explicatePred(cond, thenBlock, elseBlock, basicBlocks)
  }
}

def convertExprToCIf(expr: LMonIf.Expr): CIf.Expr = expr match
  case LMonIf.UnaryNumericOp(op, a)         => CIf.UnaryNumericOp(op, a)
  case LMonIf.BinaryNumericOp(op, lhs, rhs) => CIf.BinaryNumericOp(op, lhs, rhs)
  case LMonIf.Compare(cmp, lhs, rhs)        => CIf.Compare(cmp, lhs, rhs)
  case LMonIf.ReadIntCall                   => CIf.ReadIntCall
  case LMonIf.AtomExpr(a)                   => CIf.AtomExpr(a)
  case _ => sys error "cannot directly convert expression: " + expr

def explicateControl(module: LMonIf.Module): CIf.CProgram = {
  val basicBlocks = mutable.Map[String, CIf.BasicBlock]()

  // construct start block with Return(0) as tail
  val startBlock = module.stmts.foldRight(
    CIf.BasicBlock(List.empty, CIf.Return(CIf.AtomExpr(Constant(0L))))
  ) { (stmt, cont) => explicateStmt(stmt, cont, basicBlocks) }

  // add startBlock
  val startLabel = "start"
  basicBlocks(startLabel) = startBlock

  CIf.CProgram(basicBlocks.toMap)
}
