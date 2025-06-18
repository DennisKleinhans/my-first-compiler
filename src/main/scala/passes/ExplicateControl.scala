package passes
import collection.mutable
import compiler.{CIr, LMon, CommonNodes}
import CommonNodes.Identifier
import LMon.Expr
import CommonNodes.*
import CommonNodes.Atom.*

object ExplicateControl {

  /** Utility object to generate unique labels for newly created BasicBlocks.
    *
    * @note
    *   Each call to `freshLabel` returns a string of the form `nameN`, where
    *   `N` is a monotonically increasing integer. This ensures that every label
    *   is distinct across the entire translation.
    */
  object LabelGenerator {
    private var blockCounter = 0
    private var condCounter = 0
    private var doneCounter = 0
    private var bodyCounter = 0

    /** Produce a fresh, unique label string by appending a global counter to
      * the given base name.
      *
      * @param name
      *   A prefix for the label (e.g. "block", "cmpAssign", "pred", etc.).
      *   Defaults to "block".
      * @return
      *   A new label of the form `nameN` where `N` is the next unused integer.
      */
    def freshBlockLabel(name: String = "block"): CIr.Label =
      blockCounter += 1
      name + blockCounter

    /** Produce a fresh label for a condition or predicate block.
      *
      * @param name
      *   A prefix for the label (e.g. "cond", "pred"). Defaults to "cond".
      * @return
      *   A new label of the form `nameN` where `N` is the next unused integer.
      */
    def freshCondLable(name: String = "cond"): CIr.Label = {
      condCounter += 1
      name + condCounter
    }

    /** Produce a fresh label for a "done" block, which is typically the end of
      * a loop or conditional.
      *
      * @param name
      *   A prefix for the label (e.g. "done"). Defaults to "done".
      * @return
      *   A new label of the form `nameN` where `N` is the next unused integer.
      */
    def freshDoneLabel(name: String = "done"): CIr.Label = {
      doneCounter += 1
      name + doneCounter
    }

    /** Produce a fresh label for a body block, which is typically used in the
      * context of loops or conditionals.
      *
      * @param name
      *   A prefix for the label (e.g. "body"). Defaults to "body".
      * @return
      *   A new label of the form `nameN` where `N` is the next unused integer.
      */
    def freshBodyLabel(name: String = "body"): CIr.Label = {
      bodyCounter += 1
      name + bodyCounter
    }
  }

  /** Translate a LMonIf expression into one or more CIf BasicBlocks that assign
    * its value into a given temporary variable, then jump to the provided
    * continuation block.
    *
    * In detail:
    *   - If `expr` is an `IfExpr(cond, thenExpr, elseExpr)`, we
    *     1. create a fresh continuation label that points to the given
    *        continuation block, 2. recursively translate `thenExpr` and
    *        `elseExpr` into BasicBlocks, each ending with a `Goto(contLabel)`,
    *        3. generate a “test” block by calling `explicatePred(cond,
    *        thenBlock, elseBlock, basicBlocks)`.
    *
    *   - If `expr` is a `Begin(stmts, innerExpr)`, we
    *     1. recursively translate `innerExpr` (its result is stored into `id`
    *        and jumps to `continuation`), 2. fold the `stmts` list
    *        right-to-left, using each statement’s translation (`explicateStmt`)
    *        to chain into the block returned for `innerExpr`.
    *
    *   - Otherwise (i.e. `expr` is one of: `AtomExpr`, `ReadIntCall`,
    *     `UnaryNumericOp`, `BinaryNumericOp`, `Compare`), we generate exactly
    *     one BasicBlock that performs `id := <converted expr>` and then jumps
    *     to `continuation`. The `convertExprToCIf` helper is used to convert a
    *     LMonIf expression into the corresponding CIf expression form.
    *
    * Each newly created BasicBlock is prepended with the existing statements of
    * `continuation` (via `continuation.stmts`) so that any sequencing of simple
    * statements is preserved. We never drop those previously created
    * statements; instead, we append the new assignment at the front and keep
    * the tail intact.
    *
    * @param expr
    *   The LMonIf expression to evaluate and store into `id`.
    * @param id
    *   The Identifier of the temporary variable where the result will be
    *   stored.
    * @param continuation
    *   A CIf.BasicBlock representing “what comes next” after this expression’s
    *   value is assigned. Its `.stmts` are appended after this assignment, and
    *   its `.tail` is used for the Goto in the new block.
    * @param basicBlocks
    *   A mutable map from label → CIf.BasicBlock, which is populated whenever a
    *   new block is created. Any BasicBlock returned by this function must
    *   already be inserted into `basicBlocks` under a fresh label.
    * @return
    *   The newly created CIf.BasicBlock (with its assignment prepended onto
    *   `continuation.stmts`).
    */
  def explicateAssign(
      expr: LMon.Expr,
      id: Identifier,
      continuation: CIr.BasicBlock,
      basicBlocks: mutable.Map[String, CIr.BasicBlock]
  ): CIr.BasicBlock = {
    expr match {
      case LMon.IfExpr(cond, thn, els) =>
        // 1. Create a new label for the continuation block and insert it into basicBlocks
        val contLabel = LabelGenerator.freshBlockLabel()
        basicBlocks(contLabel) = continuation

        // 2. Translate the “then” branch so that it computes into `id` and then jumps to contLabel
        val thenBlock = explicateAssign(
          thn,
          id,
          CIr.BasicBlock(List.empty, CIr.Goto(contLabel)),
          basicBlocks
        )

        // 3. Translate the “else” branch similarly
        val elseBlock = explicateAssign(
          els,
          id,
          CIr.BasicBlock(List.empty, CIr.Goto(contLabel)),
          basicBlocks
        )

        // 4. Generate a predicate block that tests `cond` to jump to either thenBlock or elseBlock
        explicatePred(cond, thenBlock, elseBlock, basicBlocks)

      case LMon.Begin(stmts, expr) =>
        // 1. Recursively translate the final expression, which will store its result into `id` and end with a Goto(continuation).
        val assignBlock = explicateAssign(expr, id, continuation, basicBlocks)

        // 2. Fold all preceding statements in reverse order so that each Stmt’s translation chains into the block returned for innerExpr.
        stmts.foldRight(assignBlock) { (stmt, cont) =>
          explicateStmt(stmt, cont, basicBlocks)
        }

      case _ =>
        // Simple expressions (AtomExpr, ReadIntCall, UnaryNumericOp, BinaryNumericOp, Compare).
        // Generate exactly one BasicBlock to assign `id := <converted expr>` and then use continuation.tail.
        val assignStmt = CIr.AssignStmt(id, convertExprToCIf(expr))
        CIr.BasicBlock(assignStmt :: continuation.stmts, continuation.tail)
    }
  }

  /** Translate a LMonIf expression used as a boolean predicate into a CIf
    * BasicBlock that jumps to `thn` if the predicate is `true`, or to `els` if
    * `false`.
    *
    * In detail:
    *   - If `condition` is a `Compare(cmp, e1, e2)`, we create fresh labels for
    *     the “then” and “else” targets, insert `thn` and `els` into
    *     `basicBlocks` under those labels, return a BasicBlock whose tail is
    *     `If(cmp, Goto(thenLabel), Goto(elseLabel))`.
    *
    *   - If `condition` is `UnaryLogicOp(Not, inner)`, we simply swap the
    *     `thn`/`els` branches and recurse on `inner`.
    *
    *   - If `condition` is an `IfExpr(cond2, thenExpr, elseExpr)`, we first
    *     recursively generate predicate blocks for `thenExpr` and `elseExpr`
    *     with the same `thn`/`els` continuations, then recursively generate a
    *     block for `cond2` that jumps to those inner predicate-result blocks.
    *
    *   - If `condition` is `Begin(stmts, innerExpr)`, we first translate all
    *     side-effecting statements `stmts` into a chain of BasicBlocks that
    *     lead into the predicate block generated for `innerExpr`.
    *
    *   - If `condition` is `AtomExpr(v)` where `v` is a boolean variable, we
    *     rewrite it as `Compare(Eq, v, ConstantBool(true))` and call ourselves
    *     on that Compare. This turns a bare boolean variable test into an
    *     explicit equality‐to‐true comparison. The resulting block ends with
    *     `If(v == true, thenLabel, elseLabel)`.
    *
    *   - If `condition` is `AtomExpr(ConstantBool(true))` or
    *     `ConstantBool(false)`, we return `thn` or `els` directly (no new block
    *     is needed, because the result is constant).
    *
    * Any other form (e.g., arithmetic or non‐boolean Atoms) is invalid for a
    * predicate and raises an error.
    *
    * @param condition
    *   The LMonIf expression to treat as a boolean test.
    * @param thn
    *   A CIf.BasicBlock that must be jumped to if `condition` evaluates to
    *   true.
    * @param els
    *   A CIf.BasicBlock that must be jumped to if `condition` evaluates to
    *   false.
    * @param basicBlocks
    *   A mutable map of all BasicBlocks so far; any newly generated BasicBlock
    *   (e.g. a Compare‐If block) must be stored here under a fresh label before
    *   returning.
    * @return
    *   The newly created predicate BasicBlock (with an `If` tail) that examines
    *   `condition` and jumps to `thn` or `els`. If `condition` is constant
    *   true/false, returns `thn` or `els`.
    */
  def explicatePred(
      condition: LMon.Expr,
      thn: CIr.BasicBlock,
      els: CIr.BasicBlock,
      basicBlocks: mutable.Map[String, CIr.BasicBlock]
  ): CIr.BasicBlock = {
    condition match {
      case LMon.Compare(cmp, e1, e2) =>
        // Helper to pick a label for the BasicBlock, either by reusing an existing Goto or creating a new one
        def pickLabel(block: CIr.BasicBlock): String = block match {
          case CIr.BasicBlock(Nil, CIr.Goto(lbl)) =>
            // schon ein reiner Goto-Block ⇒ wir können direkt dieses Label verwenden
            lbl
          case _ =>
            // komplexerer Block ⇒ wir brauchen einen echten Label-Eintrag
            val fresh = LabelGenerator.freshBlockLabel()
            basicBlocks(fresh) = block
            fresh
        }

        // Generate fresh labels for the “then” and “else” targets, insert them in the map
        val thenLable = pickLabel(thn)
        val elseLable = pickLabel(els)

        // basicBlocks(thenLable) = thn
        // basicBlocks(elseLable) = els

        // Build an If‐tail: If(e1 cmp e2) goto thenLabel else goto elseLabel
        val compareExpr: CIr.Compare = CIr.Compare(cmp, e1, e2)
        val thenGoto: CIr.Goto = CIr.Goto(thenLable)
        val elseGoto: CIr.Goto = CIr.Goto(elseLable)

        CIr.BasicBlock(List.empty, CIr.If(compareExpr, thenGoto, elseGoto))

      case LMon.UnaryLogicOp(UnaryLogicOperator.Not, expr) =>
        // Swap true/false targets and recurse
        explicatePred(expr, els, thn, basicBlocks)

      case LMon.IfExpr(condExpr, thenExpr, elseExpr) =>
        // First create predicate blocks for the then/else expressions themselves
        val thenPredBlock = explicatePred(thenExpr, thn, els, basicBlocks)
        val elsePredBlock = explicatePred(elseExpr, thn, els, basicBlocks)

        // Now test cond to branch to either thenPredBlock or elsePredBlock
        explicatePred(condExpr, thenPredBlock, elsePredBlock, basicBlocks)

      case LMon.Begin(stmts, expr) =>
        // Translate all side‐effecting statements, then feed into the predicate for innerExpr
        val condBlock = explicatePred(expr, thn, els, basicBlocks)
        stmts.foldRight(condBlock) { (stmt, cont) =>
          explicateStmt(stmt, cont, basicBlocks)
        }

      case LMon.AtomExpr(v @ Variable(_)) =>
        // A bare boolean variable v: rewrite as Compare(v == true) and recurse
        val boolCompare =
          LMon.Compare(CompareOperator.Eq, v, ConstantBool(true))
        explicatePred(boolCompare, thn, els, basicBlocks)

      case LMon.AtomExpr(ConstantBool(true)) => thn

      case LMon.AtomExpr(ConstantBool(false)) => els

      case other => sys error "cannot compare this condition: " + other
    }
  }

  /** Translate a LMonIf expression purely for its side‐effects, then jump to a
    * continuation block.
    *
    *   - If `expr` is an `IfExpr(cond, thenExpr, elseExpr)`, we recursively
    *     translate the `thenExpr` (only for its side‐effects, ignoring its
    *     value) with the same continuation, recursively translate the
    *     `elseExpr` with the same continuation, generate a predicate block via
    *     `explicatePred(cond, thenBlock, elseBlock, basicBlocks)`.
    *
    *   - If `expr` is `Begin(stmts, innerExpr)`, we recursively translate
    *     `innerExpr` for its side‐effects into a block `innerBlock`, fold‐right
    *     all statements `stmts` so that each one’s translation chains into
    *     `innerBlock`.
    *
    *   - Otherwise (e.g. `AtomExpr`, `ReadIntCall`, `UnaryNumericOp`,
    *     `BinaryNumericOp`, `Compare`), we create one BasicBlock containing
    *     `ExprStmt(<converted expr>)` and then a `Goto(continuation)`, since
    *     those forms may have side‐effects (e.g. `ReadIntCall`) but no direct
    *     value assignment.
    *
    * @param expr
    *   The LMonIf expression whose side‐effects we want to preserve.
    * @param continuation
    *   A BasicBlock that represents “what to do next” after the side‐effects
    *   have executed. Its `.tail` is used for the Goto in the newly created
    *   block(s).
    * @param basicBlocks
    *   A mutable map to hold every BasicBlock we create. Newly generated blocks
    *   must be written here under a fresh label before returning.
    * @return
    *   The BasicBlock that should be executed first if we want to run `expr`’s
    *   side‐effects and then reach `continuation`.
    */
  def explicateEffect(
      expr: LMon.Expr,
      continuation: CIr.BasicBlock,
      basicBlocks: mutable.Map[String, CIr.BasicBlock]
  ): CIr.BasicBlock = {
    expr match {
      case LMon.IfExpr(condExpr, thenExpr, elseExpr) =>
        // Both branches for side-effects only
        val thenBlock = explicateEffect(thenExpr, continuation, basicBlocks)
        val elseBlock = explicateEffect(elseExpr, continuation, basicBlocks)
        explicatePred(condExpr, thenBlock, elseBlock, basicBlocks)

      case LMon.Begin(stmts, expr) =>
        // Translate the inner expression’s side-effects, then fold all preceding statements
        val effectBlock = explicateEffect(expr, continuation, basicBlocks)
        stmts.foldRight(effectBlock) { (stmt, cont) =>
          explicateStmt(stmt, cont, basicBlocks)
        }

      case _ =>
        // A simple expression treated as a statement: generate one block with ExprStmt and Goto
        val stmt = CIr.ExprStmt(convertExprToCIf(expr))
        CIr.BasicBlock(stmt :: continuation.stmts, continuation.tail)
    }
  }

  /** Translate a single LMonIf statement into one or more CIf BasicBlocks that,
    * upon completion, jump to the provided continuation block.
    *
    * Cases:
    *   - `AssignStmt(id, expr)`: calls `explicateAssign(expr, id, continuation,
    *     basicBlocks)`.
    *
    *   - `PrintStmt(atom)`: generates exactly one BasicBlock containing
    *     `PrintStmt(atom)` and then `Goto(continuation)`.
    *
    *   - `ExprStmt(expr)`: calls `explicateEffect(expr, continuation,
    *     basicBlocks)` to translate side-effects of `expr`.
    *
    *   - `IfStmt(cond, thenBranch, elseBranch)`: Create a fresh continuation
    *     label for “join” and insert the given `continuation` under that label.
    *     Fold‐right over `thenBranch` to chain all statements, ending with a
    *     `Goto(contLabel)`. Fold‐right over `elseBranch` similarly, ending with
    *     a `Goto(contLabel)`. Produce a predicate block with
    *     `explicatePred(cond, thenChainEntry, elseChainEntry, basicBlocks)`.
    *
    * Each newly created BasicBlock is stored in `basicBlocks` under a fresh
    * label before being returned, ensuring that all generated blocks appear in
    * the final CIf.CProgram.
    *
    * @param stmt
    *   The LMonIf.Stmt to translate.
    * @param continuation
    *   A BasicBlock representing “what to do next” once `stmt` has finished.
    *   Its `.tail` is used in newly created blocks to chain control flow.
    * @param basicBlocks
    *   A mutable map from label → BasicBlock. Every new block must be added
    *   here under a unique label via `LabelGenerator.freshLabel()`.
    * @return
    *   The CIf.BasicBlock that corresponds to the “entry” of this translated
    *   statement. Execution should begin at this returned block in order to
    *   honor `stmt` followed by `continuation`.
    */
  def explicateStmt(
      stmt: LMon.Stmt,
      continuation: CIr.BasicBlock,
      basicBlocks: mutable.Map[String, CIr.BasicBlock]
  ): CIr.BasicBlock = {
    stmt match {
      case LMon.AssignStmt(id, expr) =>
        explicateAssign(expr, id, continuation, basicBlocks)

      // PrintStmt produces exactly one BasicBlock with a Print statement and then jumps to continuation
      case LMon.PrintStmt(atom) =>
        val printStmt = CIr.PrintStmt(atom)
        CIr.BasicBlock(printStmt :: continuation.stmts, continuation.tail)

      case LMon.ExprStmt(expr) =>
        explicateEffect(expr, continuation, basicBlocks)

      case LMon.IfStmt(cond, thenBranch, elseBranch) =>
        // 1. Create a new label for the join‐continuation and store `continuation` under it.
        val contLabel = LabelGenerator.freshBlockLabel()
        basicBlocks(contLabel) = continuation

        // 2. Build the “then” chain: each stmt in thenBranch, folded right, ending with Goto(contLabel)
        val thenBlock =
          thenBranch.foldRight(
            CIr.BasicBlock(List.empty, CIr.Goto(contLabel))
          ) { (stmt, cont) =>
            explicateStmt(stmt, cont, basicBlocks)
          }

        // 3. Build the “else” chain similarly
        val elseBlock =
          elseBranch.foldRight(
            CIr.BasicBlock(List.empty, CIr.Goto(contLabel))
          ) { (stmt, cont) =>
            explicateStmt(stmt, cont, basicBlocks)
          }

        // 4. Create a predicate‐block that tests `cond` and jumps to either thenBlock or elseBlock
        explicatePred(cond, thenBlock, elseBlock, basicBlocks)

      case LMon.WhileStmt(cond, body) =>
        // 1. Create fresh labels for the condition, body, and done blocks
        val condLbl = LabelGenerator.freshCondLable()
        val bodyLbl = LabelGenerator.freshBodyLabel()
        val doneLbl = LabelGenerator.freshDoneLabel()

        // 2. Store the continuation under doneLbl
        basicBlocks(doneLbl) = continuation

        // 3. bodyLbl: Translate the body statements, ending with a jump back to condLbl
        val backToCond = CIr.BasicBlock(Nil, CIr.Goto(condLbl))
        val bodyEntry = body.foldRight(backToCond) { (stmt, cont) =>
          explicateStmt(stmt, cont, basicBlocks)
        }
        basicBlocks(bodyLbl) = bodyEntry

        // 4. condLbl: Create a new BasicBlock that tests the condition and jumps to either bodyLbl or doneLbl
        val thnBlock = CIr.BasicBlock(Nil, CIr.Goto(bodyLbl))
        val elsBlock = CIr.BasicBlock(Nil, CIr.Goto(doneLbl))

        val condBlock = explicatePred(cond, thnBlock, elsBlock, basicBlocks)

        basicBlocks(condLbl) = condBlock

        CIr.BasicBlock(Nil, CIr.Goto(condLbl))
    }
  }

  /** Helper to convert a LMonIf expression node into the corresponding CIf.Expr
    * node.
    *
    * This function handles only those expression forms that directly map to
    * CIf:
    *   - `UnaryNumericOp` → `CIf.UnaryNumericOp`
    *   - `BinaryNumericOp` → `CIf.BinaryNumericOp`
    *   - `Compare` → `CIf.Compare`
    *   - `ReadIntCall` → `CIf.ReadIntCall`
    *   - `AtomExpr(a)` → `CIf.AtomExpr(a)`
    *
    * Any other form (e.g. nested `IfExpr`, `Begin`, or `UnaryLogicOp`) cannot
    * be directly converted and triggers an error. Such forms must be handled by
    * `explicateAssign`, `explicatePred`, or `explicateEffect` instead, which
    * ensure that all subexpressions are in “simple” (atomic or comparison) form
    * before calling this helper.
    *
    * @param expr
    *   The LMonIf.Expr to convert.
    * @return
    *   The equivalent CIf.Expr node.
    * @throws RuntimeException
    *   if `expr` cannot be directly converted.
    */
  def convertExprToCIf(expr: LMon.Expr): CIr.Expr = expr match
    case LMon.UnaryNumericOp(op, a) => CIr.UnaryNumericOp(op, a)
    case LMon.BinaryNumericOp(op, lhs, rhs) =>
      CIr.BinaryNumericOp(op, lhs, rhs)
    case LMon.Compare(cmp, lhs, rhs) => CIr.Compare(cmp, lhs, rhs)
    case LMon.ReadIntCall            => CIr.ReadIntCall
    case LMon.AtomExpr(a)            => CIr.AtomExpr(a)
    case _ => sys error "cannot directly convert expression: " + expr

  /** The top‐level pass that translates an entire LMonIf.Module into a
    * CIf.CProgram.
    *
    *   1. Create an initially empty mutable map `basicBlocks` to collect all
    *      generated BasicBlocks. Fold‐right over the module’s statements, using
    *      `foldRight(CIf.BasicBlock(Nil, Return(0))) { (stmt, cont) =>
    *      explicateStmt(stmt, cont, basicBlocks) }` so that each top‐level
    *      statement is translated (via `explicateStmt`) into a chain of
    *      BasicBlocks, ultimately ending in a block whose tail is `Return(0)`.
    *      The result of the fold (`entryBlock`) is the first block to execute
    *      for the program. Assign the final `entryBlock` under the label
    *      `"start"` in `basicBlocks`, marking the program entry. Return
    *      `CIf.CProgram(basicBlocks.toMap)`, which contains every label →
    *      BasicBlock mapping.
    *
    * In the resulting CIf.CProgram:
    *   - The entry point is `"start"`.
    *   - Every BasicBlock in the map has a list of simple CIf.Statements
    *     (`stmts`) and a tail (`Goto`, `If`, or `Return`).
    *   - Control flows exclusively via explicit jumps (`Goto`) or conditional
    *     jumps (`If`).
    *
    * @param module
    *   The parsed LMonIf.Module to translate.
    * @return
    *   The equivalent CIf.CProgram, representing the same program with explicit
    *   control flow.
    */
  def explicateControl(module: LMon.Module): CIr.CProgram = {
    val basicBlocks = mutable.Map[String, CIr.BasicBlock]()

    // construct start block with Return(0) as tail
    val startBlock = module.stmts.foldRight(
      CIr.BasicBlock(List.empty, CIr.Return(CIr.AtomExpr(Constant(0L))))
    ) { (stmt, cont) => explicateStmt(stmt, cont, basicBlocks) }

    // add startBlock
    val startLabel = "start"
    basicBlocks(startLabel) = startBlock

    CIr.CProgram(basicBlocks.toMap)
  }
}
