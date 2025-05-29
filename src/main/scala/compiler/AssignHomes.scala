package compiler

import CommonNodes.Identifier

/** Transforms a list of x86Var instructions into x86 instructions, assigning
  * each variable a unique stack location (home) relative to the RBP register.
  *
  * The function ensures that all variables are replaced with appropriate memory
  * accesses (Deref) using negative offsets from RBP. Each variable gets its own
  * unique slot on the stack.
  *
  * @param instrs
  *   The list of x86Var instructions using abstract variables.
  * @return
  *   A list of concrete x86 instructions with resolved memory locations.
  */
def assignHomes(instrs: List[x86Var.Instr]): (List[x86.Instr], Int) = {
  type Locations = Map[Identifier, Int]

  /** Translates an abstract argument into a concrete one, allocating a new
    * stack slot for variables if needed.
    *
    * @param arg
    *   The x86Var argument (Immediate, Register, or Variable).
    * @param locations
    *   The current mapping of variable identifiers to stack offsets.
    * @param nextFreeOffset
    *   The next free offset to assign (must be <= 0, decreasing).
    * @return
    *   A tuple containing the translated argument, the updated locations map,
    *   and the updated next free offset.
    */
  def assignArg(
      arg: x86Var.Arg,
      locations: Locations,
      nextFreeOffset: Int
  ): (x86.Arg, Locations, Int) = arg match {
    case x86Var.Immediate(n) =>
      // Constants are translated directly
      (x86.Arg.Immediate(n), locations, nextFreeOffset)

    case x86Var.Variable(id) =>
      // If the variable has already been assigned a stack slot, reuse it
      locations.get(id) match {
        case Some(offset) =>
          (x86.Arg.Deref(x86.Reg.Rbp, offset), locations, nextFreeOffset)
        case None =>
          // Assign a new slot: stack grows downward in memory
          val newOffset = nextFreeOffset - 8
          val updatedLocations = locations.updated(id, newOffset)
          (x86.Arg.Deref(x86.Reg.Rbp, newOffset), updatedLocations, newOffset)
      }

    case x86Var.Register(reg) =>
      // Registers are left unchanged
      (x86.Arg.Register(reg), locations, nextFreeOffset)
  }

  /** Translates a single instruction from x86Var to x86, resolving all
    * arguments.
    *
    * @param instr
    *   The abstract instruction.
    * @param locations
    *   The current variable-to-offset map.
    * @param nextFreeOffset
    *   The next free offset to assign on the stack.
    * @return
    *   A tuple containing the concrete instruction, updated locations, and
    *   updated next offset.
    */
  def assignInstr(
      instr: x86Var.Instr,
      locations: Locations,
      nextFreeOffset: Int
  ): (x86.Instr, Locations, Int) = instr match {
    case x86Var.MovQ(src, dest) =>
      // Process both source and destination arguments
      val (srcArg, updatedLocations1, offset1) =
        assignArg(src, locations, nextFreeOffset)
      val (destArg, updatedLocations2, offset2) =
        assignArg(dest, updatedLocations1, offset1)
      (x86.Instr.MovQ(srcArg, destArg), updatedLocations2, offset2)

    case x86Var.AddQ(src, dest) =>
      val (srcArg, updatedLocations1, offset1) =
        assignArg(src, locations, nextFreeOffset)
      val (destArg, updatedLocations2, offset2) =
        assignArg(dest, updatedLocations1, offset1)
      (x86.Instr.AddQ(srcArg, destArg), updatedLocations2, offset2)

    case x86Var.SubQ(src, dest) =>
      val (srcArg, updatedLocations1, offset1) =
        assignArg(src, locations, nextFreeOffset)
      val (destArg, updatedLocations2, offset2) =
        assignArg(dest, updatedLocations1, offset1)
      (x86.Instr.SubQ(srcArg, destArg), updatedLocations2, offset2)

    case x86Var.NegQ(arg) =>
      val (resolvedArg, updatedLocations, updatedOffset) =
        assignArg(arg, locations, nextFreeOffset)
      (x86.Instr.NegQ(resolvedArg), updatedLocations, updatedOffset)

    case x86Var.CallQ(label, arity) =>
      // Calls do not need variable resolution
      (x86.Instr.CallQ(label, arity), locations, nextFreeOffset)

    case x86Var.PushQ(arg) =>
      val (resolvedArg, updatedLocations, updatedOffset) =
        assignArg(arg, locations, nextFreeOffset)
      (x86.Instr.PushQ(resolvedArg), updatedLocations, updatedOffset)

    case x86Var.PopQ(arg) =>
      val (resolvedArg, updatedLocations, updatedOffset) =
        assignArg(arg, locations, nextFreeOffset)
      (x86.Instr.PopQ(resolvedArg), updatedLocations, updatedOffset)

    case x86Var.RetQ =>
      (x86.Instr.RetQ, locations, nextFreeOffset)
  }

  // Initial values: empty instruction list, empty locations map, and offset starting at -8
  val (finalInstrs, _, finalOffset) =
    instrs.foldLeft((List.empty[x86.Instr], Map.empty[Identifier, Int], -8)) {
      case ((translatedInstrs, currentLocations, currentOffset), instr) =>
        val (translatedInstr, updatedLocations, updatedOffset) =
          assignInstr(instr, currentLocations, currentOffset)
        (translatedInstrs :+ translatedInstr, updatedLocations, updatedOffset)
    }
  val stackSpace = -finalOffset
  (finalInstrs, stackSpace)
}
