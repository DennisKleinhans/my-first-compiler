package compiler

import CommonNodes.Identifier

type Locations = Map[Identifier, Int]

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
def assignHomes(instrs: List[x86VarIf.Instr]): (List[x86.Instr], Int) = {
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

/** Translates an abstract argument into a concrete one, allocating a new stack
  * slot for variables if needed.
  *
  * @param arg
  *   The x86Var argument (Immediate, Register, or Variable).
  * @param locations
  *   The current mapping of variable identifiers to stack offsets.
  * @param nextFreeOffset
  *   The next free offset to assign (must be <= 0, decreasing).
  * @return
  *   A tuple containing the translated argument, the updated locations map, and
  *   the updated next free offset.
  */
private def assignArg(
    arg: x86VarIf.Arg,
    locations: Locations,
    nextFreeOffset: Int
): (x86.Location | x86.Immediate, Locations, Int) = arg match {
  case x86VarIf.Immediate(n) =>
    // Constants are translated directly
    (x86.Immediate(n), locations, nextFreeOffset)

  case x86VarIf.Variable(id) =>
    // If the variable has already been assigned a stack slot, reuse it
    locations.get(id) match {
      case Some(offset) =>
        (x86.Deref(x86.Reg.Rbp, offset), locations, nextFreeOffset)
      case None =>
        // Assign a new slot: stack grows downward in memory
        val newOffset = nextFreeOffset - 8
        val updatedLocations = locations.updated(id, newOffset)
        (x86.Deref(x86.Reg.Rbp, newOffset), updatedLocations, newOffset)
    }

  case reg: x86.Reg =>
    // Registers are left unchanged
    (reg, locations, nextFreeOffset)
}

/** Resolves a variable location to its stack offset, allocating a new slot if
  * necessary.
  *
  * @param loc
  *   The x86Var location (Variable or Register).
  * @param locations
  *   The current mapping of variable identifiers to stack offsets.
  * @param nextFreeOffset
  *   The next free offset to assign (must be <= 0, decreasing).
  * @return
  *   A tuple containing the resolved x86 location, updated locations map, and
  *   updated next free offset.
  */
def assignLocation(
    loc: x86VarIf.Location,
    locations: Locations,
    nextFreeOffset: Int
): (x86.Location, Locations, Int) = loc match {
  case x86VarIf.Variable(id) =>
    locations.get(id) match {
      case Some(offset) =>
        (x86.Deref(x86.Reg.Rbp, offset), locations, nextFreeOffset)
      case None =>
        val newOffset = nextFreeOffset - 8
        val updated = locations.updated(id, newOffset)
        (x86.Deref(x86.Reg.Rbp, newOffset), updated, newOffset)
    }
  case reg: x86.Reg =>
    (reg, locations, nextFreeOffset)
}

/** Translates a single instruction from x86Var to x86, resolving all arguments.
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
    instr: x86VarIf.Instr,
    locations: Locations,
    nextFreeOffset: Int
): (x86.Instr, Locations, Int) = instr match {
  case x86VarIf.MovQ(src, dest) =>
    // Process both source and destination arguments
    val (srcArg, updatedLocations1, offset1) =
      assignArg(src, locations, nextFreeOffset)
    val (destLoc, updatedLocations2, offset2) =
      assignLocation(dest, updatedLocations1, offset1)
    (x86.Instr.MovQ(srcArg, destLoc), updatedLocations2, offset2)

  case x86VarIf.AddQ(src, dest) =>
    val (srcArg, updatedLocations1, offset1) =
      assignArg(src, locations, nextFreeOffset)
    val (destLoc, updatedLocations2, offset2) =
      assignLocation(dest, updatedLocations1, offset1)
    (x86.Instr.AddQ(srcArg, destLoc), updatedLocations2, offset2)

  case x86VarIf.SubQ(src, dest) =>
    val (srcArg, updatedLocations1, offset1) =
      assignArg(src, locations, nextFreeOffset)
    val (destLoc, updatedLocations2, offset2) =
      assignLocation(dest, updatedLocations1, offset1)
    (x86.Instr.SubQ(srcArg, destLoc), updatedLocations2, offset2)

  case x86VarIf.NegQ(arg) =>
    val (destLoc, updatedLocations, updatedOffset) =
      assignLocation(arg, locations, nextFreeOffset)
    (x86.Instr.NegQ(destLoc), updatedLocations, updatedOffset)

  case x86VarIf.CallQ(label, arity) =>
    // Calls do not need variable resolution
    (x86.Instr.CallQ(label, arity), locations, nextFreeOffset)

  case x86VarIf.PushQ(arg) =>
    val (resolvedArg, updatedLocations, updatedOffset) =
      assignArg(arg, locations, nextFreeOffset)
    (x86.Instr.PushQ(resolvedArg), updatedLocations, updatedOffset)

  case x86VarIf.PopQ(arg) =>
    val (resolvedArg, updatedLocations, updatedOffset) =
      assignArg(arg, locations, nextFreeOffset)
    (x86.Instr.PopQ(resolvedArg), updatedLocations, updatedOffset)

  case x86VarIf.RetQ =>
    (x86.Instr.RetQ, locations, nextFreeOffset)
}
