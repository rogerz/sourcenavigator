// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class Variable extends Location implements java.util.Enumeration {

  /* Variables in a Scope are linked together in a linked list. */
  Variable next;
  public final Variable nextVar () { return next; }
  public final boolean hasMoreElements () { return next != null; }
  public Object nextElement ()
  {
    if (next == null)
      throw new java.util.NoSuchElementException("Variable enumeration");
    return next;
  }

  private int flags = SIMPLE_FLAG;
  /* The SIMPLE_FLAG records the isSimple (q.v.) state. */
  private static final int SIMPLE_FLAG = 0x1;
  /* The PARAMETER_FLAG bit is true for parameters. */
  private static final int PARAMETER_FLAG = 0x2;
  /* The ARTIFICIAL_FLAG bits marks internals variables.
     PARAMETER_FLAG|ARTIFICIAL_FLAG means an incoming parameter. */
  private static final int ARTIFICIAL_FLAG = 0x4;

  static final int UNASSIGNED = -1;
  /** The local variable slot number used by this variable.
   * Not used (by the codegen layer) if !isSimple(). */
  public int offset = UNASSIGNED;
  /** Returns true iff assigned to a local variable slot.
   * Only relevant if isSimple (). */
  public final boolean isAssigned () { return offset != UNASSIGNED; }

  int start_pc;
  int end_pc;

  final boolean dead () { return end_pc > 0; }

  /** Returns true for a "simple" variable.
   * A "simple" Variable can be stack-allocated using standard local
   * variable slots.  It is allocated by the codegen package.
   * A non-simple variable may need heap allocation, or more more
   * complex access;  it is basically ignored by the codegen package,
   * and must be managed by higher layers.  */
  public final boolean isSimple ()
  {
    return (flags & SIMPLE_FLAG) != 0;
  }
  
  public final void setSimple (boolean simple)
  {
    if (simple)
      flags |= SIMPLE_FLAG;
    else
      flags &= ~SIMPLE_FLAG;
  }

  public final boolean isParameter ()
  {
    return (flags & PARAMETER_FLAG) != 0;
  }
  
  public final void setParameter (boolean parameter)
  {
    if (parameter)
      flags |= PARAMETER_FLAG;
    else
      flags &= ~PARAMETER_FLAG;
  }

  public final boolean isArtificial ()
  {
    return (flags & ARTIFICIAL_FLAG) != 0;
  }
  
  public final void setArtificial (boolean artificial)
  {
    if (artificial)
      flags |= ARTIFICIAL_FLAG;
    else
      flags &= ~ARTIFICIAL_FLAG;
  }

  /** Assign a local variable to a given local variable slot.
   * @param local the index of  the local variables.
   * @return true iff we succeeded (i.e. the slot was unused) */
  public boolean reserveLocal (int varIndex, CodeAttr code)
  {
    int size = getType().size > 4 ? 2 : 1;
    if (code.locals.used == null)
      code.locals.used = new Variable[20+size];
    else if (code.getMaxLocals() + size >= code.locals.used.length) {
      Variable[] new_locals = new Variable [2 * code.locals.used.length + size];
      System.arraycopy (code.locals.used, 0, new_locals, 0, code.getMaxLocals());
      code.locals.used = new_locals;
    }
    for (int j = 0; j < size; j++)
      {
	if (code.locals.used[varIndex+j] != null)
	  return false;
      }
    for (int j = 0; j < size; j++)
      code.locals.used[varIndex + j] = this;
    if (varIndex + size > code.getMaxLocals())
      code.setMaxLocals(varIndex + size);
    offset = varIndex;
    return true;
  }

  /**
   * Allocate slots for a local variable (or parameter).
   * @return the index of the (first) slot.
   */
  public void allocateLocal (CodeAttr code)
  {
    for (int i = 0; ; i++)
      {
	if (reserveLocal (i, code))
	  {
	    return;
	  }
      }
  }

  public void freeLocal (CodeAttr code)
  {
    end_pc = code.PC;
    int size = getType().size > 4 ? 2 : 1;
    while (--size >= 0)
      code.locals.used [offset + size] = null;
  }


  
}
