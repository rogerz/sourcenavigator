// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/**
 * A Label represents a location in a Code attribute.
 */

/*
 * Java VM control transfer instructions contain PC-relative offsets
 * to the target (location/Label).  (The offsets are relative to the
 * start of the instruction.)  A complication is that some instructions
 * use 2-byte relative offsets, while others use 4-byte relative offsets.
 * A few instructions exist in both "narrow" 2-byte and "wide" 4-byte forms.
 *
 * This library uses a very simple data structure to accumulate
 * generated instructions - just a simple byte array of opcodes and
 * operands, just as in the final .class file.  When emitting a forward
 * branch, we cannot emit the relative addresss of the still-unknown
 * target.  Instead, we emit the negative of the PC of the instruction,
 * and later add the address of the target by back-patching.
 * We keep track of which locations need back-patching in the fixups and
 * wide_fixups arrays (for 2-byte and 4-byte offsets, respectively).
 * This is simple, fast, and concise.  However, if it turns out that
 * the target is more than 32767 bytes away, we have problem:  Once an
 * instruction has been emitted, we cannot change its size (e.g. change
 * a 2-byte jump offset into a 4-byte offset).  (I.e. there is no
 * "relaxation" as is done by many assemblers.)
 *
 * Since very few methods actually need 4-byte jump offsets, the code
 * generator optimistically assumes that when it emits a forward branch
 * the not-yet-defined label will later be defined within the 2-byte span
 * of the current instruction.  (Except of course for instructions like
 * tableswitch and lookupswitch that only exist in 4-byte-offset form.)
 *
 * But what do we do if it turns out that the branch target is > 32767
 * bytes away?  In that case, we emit what I call a "spring" *before*
 * we get out of range.  A "spring" is a trivial basic block that
 * contains only a single wide goto.  The branches that are close to
 * the limit are patched to point to the "spring" instead.  Before
 * every instruction Method.instruction_start_hook is called.
 * It checks if there are any pending banches that are getting close
 * to the limit for 2-byte offsets, in which case it calls Label.emit_spring
 * to generate the spring.
 *
 * Note that we may also need a spring even if the target is known,
 * if we need a 2-byte offset, and the target is too far away.
 *
 * Note that while the Java bytecodes supports 32-bit relative address,
 * there are limitations in the .class file format that limit each
 * method to 2*16 bytes.  This may change.  In any case, this code has
 * been written to support "wide jumps" - but that has not been tested!
 */

public class Label {
  Label next;

  // The PC of where the label is, or -1 if not yet defined.
  int position;

  // If >= 0, this is the location of a "spring" to this Label.
  int spring_position;

  // Array of PC locations that reference this Label.
  // Elements that are -1 provide room to grow.
  // For an element fi in fixups (where fi >= 0), the 2-byte
  // sequence code[fi]:code[fi+1] will need to be adjusted (relocated) by
  // the relative position of this Label, or alternatively spring_postion.
  // After relcocation, code[fi]:code[fi+1] will be a reference to 
  // this label (usually a PC-relative reference).
  // The lowest element fi (>= 0) is first, but fixups is otherwise unsorted.
  // Normally, if defined(), then fixups is null.
  // However, we may still need fixups even if defined(), if
  // position is further away than allowed by a 16-bit offset.
  int[] fixups;

  // Similar to fixups, but each element is a 4-byte relative jump offset.
  int[] wide_fixups;

  public final boolean defined () { return position >= 0; }

  public Label (Method method)
  {
    this(method.code);
  }

  public Label (CodeAttr code)
  {
    position = -1;
    if (code.labels != null)
      {
	next = code.labels.next;
	code.labels.next = this;
      }
    else
      code.labels = this;
  }

  /* Make all narrow fixups for this label point (relatively) to target. */
  final private void relocate_fixups (CodeAttr code, int target)
 {
    if (fixups == null)
      return;
    for (int i = fixups.length; --i >= 0; )
      {
	int pos = fixups[i];
	/* Adjust the 2-byte offset at pos in method by (target-pos). */
	if (pos >= 0)
	  {
	    byte[] insns = code.getCode();
	    int code_val = ((insns[pos] & 0xFF) << 8) | (insns[pos+1] & 0xFF);
	    code_val += target - pos;
	    if (code_val < -32768 || code_val >= 32768)
	      throw new Error ("overflow in label fixup");
	    insns[pos] = (byte) (code_val >> 8);
	    insns[pos+1] = (byte) (code_val);
	  }
      }
    if (this != code.labels)
      code.reorder_fixups ();
    fixups = null;
  }

  /* Adjust the 4-byte offset at pos in method by (target-pos). */
  static final void relocate_wide (CodeAttr code, int pos, int target) {
    if (pos >= 0) {
      byte[] insns = code.getCode();
      int code_val
	= ((insns[pos] & 0xFF) << 24) | ((insns[pos+1] & 0xFF) << 16)
	| ((insns[pos+2] & 0xFF) << 8) | (insns[pos+3] & 0xFF);
      code_val += target-pos;
      insns[pos] = (byte) (code_val >> 24);
      insns[pos+1] = (byte) (code_val >> 16);
      insns[pos+2] = (byte) (code_val >> 8);
      insns[pos+3] = (byte) code_val;
    }
  }

  /**
   * Define the value of a label as having the current location.
   * @param code the "Code" attribute of the current method
   */
  public void define (CodeAttr code)
  {
    code.unreachable_here = false;
    if (position >= 0)
      throw new Error ("label definition more than once");

    position = code.PC;

    /* Remove redundant goto in:  goto L; L:
       These tend to be generated when compiling IfExp's,
       so it is worth checking for them.
    */
    int goto_pos = position - 3;  // Position of hypothetical goto.
    if (code.readPC <= goto_pos
        && fixups != null && wide_fixups == null
	&& goto_pos > 0 && code.getCode()[goto_pos] == (167-256) /* goto */)
      {
	int i;
	goto_pos++;  // Skip goto opcode.
	for (i = fixups.length;  -- i >= 0; )
	  {
	    if (fixups[i] == goto_pos)
	      break;
	  }
	if (i >= 0)
	  {
	    position -= 3;
	    code.PC = position;
	    fixups[i] = -1;
	  }
      }

    code.readPC = position;
    relocate_fixups (code, position);

    if (wide_fixups != null) {
      for (int i = wide_fixups.length; --i >= 0; )
	relocate_wide (code, wide_fixups[i], position);
      wide_fixups = null;
    }
  }

  /**
   * Emit goto_w as target for far-away gotos in large methods.
   * Needs to be invoked if the earliest still-pending fixup
   * is at the limit of a 2-byte relative jump.
   * To solve the problem, we emit a goto_w for the label,
   * and change all pending (short) fixups to point here.
   * We also have to jump past this goto.
   */
   
  void emit_spring (CodeAttr code)
  {
    code.reserve(8);
    if (!code.unreachable_here)
      {
	code.put1 (167);  // goto PC+6
	code.put2 (6);
      }
    spring_position = code.PC;
    relocate_fixups (code, spring_position);
    code.put1 (200);  // goto_w
    add_wide_fixup (code.PC);
    code.put4 (1);
    code.readPC = code.PC;
  }

  /* Save a fixup so we can later backpatch code[method.PC..method.PC+1]. */
  private void add_fixup (CodeAttr code)
  {
    int PC = code.PC;
    int i;
    if (fixups == null)
      {
	fixups = new int[2];
	fixups[0] = PC;
	fixups[1] = -1;
      }
    else
      {
	int fixups_length = fixups.length;
	for (i = 0; i < fixups_length && fixups[i] >= 0; ) i++;
	if (i == fixups_length)
	  {
	    int[] new_fixups = new int[2 * fixups.length];
	    System.arraycopy (fixups, 0, new_fixups, 0, fixups.length);
	    i = new_fixups.length;
	    do { new_fixups[--i] = -1; } while (i > fixups_length);
	    fixups = new_fixups;
	  }
	if (PC < fixups[0])
	  {
	    fixups[i] = fixups[0];
	    fixups[0] = PC;
	  }
	else
	  fixups[i] = PC;
      }
    if (this != code.labels
	&& (code.labels.fixups == null
	    || PC < code.labels.fixups[0]))
      code.reorder_fixups ();
  }
  
  private void add_wide_fixup (int PC)
  {
    int i;
    if (wide_fixups == null) {
      wide_fixups = new int[2];
      wide_fixups[0] = PC;
      wide_fixups[1] = -1;
      return;
    }
    for (i = 0; i < wide_fixups.length; i++ ) {
      if (wide_fixups[i] < 0) {
	wide_fixups[i] = PC;
	return;
      }
    }
    int[] new_fixups = new int[2 * wide_fixups.length];
    System.arraycopy (wide_fixups, 0, new_fixups, 0, wide_fixups.length);
    new_fixups[wide_fixups.length] = PC;
    for (i = wide_fixups.length; ++i < new_fixups.length; )
      new_fixups[i] = -1;
    wide_fixups = new_fixups;
  }

  /**
   * Emit a reference to the current label.
   * @param method the current method
   * Emit the reference as a 2-byte difference relative to PC-1.
   */
   
  public void emit (CodeAttr code) {
    int PC_rel = 1 - code.PC;
    int delta = PC_rel;
    if (defined ()) {
      delta += position;
      if (delta < -32768) {
	if (spring_position >= 0 && spring_position + PC_rel >= -32768)
	  delta = spring_position + PC_rel;
	else {
	  add_fixup (code);
	  delta = PC_rel;
	}
      }
    } else
      add_fixup (code);
    code.put2 (1);
  }

  /**
   * Emit a wide reference to the current label.
   * @param method the current method
   * Emit the reference as a 4-byte difference relative to PC-offset.
   */
  public void emit_wide (CodeAttr code, int start_offset)
  {
    int delta = 1 - code.PC;
    if (defined ())
      delta += position;
    else
      add_wide_fixup (code.PC);
    code.put4 (start_offset);
  }
}
