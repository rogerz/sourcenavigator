// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/**
  * Represents the contents of a standard "Code" attribute.
  * <p>
  * Most of the actual methods that generate bytecode operation
  * are in this class (typically with names starting with <code>emit</code>),
  * though there are also some in <code>Method</code>.
  * <p>
  * Note that a <code>CodeAttr</code> is an <code>Attribute</code>
  * of a <code>Method</code>, and can in turn contain other
  * <code>Attribute</code>s, such as a <code>LineNumbersAttr</code>.
  *
  * @author      Per Bothner
  */

public class CodeAttr extends Attribute implements AttrContainer
{
  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
  { this.attributes = attributes; }
  LineNumbersAttr lines;
  LocalVarsAttr locals;

  private Type[] stack_types;
  int SP;  // Current stack size (in "words")
  private int max_stack;
  private int max_locals;
  int PC;
  // readPC (which is <= PC) is a bound on locations that have been
  // saved into labels or otherwise externally seen.
  // Hence, we cannot re-arrange code upto readPC, but we can
  // rearrange code between readPC and PC.
  int readPC;
  byte[] code;

  /* The exception handler table, as a vector of quadruples
     (start_pc, end_pc, handler_pc, catch_type).
     Only the first exception_table_length quadrules are defined. */
  short[] exception_table;

  /* The number of (defined) exception handlers (i.e. quadruples)
     in exception_table. */
  int exception_table_length;

  /* A chain of labels.  Unsorted, except that the Label with
     the lowest element in fixups must be the first one. */
  Label labels;

  /** The stack of currently active conditionals. */
  IfState if_stack;

  /** The stack of currently active try statements. */
  TryState try_stack;

  public final Method getMethod() { return (Method) getContainer(); }

  public final ConstantPool getConstants ()
  {
    return getMethod().classfile.constants;
  }

  /* True if we cannot fall through to bytes[PC] -
     the previous instruction was an uncondition control transfer.  */
  boolean unreachable_here;
  /** True if control could reach here. */
  public boolean reachableHere () { return !unreachable_here; }

  /** Get the maximum number of words on the operand stack in this method. */
  public int getMaxStack() { return max_stack; }
  /** Get the maximum number of local variable words in this method. */
  public int getMaxLocals() { return max_locals; }

  /** Set the maximum number of words on the operand stack in this method. */
  public void setMaxStack(int n) { max_stack = n; }
  /** Set the maximum number of local variable words in this method. */
  public void setMaxLocals(int n) { max_locals = n; }

  /** Get the code (instruction bytes) of this method.
    * Does not make a copy. */
  public byte[] getCode() { return code; }
  /** Set the code (instruction bytes) of this method.
    * @param code the code bytes (which are not copied).
    * Implicitly calls setCodeLength(code.length). */
  public void setCode(byte[] code) {
    this.code = code; this.PC = code.length; readPC = PC; }
  /** Set the length the the code (instruction bytes) of this method.
    * That is the number of current used bytes in getCode().
    * (Any remaing bytes provide for future growth.) */
  public void setCodeLength(int len) { PC = len; readPC = len;}
  /** Set the current lengthof the code (instruction bytes) of this method. */
  public int getCodeLength() { readPC = PC;  return PC; }

  public CodeAttr (Method meth)
  {
    super ("Code");
    setContainer(meth);
    setNext(meth.getAttributes());
    meth.setAttributes(this);
  }

  public final void reserve (int bytes)
  {
    if (code == null)
      code = new byte[100+bytes];
    else if (PC + bytes > code.length)
      {
	byte[] new_code = new byte[2 * code.length + bytes];
	System.arraycopy (code, 0, new_code, 0, PC);
	code = new_code;
      }

    while (labels != null && labels.fixups != null) {
      int oldest_fixup = labels.fixups[0];
      int threshold = unreachable_here ? 30000 : 32000;
      if (PC + bytes - oldest_fixup > threshold)
	labels.emit_spring (this);
      else
	break;
    }
  }

  /**
   * Write an 8-bit byte to the current code-stream.
   * @param i the byte to write
   */
  public final void put1(int i)
  {
    code[PC++] = (byte) i;
    unreachable_here = false;
  }

  /**
   * Write a 16-bit short to the current code-stream
   * @param i the value to write
   */
  public final void put2(int i)
  {
    code[PC++] = (byte) (i >> 8);
    code[PC++] = (byte) (i);
    unreachable_here = false;
  }
  /**
   * Write a 32-bit int to the current code-stream
   * @param i the value to write
   */
  public final void put4(int i)
  {
    code[PC++] = (byte) (i >> 24);
    code[PC++] = (byte) (i >> 16);
    code[PC++] = (byte) (i >> 8);

    code[PC++] = (byte) (i);
    unreachable_here = false;
  }

  public final void putIndex2 (CpoolEntry cnst)
  {
    put2(cnst.index);
  }

  public final void putLineNumber (int linenumber)
  {
    if (lines == null)
      lines = new LineNumbersAttr(this);
    readPC = PC;
    lines.put(linenumber, PC);
  }

  public final void pushType(Type type)
  {
    if (type == Type.void_type)
      throw new Error ("pushing void type onto stack");
    if (stack_types == null)
      stack_types = new Type[20];
    else if (SP + 1 >= stack_types.length) {
      Type[] new_array = new Type[2 * stack_types.length];
      System.arraycopy (stack_types, 0, new_array, 0, SP);
      stack_types = new_array;
    }
    if (type.size == 8)
      stack_types[SP++] = Type.void_type;
    stack_types[SP++] = type;
    if (SP > max_stack)
      max_stack = SP;
  }

  public final Type popType ()
  {
    if (SP <= 0)
      throw new Error("popType called with empty stack");
    Type type = stack_types[--SP];
    if (type.size == 8)
      if (popType () != Type.void_type)
	throw new Error("missing void type on stack");
    return type;
  }

  public final Type topType ()
  {
    return stack_types[SP - 1];
  }

  /** Compile code to pop values off the stack (and ignore them).
   * @param nvalues the number of values (not words) to pop
   */
  public void emitPop (int nvalues)
  {
    for ( ; nvalues > 0;  --nvalues)
      {
        reserve(1);
	Type type = popType();
	if (type.size > 4)
	  put1(88);  // pop2
	else if (nvalues > 1)
	  { // optimization:  can we pop 2 4-byte words using a pop2
	    Type type2 = popType();
	    if (type2.size > 4)
	      {
		put1(87);  // pop
		reserve(1);
	      }
	    put1(88);  // pop2
	    --nvalues;
	  }
	else
	  put1(87); // pop
      }
  }

  public void emitSwap ()
  {
    reserve(1);
    Type type1 = popType();
    Type type2 = popType();
    if (type1.size > 4 || type2.size > 4)
      throw new Error ("emitSwap:  not allowed for long or double");
    pushType(type1);
    put1(95);  // swap
    pushType(type2);
  }

  /** Compile code to duplicate with offset.
   * @param size the size of the stack item to duplicate (1 or 2)
   * @param offset where to insert the result (must be 0, 1, or 2)
   * The new words get inserted at stack[SP-size-offset]
   */
  public void emitDup (int size, int offset)
  {
    if (size == 0)
      return;
    reserve(1);
    // copied1 and (optionally copied2) are the types of the duplicated words
    Type copied1 = popType ();
    Type copied2 = null;
    if (size == 1)
      {
	if (copied1.size > 4)
	  throw new Error ("using dup for 2-word type");
      }
    else if (size != 2)
      throw new Error ("invalid size to emitDup");
    else if (copied1.size <= 4)
      {
	copied2 = popType();
	if (copied2.size > 4)
	  throw new Error ("dup will cause invalid types on stack");
      }

    int kind;
    // These are the types of the words (in any) that are "skipped":
    Type skipped1 = null;
    Type skipped2 = null;
    if (offset == 0)
      {
	kind = size == 1 ? 89 : 92;  // dup or dup2
      }
    else if (offset == 1)
      {
	kind = size == 1 ? 90 : 93; // dup_x1 or dup2_x1
	skipped1 = popType ();
	if (skipped1.size > 4)
	  throw new Error ("dup will cause invalid types on stack");
      }
    else if (offset == 2)
      {
	kind = size == 1 ? 91 : 94; // dup_x2 or dup2_x2
	skipped1 = popType();
	if (skipped1.size <= 4)
	  {
	    skipped2 = popType();
	    if (skipped2.size > 4)
	      throw new Error ("dup will cause invalid types on stack");
	  }
      }
    else
      throw new Error ("emitDup:  invalid offset");

    put1(kind);
    if (copied2 != null)
      pushType(copied2);
    pushType(copied1);
    if (skipped2 != null)
      pushType(skipped2);
    if (skipped1 != null)
      pushType(skipped1);
    if (copied2 != null)
      pushType(copied2);
    pushType(copied1);
  }

  /**
   * Compile code to duplicate the top 1 or 2 words.
   * @param size number of words to duplicate
   */
  public void emitDup (int size)
  {
    emitDup(size, 0);
  }

  public void emitDup (Type type)
  {
    emitDup(type.size > 4 ? 2 : 1, 0);
  }

  public void enterScope (Scope scope)
  {
    locals.enterScope(scope);
  }

  public Scope pushScope () {
    Scope scope = new Scope ();
    scope.start_pc = PC;
    readPC = PC;
    if (locals == null)
      locals = new LocalVarsAttr(this);
    locals.enterScope(scope);
    if (locals.parameter_scope == null) 
      locals.parameter_scope= scope;
    return scope;
  }


  public Scope popScope () {
    Scope scope = locals.current_scope;
    locals.current_scope = scope.parent;
    scope.end_pc = PC;  readPC = PC;
    for (Variable var = scope.vars; var != null; var = var.next) {
      if (var.isSimple () && ! var.dead ())
	var.freeLocal(this);
    }
    return scope;
  }

  /** Get the index'th parameter. */
  public Variable getArg (int index)
  {
    return locals.parameter_scope.find_var (index);
  }

  /**
   * Search by name for a Variable
   * @param name name to search for
   * @return the Variable, or null if not found (in any scope of this Method).
   */
  Variable lookup (String name)
  {
    Scope scope = locals.current_scope;
    for (; scope != null;  scope = scope.parent)
      {
	Variable var = scope.lookup (name);
	if (var != null)
	  return var;
      }
    return null;
  }

  /** Add a new local variable (in the current scope).
   * @param type type of the new Variable.
   * @return the new Variable. */
  public Variable addLocal (Type type)
  {
    return locals.current_scope.addVariable(this, type, null);
  }

  /** Add a new local variable (in the current scope).
   * @param type type of the new Variable.
   * @param name name of the new Variable.
   * @return the new Variable. */
  Variable addLocal (Type type, String name)
  {
    return locals.current_scope.addVariable (this, type, name);
  }

  public final void emitPushConstant(int val, Type type)
  {
    switch (type.getSignature().charAt(0))
      {
      case 'B':  case 'C':  case 'I':  case 'Z':  case 'S':
	emitPushInt(val);  break;
      case 'J':
	emitPushLong((long)val);  break;
      case 'F':
	emitPushFloat((float)val);  break;
      case 'D':
	emitPushDouble((double)val);  break;
      default:
	throw new Error("bad type to emitPushConstant");
      }
  }

  public final void emitPushConstant (CpoolEntry cnst)
  {
    reserve(3);
    int index = cnst.index;
    if (cnst instanceof CpoolValue2)
      {
      	put1 (20); // ldc2w
	put2 (index);
      }
    else if (index < 256)
      {
	put1(18); // ldc1
	put1(index);
      }
    else
      {
	put1(19); // ldc2
	put2(index);
      }
  }

  public final void emitPushInt(int i)
  {
    reserve(3);
    if (i >= -1 && i <= 5)
      put1(i + 3);  // iconst_m1 .. iconst_5
    else if (i >= -128 && i < 128)
      {
	put1(16); // bipush
	put1(i);
      }
    else if (i >= -32768 && i < 32768)
      {
	put1(17); // sipush
	put2(i);
      }
    else
      {
	emitPushConstant(getConstants().addInt(i));
      }
    pushType(Type.int_type);
  }

  public void emitPushLong (long i)
  {
    if (i == 0 || i == 1)
      {
	reserve(1);
	put1 (9 + (int) i);  // lconst_0 .. lconst_1
      }
    else if ((long) (int) i == i)
      {
	emitPushInt ((int) i);
	reserve(1);
	popType();
	put1 (133); // i2l
      }
    else
      {
	emitPushConstant(getConstants().addLong(i));
      }
    pushType(Type.long_type);
  }

  public void emitPushFloat (float x)
  {
    int xi = (int) x;
    if ((float) xi == x && xi >= -128 && xi < 128)
      {
	if (xi >= 0 && xi <= 2)
	  {
	    reserve(1);
	    put1(11 + xi);  // fconst_0 .. fconst_2
	  }
	else
	  {
	    // Saves space in the constant pool
	    // Probably faster, at least on modern CPUs.
	    emitPushInt (xi);
	    reserve(1);
	    popType();
	    put1 (134); // i2f
	  }
      }
    else
      {
	emitPushConstant(getConstants().addFloat(x));
      }
    pushType(Type.float_type);
  }

  public void emitPushDouble (double x)
  {
    int xi = (int) x;
    if ((double) xi == x && xi >= -128 && xi < 128)
      {
	if (xi == 0 || xi == 1)
	  {
	    reserve(1);
	    put1(14+xi);  // dconst_0 or dconst_1
	  }
	else
	  {
	    // Saves space in the constant pool
	    // Probably faster, at least on modern CPUs.
	    emitPushInt (xi);
	    reserve(1);
	    popType();
	    put1 (135); // i2d
	  }
      }
    else
      {
	emitPushConstant(getConstants().addDouble(x));
      }
    pushType(Type.double_type);
  }

  public final void emitPushString (String str)
  {
    emitPushConstant(getConstants().addString(str));
    pushType(Type.string_type);
  }

  public void emitPushNull ()
  {
    reserve(1);
    put1(1);  // aconst_null
    pushType(Type.pointer_type);
  }

  public final void emitPushThis()
  {
    reserve(1);
    put1(42);  // aload_0
    pushType(getMethod().getDeclaringClass());
  }

  void emitNewArray (int type_code)
  {
    reserve(2);
    put1(188);  // newarray
    put1(type_code);
  }

  public final void emitArrayLength ()
  {
    reserve(1);
    put1(190);  // arraylength
    pushType(Type.int_type);
  }

  private int adjustTypedOp  (Type type)
  {
    switch (type.getSignature().charAt(0))
      {
      case 'I':  return 0;  // int
      case 'J':  return 1;  // long
      case 'F':  return 2;  // float
      case 'D':  return 3;  // double
      default:   return 4;  // object
      case 'B':
      case 'Z':  return 5;  // byte or boolean
      case 'C':  return 6;  // char
      case 'S':  return 7;  // short
      }
  }

  private void emitTypedOp (int op, Type type)
  {
    reserve(1);
    put1(op + adjustTypedOp(type));
  }

  /** Store into an element of an array.
   * Must already have pushed the array reference, the index,
   * and the new value (in that order).
   * Stack:  ..., array, index, value => ...
   */
  public void emitArrayStore (Type element_type)
  {
    popType();  // Pop new value
    popType();  // Pop index
    popType();  // Pop array reference
    emitTypedOp(79, element_type);
  }

  /** Load an element from an array.
   * Must already have pushed the array and the index (in that order):
   * Stack:  ..., array, index => ..., value */
  public void emitArrayLoad (Type element_type)
  {
    popType();  // Pop index
    popType();  // Pop array reference
    emitTypedOp(46, element_type);
    pushType(element_type);
  }

  /**
   * Invoke new on a class type.
   * Does not call the constructor!
   * @param type the desired new object type
   */
  public void emitNew (ClassType type)
  {
    reserve(3);
    put1(187); // new
    putIndex2(getConstants().addClass(type));
    pushType(type);
  }

  /** Compile code to allocate a new array.
   * The size shold have been already pushed on the stack.
   * @param type type of the array elements
   */
  public void emitNewArray (Type element_type)
  {
    popType();
    if (element_type instanceof PrimType)
      {
	int code;
	switch (element_type.getSignature().charAt(0))
	  {
	  case 'B':  code =  8;  break;
	  case 'S':  code =  9;  break;
	  case 'I':  code = 10;  break;
	  case 'J':  code = 11;  break;
	  case 'F':  code =  6;  break;
	  case 'D':  code =  7;  break;
	  case 'Z':  code =  4;  break;
	  case 'C':  code =  5;  break;
	  default:   throw new Error("bad PrimType in emitNewArray");
	  }
	emitNewArray(code);
      }
    else if (element_type instanceof ClassType)
      {
	reserve(3);
	put1(189); // anewarray
	putIndex2(getConstants().addClass((ClassType) element_type));
      }
    else
      throw new Error ("unimplemented type in emitNewArray");
    pushType(Type.pointer_type);
  }

  private void emitBinop (int base_code)
  {
    Type type2 = popType().promote();
    Type type1_raw = popType();
    Type type1 = type1_raw.promote();
    if (type1 != type2 || ! (type1 instanceof PrimType))
      throw new Error ("non-matching or bad types in binary operation");
    emitTypedOp(base_code, type1);
    pushType(type1_raw);
  }

  // public final void emitIntAdd () { put1(96); popType();}
  // public final void emitLongAdd () { put1(97); popType();}
  // public final void emitFloatAdd () { put1(98); popType();}
  // public final void emitDoubleAdd () { put1(99); popType();}

  public final void emitAdd () { emitBinop (96); }
  public final void emitSub () { emitBinop (100); }
  public final void emitMul () { emitBinop (104); }
  public final void emitDiv () { emitBinop (108); }
  public final void emitRem () { emitBinop (112); }
  public final void emitAnd () { emitBinop (126); }
  public final void emitIOr () { emitBinop (128); }
  public final void emitXOr () { emitBinop (130); }

  public final void emitNot()
  {
    Type type = topType();  // Must be int or long.
    emitPushConstant(1, type);
    emitAdd();
    emitPushConstant(1, type);
    emitAnd();
  }

  public void emitPrimop (int opcode, int arg_count, Type retType)
  {
    reserve(1);
    while (-- arg_count >= 0)
      popType();
    put1(opcode);
    pushType(retType);
  }

  /**
   * Comple code to push the contents of a local variable onto the statck.
   * @param var The variable whose contents we want to push.
   */
  public final void emitLoad (Variable var)
  {
    if (var.dead())
      throw new Error("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple())
      throw new Error ("attempting to load from unassigned variable "+var
		       +" simple:"+var.isSimple()+", offset: "+offset);
    Type type = var.getType().promote();
    reserve(4);
    int kind = adjustTypedOp(type);
    if (offset <= 3)
      put1(26 + 4 * kind + offset);  // [ilfda]load_[0123]
    else
      {
	if (offset >= 256)
	  {
	    put1(196); // wide
	    put1(offset >> 8);
	  }
	put1(21 + kind);  // [ilfda]load
	put1(offset);
      }
    pushType(var.getType());
  }

  public void emitStore (Variable var)
  {
   if (var.dead ())
      throw new Error ("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple ())
      throw new Error ("attempting to store in unassigned variable");
    Type type = var.getType().promote ();
    reserve(4);
    popType();
    int kind = adjustTypedOp(type);
    if (offset <= 3)
      put1(59 + 4 * kind + offset);  // [ilfda]store_[0123]
    else
      {
	if (offset >= 256)
	  {
	    put1(196); // wide
	    put1(offset >> 8);
	  }
	put1(54 + kind);  // [ilfda]store
	put1(offset);
      }
  }


  private final void emitFieldop (Field field, int opcode)
  {
    reserve(3);
    put1(opcode);
    putIndex2(getConstants().addFieldRef(field));
  }

  /** Compile code to get a static field value.
   * Stack:  ... => ..., value */

  public final void emitGetStatic(Field field)
  {
    pushType(field.type);
    emitFieldop (field, 178);  // getstatic
  }

  /** Compile code to get a non-static field value.
   * Stack:  ..., objectref => ..., value */

  public final void emitGetField(Field field)
  {
    popType();
    pushType(field.type);
    emitFieldop(field, 180);  // getfield
  }

  /** Compile code to put a static field value.
   * Stack:  ..., value => ... */

  public final void emitPutStatic (Field field)
  {
    popType();
    emitFieldop(field, 179);  // putstatic
  }

  /** Compile code to put a non-static field value.
   * Stack:  ..., objectref, value => ... */

  public final void emitPutField (Field field)
  {
    popType();
    popType();
    emitFieldop(field, 181);  // putfield
  }

  public void emitInvokeMethod (Method method, int opcode)
  {
    reserve(opcode == 185 ? 5 : 3);
    int arg_count = method.arg_types.length;
    boolean is_invokestatic = opcode == 184;
    if (is_invokestatic != ((method.access_flags & Access.STATIC) != 0))
      throw new Error
	("emitInvokeXxx static flag mis-match method.flags="+method.access_flags);
    if (!is_invokestatic)
      arg_count++;
    put1(opcode);  // invokevirtual, invokespecial, or invokestatic
    putIndex2(getConstants().addMethodRef(method));
    if (opcode == 185)  // invokeinterface
      {
	put1(arg_count);
	put1(0);
      }
    while (--arg_count >= 0)
      popType();
    if (method.return_type != Type.void_type)
      pushType(method.return_type);
  }

  /** Compile a virtual method call.
   * The stack contains the 'this' object, followed by the arguments in order.
   * @param method the method to invoke virtually
   */
  public void emitInvokeVirtual (Method method)
  {
    emitInvokeMethod(method, 182);  // invokevirtual
  }

  public void emitInvokeSpecial (Method method)
  {
    emitInvokeMethod(method, 183);  // invokespecial
  }

  /** Compile a static method call.
   * The stack contains the the arguments in order.
   * @param method the static method to invoke
   */
  public void emitInvokeStatic (Method method)
  {
    emitInvokeMethod(method, 184);  // invokestatic
  }

  final void emitTransfer (Label label, int opcode)
  {
    put1(opcode);
    label.emit(this);
  }

  /** Compile an unconditional branch (goto) or a jsr.
   * @param label target of the branch (must be in this method).
   */
  public final void emitGoto (Label label, int opcode)
  {
    reserve(5);
    if (label.defined ())
      {
	readPC = PC;
	int delta = label.position - PC;
	if (delta < -32768)
	  {
	    put1(opcode-167);  // goto_w or jsr_w
	    put4(delta);
	  }
	else
	  {
	    put1(opcode); // goto or jsr
	    put2(delta);
	  }
      }
    else
      emitTransfer (label, opcode); // goto label or jsr label
  }

  /** Compile an unconditional branch (goto).
   * @param label target of the branch (must be in this method).
   */
  public final void emitGoto (Label label)
  {
    emitGoto(label, 167);
    unreachable_here = true;
  }

  //public final void compile_goto_ifeq (Label label, boolean invert)
  public final void emitGotoIfEq (Label label, boolean invert)
  {
    Type type2 = popType().promote();
    Type type1 = popType().promote();
    reserve(4);
    int opcode;
    char sig1 = type1.getSignature().charAt(0);
    char sig2 = type2.getSignature().charAt(0);
    if (sig1 == 'I' && sig2 == 'I')
      opcode = 159;  // if_cmpeq (inverted: if_icmpne)
    else if (sig1 == 'J' && sig2 == 'J')
      {
	put1(148);   // lcmp
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (sig1 == 'F' && sig2 == 'F')
      {
	put1(149);   // fcmpl
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (sig1 == 'D' && sig2 == 'D')
      {
	put1(149);   // fcmpl
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if ((sig1 == 'L' || sig1 == '[')
	     && (sig2 == 'L' || sig2 == '['))
      opcode = 165;  // if_acmpeq (inverted: if_acmpne)
    else
      throw new Error ("non-matching types to compile_goto_ifeq");
    if (invert)
      opcode++;
    emitTransfer (label, opcode);
  }

  /** Compile a conditional transfer if 2 top stack elements are equal. */
  public final void emitGotoIfEq (Label label)
  {
    emitGotoIfEq(label, false);
  }

  /** Compile conditional transfer if 2 top stack elements are not equal. */
  public final void emitGotoIfNE (Label label)
  {
    emitGotoIfEq(label, true);
  }

  public final void emitGotoIfCompare1 (Label label, int opcode)
  {
    popType();
    reserve(3);
    emitTransfer (label, opcode);
  }

  public final void emitGotoIfIntEqZero(Label label)
  { emitGotoIfCompare1(label, 153); }
  public final void emitGotoIfIntNeZero(Label label)
  { emitGotoIfCompare1(label, 154); }
  public final void emitGotoIfIntLtZero(Label label)
  { emitGotoIfCompare1(label, 155); }
  public final void emitGotoIfIntGeZero(Label label)
  { emitGotoIfCompare1(label, 156); }
  public final void emitGotoIfIntGtZero(Label label)
  { emitGotoIfCompare1(label, 157); }
  public final void emitGotoIfIntLeZero(Label label)
  { emitGotoIfCompare1(label, 158); }

  /** Compile start of a conditional:  if (!(x OPCODE 0)) ...
   * The value of x must already have been pushed. */
  public final void emitIfCompare1 (int opcode)
  {
    IfState new_if = new IfState(this);
    popType();
    reserve(3);
    emitTransfer (new_if.end_label, opcode);
    new_if.start_stack_size = SP;
  }

  /** Compile start of conditional:  if (x != 0) */
  public final void emitIfIntNotZero()
  {
    emitIfCompare1(153); // ifeq
  }

  /** Compile start of a conditional:  if (!(x OPCODE y)) ...
   * The value of x and y must already have been pushed. */
  public final void emitIfIntCompare(int opcode)
  {
    IfState new_if = new IfState(this);
    popType();
    popType();
    reserve(3);
    emitTransfer(new_if.end_label, opcode);
    new_if.start_stack_size = SP;
  }

  /* Compile start of a conditional:  if (x < y) ... */
  public final void emitIfIntLt()
  {
    emitIfIntCompare(162);  // if_icmpge
  }

  /** Compile start of a conditional:  if (x != y) ...
   * The values of x and y must already have been pushed. */
  public final void emitIfNEq ()
  {
    IfState new_if = new IfState (this);
    emitGotoIfEq(new_if.end_label);
    new_if.start_stack_size = SP;
  }

  /** Compile start of a conditional:  if (x == y) ...
   * The values of x and y must already have been pushed. */
  public final void emitIfEq ()
  {
    IfState new_if = new IfState (this);
    emitGotoIfNE(new_if.end_label);
    new_if.start_stack_size = SP;
  }

  /** Emit a 'ret' instruction.
    * @param var the variable containing the return address */
  public void emitRet (Variable var)
  {
    int offset = var.offset;
    if (offset < 256)
      {
	reserve(2);
	put1(169);  // ret
	put1(offset);
      }
    else
      {
	reserve(4);
	put1(196);  // wide
	put1(169);  // ret
	put2(offset);
      }
  }

  public final void emitIfThen ()
  {
    new IfState(this);
  }

  /** Compile start of else clause. */
  public final void emitElse ()
  {
    Label else_label = if_stack.end_label;
    Label end_label = new Label (this);
    if_stack.end_label = end_label;
    if (reachableHere ())
      {
	int stack_growth = SP-if_stack.start_stack_size;
	if_stack.then_stacked_types = new Type[stack_growth];
	System.arraycopy (stack_types, if_stack.start_stack_size,
			  if_stack.then_stacked_types, 0, stack_growth);
	emitGoto (end_label);
      }
    while (SP != if_stack.start_stack_size)
      popType();
    else_label.define (this);
    if_stack.doing_else = true;    
  }

  /** Compile end of conditional. */
  public final void emitFi ()
  {
    boolean make_unreachable = false;
    if (! if_stack.doing_else)
      { // There was no 'else' clause.
	if (reachableHere ()
	    && SP != if_stack.start_stack_size)
	  throw new Error ("then clause grows stack with no else clause");
      }
    else if (if_stack.then_stacked_types != null)
      {
	int then_clause_stack_size
	  = if_stack.start_stack_size + if_stack.then_stacked_types.length;
	if (! reachableHere ())
	  {
	    System.arraycopy (if_stack.then_stacked_types, 0,
			      stack_types, if_stack.start_stack_size,
			      if_stack.then_stacked_types.length);
	    SP = then_clause_stack_size;
	  }
	else if (SP != then_clause_stack_size)
	  throw new Error ("SP at end of 'then' was " +
			   then_clause_stack_size
			   + " while SP at end of 'else' was " + SP);
      }
    else if (unreachable_here)
      make_unreachable = true;

    if_stack.end_label.define (this);
    if (make_unreachable)
      unreachable_here = true;
    // Pop the if_stack.
    if_stack = if_stack.previous;
  }

  /**
   * Convert the element on top of the stack to requested type
   */
  public final void emitConvert (Type type) {
    Type from = popType();
    pushType(from);
    emitConvert(from, type);
  }

  public final void emitConvert (Type from, Type to)
  {
    String to_sig = to.getSignature();
    String from_sig = from.getSignature();
    int op = -1;
    if (to_sig.length() == 1 || from_sig.length() == 1)
      {
	char to_sig0 = to_sig.charAt(0);
	char from_sig0 = from_sig.charAt(0);
	if (from.size < 4)
	  from_sig0 = 'I';
	if (to.size < 4)
	  {
	    emitConvert(from, Type.int_type);
	    from_sig0 = 'I';
	  }
	if (from_sig0 == to_sig0)
	  return;
	switch (from_sig0)
	  {
	  case 'I':
	    switch (to_sig0)
	      {
	        case 'B':  op = 145;  break;  // i2b
	        case 'C':  op = 146;  break;  // i2c
	        case 'S':  op = 147;  break;  // i2s
		case 'J':  op = 133;  break;  // i2l
		case 'F':  op = 134;  break;  // i2f
		case 'D':  op = 135;  break;  // i2d
	      }
	    break;
	  case 'J':
	    switch (to_sig0)
	      {
		case 'I':  op = 136;  break;  // l2i
		case 'F':  op = 137;  break;  // l2f
		case 'D':  op = 138;  break;  // l2d
	      }
	    break;
	  case 'F':
	    switch (to_sig0)
	      {
		case 'I':  op = 139;  break;  // f2i
		case 'J':  op = 140;  break;  // f2l
		case 'D':  op = 141;  break;  // f2d
	      }
	    break;
	  case 'D':
	    switch (to_sig0)
	      {
		case 'I':  op = 142;  break;  // d2i
		case 'J':  op = 143;  break;  // d2l
		case 'F':  op = 144;  break;  // d2f
	      }
	    break;
	  }
      }
    if (op < 0)
      throw new Error ("unsupported Method.compile_convert");
    reserve(1);
    popType();
    put1(op);
    pushType(to);
  }

  private void emitCheckcast (Type type, int opcode)
  {
    reserve(3);
    popType();
    put1(opcode);
    if (type instanceof ArrayType)
      {
	ArrayType atype = (ArrayType) type;
	CpoolUtf8 name = getConstants().addUtf8(atype.signature);
	putIndex2(getConstants().addClass(name));
      }
    else if (type instanceof ClassType)
      {
	putIndex2(getConstants().addClass((ClassType) type));
      }
    else
      throw new Error ("unimplemented type in emitCheckcast/emitInstanceof");
  } 

  public void emitCheckcast (Type type)
  {
    emitCheckcast(type, 192);
    pushType(type);
  }

  public void emitInstanceof (Type type)
  {
    emitCheckcast(type, 193);
    pushType(Type.boolean_type);
  }

  public final void emitThrow ()
  {
    popType();
    reserve(1);
    put1 (191);  // athrow
    unreachable_here = true;
  }

  /**
   * Compile a method return.
   */
  public final void emitReturn ()
  {
    if (getMethod().getReturnType() == Type.void_type)
      {
	reserve(1);
	put1(177); // return
      }
    else
      emitTypedOp (172, popType().promote());
  }

  /** Add an exception handler. */
  public void addHandler (int start_pc, int end_pc,
			  int handler_pc, int catch_type)
  {
    int index = 4 * exception_table_length;
    if (exception_table == null)
      {
	exception_table = new short[20];
      }
    else if (exception_table.length <= index)
      {
	short[] new_table = new short[2 * exception_table.length];
	System.arraycopy(exception_table, 0, new_table, 0, index);
	exception_table = new_table;
      }
    exception_table[index++] = (short) start_pc;
    exception_table[index++] = (short) end_pc;
    exception_table[index++] = (short) handler_pc;
    exception_table[index++] = (short) catch_type;
    exception_table_length++;
  }

  /** Add an exception handler. */
  public void addHandler (int start_pc, int end_pc, int handler_pc,
			  ClassType catch_type, ConstantPool constants)
  {
    int catch_type_index;
    if (catch_type == null)
      catch_type_index = 0;
    else
      catch_type_index = constants.addClass(catch_type).index;
    addHandler(start_pc, end_pc, handler_pc, catch_type_index);
  }


  public void emitTryStart(boolean has_finally, Type result_type)
  {
    TryState try_state = new TryState(this);
    boolean must_save_result = has_finally && result_type != null;
    if (must_save_result || SP > 0)
      {
	pushScope();
	if (must_save_result)
	  try_state.saved_result = addLocal(result_type);
      }
    if (SP > 0)
      {
	try_state.savedStack = new Variable[SP];
	int i = 0;
	while (SP > 0)
	  {
	    Variable var = addLocal(topType());
	    emitStore(var);
	    try_state.savedStack[i++] = var;
	  }
      }
    if (has_finally)
      try_state.finally_subr = new Label(this);
  }

  public void emitTryEnd()
  {
    if (try_stack.end_label == null)
      {
	if (try_stack.saved_result != null)
	  emitStore(try_stack.saved_result);
	try_stack.end_label = new Label(this);
	if (try_stack.finally_subr != null)
	  emitGoto(try_stack.finally_subr, 168);  // jsr
	if (reachableHere())
	  emitGoto(try_stack.end_label);
	readPC = PC;
	try_stack.end_pc = PC;
      }
  }

  public void emitCatchStart(Variable var)
  {
    emitTryEnd();
    if (try_stack.try_type != null)
      {
	emitCatchEnd();
      }
    ClassType type = var == null ? null : (ClassType) var.getType();
    try_stack.try_type = type;
    readPC = PC;
    addHandler(try_stack.start_pc, try_stack.end_pc,
	       PC, type, getConstants());
    if (var != null)
      {
	pushType(type);
	emitStore(var);
      }
  }

  public void emitCatchEnd()
  {
    if (reachableHere())
      {
	if (try_stack.saved_result != null)
	  emitStore(try_stack.saved_result);
	if (try_stack.finally_subr != null)
	  emitGoto(try_stack.finally_subr, 168); // jsr
	emitGoto(try_stack.end_label);
      }
    popScope();
    try_stack.try_type = null;
  }

  public void emitFinallyStart()
  {
    emitTryEnd();
    if (try_stack.try_type != null)
      {
	emitCatchEnd();
      }
    readPC = PC;
    try_stack.end_pc = PC;

    pushScope();
    Type except_type = Type.pointer_type;
    Variable except = addLocal(except_type);
    emitCatchStart(null);
    pushType(except_type);
    emitStore(except);
    emitGoto(try_stack.finally_subr, 168); // jsr
    emitLoad(except);
    emitThrow();
    
    //emitCatchEnd();
    
    //if (try_stack.finally_subr == null)
    // error();
    try_stack.finally_subr.define(this);
    Type ret_addr_type = Type.pointer_type;
    try_stack.finally_ret_addr = addLocal(ret_addr_type);
    pushType(ret_addr_type);
    emitStore(try_stack.finally_ret_addr);
  }

  public void emitFinallyEnd()
  {
    emitRet(try_stack.finally_ret_addr);
    unreachable_here = true;
    popScope();
    try_stack.finally_subr = null;
  }

  public void emitTryCatchEnd()
  {
    if (try_stack.finally_subr != null)
      emitFinallyEnd();
    try_stack.end_label.define(this);
    Variable[] vars = try_stack.savedStack;
    if (vars != null)
      {
	for (int i = vars.length;  --i >= 0; )
	  {
	    Variable v = vars[i];
	    if (v != null) {
	      emitLoad(v);
	    }
	  }
      }
    if (try_stack.saved_result != null)
	emitLoad(try_stack.saved_result);
    if (try_stack.saved_result != null || vars != null)
	popScope();
    try_stack = try_stack.previous;
  }

  /* Make sure the label with oldest fixup is first in labels. */
  void reorder_fixups ()
  {
    Label prev = null;
    Label cur;
    Label oldest = null;
    Label oldest_prev = null;
    int oldest_fixup = PC+100;
    for (cur = labels;  cur != null;  cur = cur.next)
      {
	if (cur.fixups != null && cur.fixups[0] < oldest_fixup)
	  {
	    oldest = cur;
	    oldest_prev = prev;
	    oldest_fixup = cur.fixups[0];
	  }
	prev = cur;
      }
    if (oldest != labels)
      {
	oldest_prev.next = oldest.next;
	oldest.next = labels;
	labels = oldest;
      }
  }

  public void finalize_labels ()
  {
    while (labels != null && labels.fixups != null)
      labels.emit_spring (this);
    for (Label label = labels;  label != null;  label = label.next)
      {
	if (!label.defined ())
	  throw new Error ("undefined label");
	if (label.fixups != null)
	  throw new Error ("internal error: finalize_labels");
    }
  }

  public void assignConstants (ClassType cl)
  {
    super.assignConstants(cl);
    Attribute.assignConstants(this, cl);
  }

  public final int getLength()
  {
    return 12 + getCodeLength() + 8 * exception_table_length
      + Attribute.getLengthAll(this);
  }

  public void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeShort (max_stack);
    dstr.writeShort (max_locals);
    dstr.writeInt (PC);
    dstr.write (code, 0, PC);

    dstr.writeShort (exception_table_length);
    int count = exception_table_length;
    for (int i = 0;  --count >= 0;  i += 4)
      {
	dstr.writeShort(exception_table[i]);
	dstr.writeShort(exception_table[i+1]);
	dstr.writeShort(exception_table[i+2]);
	dstr.writeShort(exception_table[i+3]);
      }

    Attribute.writeAll(this, dstr);
  }

  public void print (ClassTypeWriter dst) 
  {
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.print(getLength());
    dst.print(", max_stack:");
    dst.print(max_stack);
    dst.print(", max_locals:");
    dst.print(max_locals);
    dst.print(", code_length:");
    int length = getCodeLength();
    dst.println(length);
    disAssemble(dst, 0, length);
    if (exception_table_length > 0)
      {
	dst.print("Exceptions (count: ");
	dst.print(exception_table_length);
	dst.println("):");
	int count = exception_table_length;
	for (int i = 0;  --count >= 0;  i += 4)
	  {
	    dst.print("  start: ");
	    dst.print(exception_table[i] & 0xffff);
	    dst.print(", end: ");
	    dst.print(exception_table[i+1] & 0xffff);
	    dst.print(", handler: ");
	    dst.print(exception_table[i+2] & 0xffff);
	    dst.print(", type: ");
	    int catch_type_index = exception_table[i+3] & 0xffff;
	    if (catch_type_index == 0)
	      dst.print("0 /* finally */");
	    else
	      {
		dst.printOptionalIndex(catch_type_index);
		dst.printConstantTersely(catch_type_index, ConstantPool.CLASS);
	      }
	    dst.println();
	  }
      }
    dst.printAttributes(this);
  }

  public void disAssemble (ClassTypeWriter dst, int offset, int length)
  {
    boolean wide = false;
    for (int i = offset;  i < length; )
      {
	int oldpc = i++;
	int op = code[oldpc] & 0xff;
	String str = Integer.toString(oldpc);
	int printConstant = 0;
	int j = str.length();
	while (++j <= 3) dst.print(' ');
	dst.print(str);
	dst.print(": ");
	if (op < 120)
	  {
	    if (op < 87)
	      {
		if (op < 3)  print("nop;aconst_null;iconst_m1;", op, dst);
		else if (op < 9) { dst.print("iconst_");  dst.print(op-3); }
		else if (op < 16) // op >= 9 [lconst_0] && op <= 15 [dconst_1]
		  {
		    char typ;
		    if (op < 11) { typ = 'l';  op -= 9; }
		    else if (op < 14) { typ = 'f';  op -= 11; }
		    else { typ = 'd';  op -= 14; }
		    dst.print(typ);  dst.print("const_");  dst.print(op);
		  }
		else if (op < 21)
		  {
		    if (op < 18)  // op >= 16 [bipush] && op <= 17 [sipush]
		      {
			print("bipush ;sipush ;", op-16, dst);
			int constant;
			if (op == 16)  constant = code[i++];
			else { constant = (short) readUnsignedShort(i);  i+=2;}
			dst.print(constant);
		      }
		    else // op >= 18 [ldc] && op <= 20 [ldc2_w]
		      {
			printConstant = op == 18 ? 1 : 2;
			print("ldc;ldc_w;ldc2_w;", op-18, dst);
		      }
		  }
		else // op >= 21 && op < 87
		  {
		    String load_or_store;
		    if (op < 54) { load_or_store = "load"; }
		    else { load_or_store = "store"; op -=(54-21); }
		    int index;  // -2 if array op;  -1 if index follows
		    if (op < 26) { index = -1; op -= 21; }
		    else if (op < 46) { op -= 26;  index = op % 4;  op >>= 2; }
		    else { index = -2; op -= 46; }
		    dst.print("ilfdabcs".charAt(op));
		    if (index == -2) dst.write('a');
		    dst.print(load_or_store);
		    if (index >= 0) { dst.write('_');  dst.print(index); }
		    else if (index == -1)
		      {
			if (wide) { index = readUnsignedShort(i); index += 2; }
			else { index = code[i] & 0xff; i++; }
			wide = false;
			dst.print(' ');
			dst.print(index);
		      }
		  }
	      }
	    else // op >= 87 && op < 120
	      {
		if (op < 96)
		  print("pop;pop2;dup;dup_x1;dup_x2;dup2;dup2_x1;dup2_x2;swap;"
			, op-87, dst);
		else // op >= 96 [iadd] && op <= 119 [dneg]
		  {
		    dst.print("ilfda".charAt((op-96) % 4));
		    print("add;sub;mul;div;rem;neg;", (op-96)>>2, dst);
		  }
	      }
	  }
	else // op >= 120
	  {
	    if (op < 170)
	      {
		if (op < 132) // op >= 120 [ishl] && op <= 131 [lxor]
		  {
		    dst.print((op & 1) == 0 ? 'i' : 'l');
		    print("shl;shr;ushr;and;or;xor;", (op-120)>>1, dst);
		  }
		else if (op == 132) // iinc
		  {
		    int var_index;
		    int constant;
		    dst.print("iinc");
		    if (! wide)
		      {
			var_index = 0xff & code[i++];
			constant = code[i++];
		      }
		    else
		      {
			var_index = readUnsignedShort(i);
			i += 2;
			constant = (short) readUnsignedShort(i);
			i += 2;
			wide = false;
		      }
		    dst.print(' ');  dst.print(var_index);
		    dst.print(' ');  dst.print(constant);
		  }
		else if (op < 148) // op >= 133 [i2l] && op <= 147 [i2s]
		  {
		    dst.print("ilfdi".charAt((op-133) / 3));
		    dst.print('2');
		    dst.print("lfdifdildilfbcs".charAt(op-133));
		  }
		else if (op < 153) // op >= 148 [lcmp] && op <= 152 [dcmpg]
		  print("lcmp;fcmpl;fcmpg;dcmpl;dcmpg;", op-148, dst);
		else
		  {
		    if (op < 159)
		      {
			dst.print("if");
			print("eq;ne;lt;ge;gt;le;", op-153, dst);
		      }
		    else if (op < 167)
		      {
			if (op < 165) { dst.print("if_icmp"); }
			else { dst.print("if_acmp"); op -= 165-159; }
			print("eq;ne;lt;ge;gt;le;", op-159, dst);
		      }
		    else
		      print("goto;jsr;ret;", op-167, dst);
		    int delta = (short) readUnsignedShort(i);
		    i += 2;
		    dst.print(' ');  dst.print(oldpc+delta);
		  }
	      }
	    else
	      {
		if (op < 172) //  [tableswitch] or [lookupswitch]
		  {
		    i = (i + 3) & ~3; // skip 0-3 byte padding.
		    int code_offset = readInt(i);  i += 4;
		    if (op == 170)
		      {
			dst.print("tableswitch");
			int low = readInt(i);  i += 4;
			int high = readInt(i+4);  i += 4;
			dst.print(" low: "); dst.print(low);
			dst.print(" high: "); dst.print(high);
			dst.print(" default: "); dst.print(code_offset);
			for (;  low <= high;  low++)
			  {
			    code_offset = readInt(i);  i += 4;
			    dst.println();
			    dst.print("  ");  dst.print(low);
			    dst.print(": ");  dst.print(oldpc + code_offset); 
			  }
		      }
		    else
		      {
			dst.print("lookupswitch");
			int npairs = readInt(i);  i += 4;
			dst.print(" npairs: "); dst.print(npairs);
			dst.print(" default: "); dst.print(code_offset);
			while (--npairs >= 0)
			  {
			    int match = readInt(i);  i += 4;
			    code_offset = readInt(i);  i += 4;
			    dst.println();
			    dst.print("  ");  dst.print(match);
			    dst.print(": ");  dst.print(oldpc + code_offset); 
			  }
		      }
		  }
		else if (op < 178) // op >= 172 [ireturn] && op <= 177 [return]
		  {
		    if (op < 177) dst.print("ilfda".charAt(op-172));
		    dst.print("return");
		  }
		else if (op < 182) // op >= 178 [getstatic] && op <= 181 [putfield]
		  {
		    print("getstatic;putstatic;getfield;putfield;", op-178, dst);
		    printConstant = 2;
		  }
		else if (op < 186) // op >= 182 && op <= 185 [invoke*]
		  {
		    dst.print("invoke");
		    print("virtual;special;static;interface;", op-182, dst);
		    printConstant = 2;
		    if (op == 185) // invokeinterface
		      i += 2;
		  }
		else if (op < 196)
		  {
		    print("186;new;newarray;anewarray;arraylength;athrow;checkcast;instanceof;moniorenter;monitorexit;", op-186, dst);
		    if (op == 187 || op == 189 || op == 192 || op == 193)
		      printConstant = 2;
		    else if (op == 188)  // newarray
		      {
			int type = code[i++];
			dst.print(' ');
			if (type >= 4 && type <= 11)
			  print("boolean;char;float;double;byte;short;int;long;",
				type-4, dst);
			else
			  dst.print(type);
		      }
			
		  }
		else if (op == 196) // wide
		  {
		    dst.print("wide");
		    wide = true;
		  }
		else if (op == 197)
		  {
		    dst.print("multianewarray");
		    int index = readUnsignedShort(i);
		    i += 2;
		    dst.printConstantOperand(index);
		    int dims = 0xff & code[i++];
		    dst.print(' ');
		    dst.print(dims);
		  }
		else if (op < 200)
		  {
		    print("ifnull;ifnonnull;", op-198, dst);
		    int delta = (short) readUnsignedShort(i);
		    i += 2;
		    dst.print(' ');  dst.print(oldpc+delta);
		  }
		else if (op < 202)
		  {
		    print("goto_w;jsr_w;", op-200, dst);
		    int delta = readInt(i);
		    i += 4;
		    dst.print(' ');  dst.print(oldpc+delta);
		  }
		else
		  dst.print(op);
	      }
	  }
	if (printConstant > 0)
	  {
	    int index;
	    if (printConstant == 1) index = 0xff & code[i++];
	    else { index = readUnsignedShort(i);  i += 2; }
	    dst.printConstantOperand(index);
	  }
	dst.println();
      }
  }

  private int readUnsignedShort(int offset)
  {
    return ((0xff & code[offset]) << 8) | (0xff & code[offset+1]);
  }

  private int readInt(int offset)
  {
    return (readUnsignedShort(offset) << 16) | readUnsignedShort(offset+2);
  }

  /* Print the i'th ';'-delimited substring of str on dst. */
  private void print (String str, int i, PrintWriter dst)
  {
    int last = 0;
    int pos = -1;
    for (; i >= 0; i--)
      {
	last = ++pos;
	pos = str.indexOf(';', last);
      }
    dst.write(str, last, pos-last);
  }
}
