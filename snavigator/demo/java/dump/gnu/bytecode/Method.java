// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/**
  * Represents a method in a <code>ClassType</code>.
  * <p>
  * A <code>Method</code> contain a <code>CodeAttr</code> object;
  * the interface for generating bytecode instructions is primarily
  * in <code>CodeAttr</code>.
  * <p>
  * All the methods whose name start with <code>compile_</code> are
  * deprecated, and should not be used;  use the methods
  * in <code>CodeAttr</code>instead.
  */

public class Method implements AttrContainer {
  private String name;
  Type[] arg_types;
  
  Type return_type;
  int access_flags;
  int name_index; /* Index in constant table, or 0 if un-assigned */
  int signature_index; /* Index in constant table, or 0 if un-assigned */
  Method next;
  ClassType classfile;

  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
  { this.attributes = attributes; }
  CodeAttr code;
  public final CodeAttr getCode () { return code; }

  Method (ClassType clfile, int flags) {
     if (clfile.last_method == null)
       clfile.methods = this;
    else
      clfile.last_method.next = this;
    clfile.last_method = this;
    clfile.methods_count++;
    access_flags = flags;
    classfile = clfile;
  }

  public final void setStaticFlag (boolean is_static) {
    if (is_static)
      access_flags |= Access.STATIC;
    else
      access_flags ^= ~Access.STATIC;
  }

  public final boolean getStaticFlag () {
    return (access_flags & Access.STATIC) != 0;
  }

  public final ConstantPool getConstants ()
  {
    return classfile.constants;
  }
    
  public Scope pushScope () {
    prepareCode(0);
    return code.pushScope();
  }

  /** True if control could reach here. */
  public boolean reachableHere () { return !code.unreachable_here; }

  public Scope popScope () { return code.popScope(); }

  /** Assign a local variable to a given slot.
   * @param local the Variable to assign
   * @param slot the local variable slot desired
   * @deprecated
   * @return true iff we succeeded (i.e. the slot was unused) */
  public boolean assign_local (Variable local, int slot)
  {
    return local.reserveLocal(slot, code);
  }

  /**
   * Allocate slots for a local variable (or parameter).
   * @param local the variable we need to allocate
   * @return the index of the (first) slot.
   * @deprecated
   */
  public void allocate_local (Variable local)
  {
    local.allocateLocal(code);
  }

  /** Allocate a Code attribute, and prepare to generate code. */

  public void initCode ()
  {
    if (classfile.constants == null)
      classfile.constants = new ConstantPool();
    prepareCode(0);
    code.pushScope();
  }

  public void init_param_slots ()
  {
    initCode ();
    if ((access_flags & Access.STATIC) == 0)
      code.addLocal(classfile).setParameter(true);
    int arg_count = arg_types.length;
    for (int i = 0;  i < arg_count;  i++) {
      code.addLocal(arg_types[i]).setParameter (true);
    }
  }

  void kill_local (Variable var) { var.freeLocal(code); }

  /** Method that must be called before we generate any instructions.
    * Set so there is room for at least max_size bytes of code.
    */
  void prepareCode(int max_size)
  {
    if (code == null)
      code = new CodeAttr(this);
    code.reserve(max_size);
  }

  // This method should be called before we generate code for
  // an instruction (or sequence).
  // An upper bound of the intruction length is max_size.
  // deprecated!
  void instruction_start_hook (int max_size)
  {
    prepareCode(max_size);
  }

  final Type pop_stack_type () { return code.popType(); }
  final void push_stack_type (Type type) { code.pushType(type); }

  public void compile_checkcast (Type type)
  {
    code.emitCheckcast (type);
  }

  public void maybe_compile_checkcast (Type type)
  {
    Type stack_type = code.topType();
    if (type != stack_type)  // FIXME rather simple-minded, but safe.
      code.emitCheckcast(type);
  }

  /**
   * Comple code to push the contents of a local variable onto the statck.
   * @param var The variable whose contents we want to push.
   * @deprecated
   */
  public void push_var (Variable var) { code.emitLoad (var); }
  /**
   * @deprecated
   */
  public void compile_push_value (Variable var) { code.emitLoad(var); }

  /** 
    * @deprecated
   */
  public void compile_store_value (Variable var)
  {
    code.emitStore(var);
  }

  public void compile_push_this ()
  {
    code.emitPushThis();
  }

  /*
  public Scope scope_start ();
  public scope_end ();
  public Variable new_temp (String name);
  public Variable free_temp (Variable temp_var);

  public void push_long_const (long i);

  */

  /** Compile a tail-call to position 0 of the current procewure.
   * If pop_args is true, copy argument registers (except this) from stack. */
  public void compile_tailcall (boolean pop_args)
  {
    if (pop_args)
      {
	int arg_slots = ((access_flags & Access.STATIC) != 0) ? 0 : 1;
	for (int i = arg_types.length;  --i >= 0; )
	  arg_slots += arg_types[i].size > 4 ? 2 : 1;
	for (int i = arg_types.length;  --i >= 0; )
	  {
	    arg_slots -= arg_types[i].size > 4 ? 2 : 1;
	    code.emitStore(code.locals.used [arg_slots]);
	  }
      }
    prepareCode(5);
    int delta = - code.PC;
    if (delta < -32768)
      {
	code.put1(200);  // goto_w
	code.put4(delta);
      }
    else
      {
	code.put1(167); // goto
	code.put2(delta);
      }
    code.unreachable_here = true;
  }

  public void compile_linenumber (int linenumber)
  {
    if (code == null)
      code = new CodeAttr(this);
    code.putLineNumber(linenumber);
  }

  void write (DataOutputStream dstr, ClassType classfile)
       throws java.io.IOException
  {
    Variable var;
    dstr.writeShort (access_flags);
    dstr.writeShort (name_index);
    dstr.writeShort (signature_index);

    Attribute.writeAll(this, dstr);
  }

  private String signature;

  public String getSignature ()
  {
    if (signature == null)
      {
	StringBuffer buf = new StringBuffer(100);
	int args_count = arg_types.length; 
	buf.append('(');
	for (int i = 0; i < args_count; i++)
	  buf.append (arg_types[i].getSignature());
	buf.append(')');
	buf.append(return_type.getSignature());
	signature = buf.toString();
      }
    return signature;
  }

  public void setSignature (String signature)
  {
    int len = signature.length();
    if (len < 3 || signature.charAt(0) != '(')
      throw new ClassFormatError("bad method signature");
    int pos = 1;
    java.util.Stack types = new java.util.Stack();
    for (;;)
      {
	int arg_sig_len = Type.signatureLength(signature, pos);
	if (arg_sig_len < 0)
	  {
	    if (pos < len && signature.charAt(pos) == ')')
	      break;
	    throw new ClassFormatError("bad method signature");
	  }
	Type arg_type = Type.signatureToType(signature, pos, arg_sig_len);
	types.push(arg_type);
	pos += arg_sig_len;
      }
    arg_types = new Type[types.size()];
    for (int i = types.size();  --i >= 0; )
      arg_types[i] = (Type) types.pop();
    return_type = Type.signatureToType(signature, pos+1, len-pos-1);
  }

  public void setSignature (int signature_index)
  {
    CpoolUtf8 sigConstant = (CpoolUtf8)
      getConstants().getForced(signature_index, ConstantPool.UTF8);
    this.signature_index = signature_index;
    setSignature(sigConstant.string);
  }

  void assign_constants ()
  {
    ConstantPool constants = getConstants();
    if (name_index == 0 && name != null)
      name_index = constants.addUtf8(name).index;
    if (signature_index == 0)
      signature_index = constants.addUtf8(getSignature()).index;
    Attribute.assignConstants(this, classfile);
  }

  public ClassType getDeclaringClass() { return classfile; }

  public final Type getReturnType() { return return_type; }

  public final Type[] getParameterTypes() { return arg_types; }

  public final String getName ()
  {
    return name;
  }
  public final void setName(String name)
  {
    this.name = name;
  }

  public final void setName(int name_index)
  {
    if (name_index <= 0)
      name = null;
    else
      {
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  getConstants().getForced(name_index, ConstantPool.UTF8);
	name = nameConstant.string;
      }
    this.name_index = name_index;
  }
};
