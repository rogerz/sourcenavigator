// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.PrintWriter;
import java.io.PrintStream;

/** This class prints out in contents of a ClassType in human-readable form.
 * The output format matches my earlier jcf-dump program (in gcc/java).
 * This class is not complete - for example it needs a dis-assembler.
 * @author Per Bothner <bothner@cygnus>
 */

public class ClassTypeWriter extends PrintWriter
{
  ClassType ctype;
  int flags;
  boolean printConstants = true;

  public ClassTypeWriter (ClassType ctype, PrintWriter stream, int flags)
  {
    super(stream);
    this.ctype = ctype;
    this.flags = flags;
  }

  public ClassTypeWriter (ClassType ctype, PrintStream stream, int flags)
  {
    super(stream);
    this.ctype = ctype;
    this.flags = flags;
  }

  public static void print (ClassType ctype, PrintWriter stream, int flags)
  {
    ClassTypeWriter writer = new ClassTypeWriter (ctype, stream, flags);
    writer.print();
    writer.flush();
  }

  public static void print (ClassType ctype, PrintStream stream, int flags)
  {
    ClassTypeWriter writer = new ClassTypeWriter (ctype, stream, flags);
    writer.print();
    writer.flush();
  }

  public void print ()
  {
    if (printConstants)
      printConstantPool();
    printClassInfo();
    printFields();
    printMethods();
  }

  public void printAttributes (AttrContainer container)
  {
    for (Attribute attr = container.getAttributes();
	 attr != null;  attr = attr.next)
      {
	attr.print(this);
      }
  }

  void printClassInfo ()
  {
    println();
    print("Access flags: 0x");
    int modifiers = ctype.getModifiers();
    print(Integer.toHexString(modifiers));
    print('=');
    print(Access.toString(modifiers));
    println();
    print("This class: ");
    printOptionalIndex(ctype.thisClassIndex);
    printConstantTersely(ctype.thisClassIndex, 7);
    print(" super: ");
    if (ctype.superClassIndex == -1)
      print("<unknown>");
    else if (ctype.superClassIndex == 0)
      print("0");
    else
      {
	printOptionalIndex(ctype.superClassIndex);
	printConstantTersely(ctype.superClassIndex, ConstantPool.CLASS);
      }
    println();
    print("Interfaces (count: ");
    int[] interfaces = ctype.interfaceIndexes;
    int n_interfaces = interfaces == null ? 0 : interfaces.length;
    print(n_interfaces);
    print("):");
    println();
    for (int i = 0;  i < n_interfaces;  i++)
      {
	print("- Implements: ");
	int index = interfaces[i];
	printOptionalIndex(index);
	printConstantTersely(index, 7);
	println();
      }
  }

  public final void printFields ()
  {
    println();
    print("Fields (count: ");
    print(ctype.fields_count);
    print("):");
    println();
    int ifield = 0;
    Field field = ctype.fields;
    for (; field != null; ifield++, field = field.next)
      {
	print("Field name: ");
	if (field.name_index != 0)
	  printOptionalIndex(field.name_index);
	print(field.getName());
	print(Access.toString(field.flags));
	print(" Signature: ");
	if (field.signature_index != 0)
	  printOptionalIndex(field.signature_index);
	printSignature(field.type);
	println();
	printAttributes(field);
      }
  }

  void printMethods()
  {
    println();
    print("Methods (count: ");
    print(ctype.methods_count);
    print("):");
    println();
    Method method = ctype.methods;
    for (; method != null; method = method.next)
      {
	println();
	print("Method name:");
	if (method.name_index != 0)
	  printOptionalIndex(method.name_index);
	print('\"');
	print(method.getName());
	print('\"');
	print(Access.toString(method.access_flags));
	print(" Signature: ");
	if (method.signature_index != 0)
	  printOptionalIndex(method.signature_index);
	print('(');
	for (int i = 0;  i < method.arg_types.length;  i++)
	  {
	    if (i > 0)
	      print(',');
	    printSignature(method.arg_types[i]);
	  }
	print(')');
	printSignature(method.return_type);
	println();
	printAttributes(method);
      }
  }

  final void printConstantTersely(int index, int expected_tag)
  {
    CpoolEntry[] pool = ctype.constants.pool;
    CpoolEntry entry;
    if (pool == null || index < 0 || index >= pool.length
	|| (entry = pool[index]) == null)
      print("<invalid constant index>");
    else if (entry.getTag() != expected_tag)
      {
	print("<unexpected constant type ");
	entry.print(this, 1);
	print('>');
      }
    else
      entry.print(this, 0);
  }

  /** Print constant pool index for dis-assembler. */
  final void printConstantOperand(int index)
  {
    print(' ');
    if (printConstants)
      {
	print('#');
	print(index);
	print('=');
      }
    CpoolEntry[] pool = ctype.constants.pool;
    CpoolEntry entry;
    if (pool == null || index < 0 || index >= pool.length
	|| (entry = pool[index]) == null)
      print("<invalid constant index>");
    else
      {
	print('<');
	entry.print(this, 1);
	print('>');
      }
  }

  public final void printConstantPool ()
  {
    CpoolEntry[] pool = ctype.constants.pool;
    int length = ctype.constants.count;
    for (int i = 1;  i <= length; i++)
      {
	CpoolEntry entry = pool[i];
	if (entry == null)
	  continue;
	print('#');
	print(entry.index);
	print(": ");
	entry.print(this, 2);
	println();
      }
  }

  public final void printOptionalIndex(int index)
  {
    if (printConstants)
      {
	print(index);
	print('=');
      }
  }

  public final void printOptionalIndex(CpoolEntry entry)
  {
    printOptionalIndex(entry.index);
  }

  void printName(String name)
  {
    // in jcf-dump:  jcf_print_utf8
    print(name);
  }

  /** Print in Java source form one type from a signature string.
   * @param sig the signature string to print
   * @param pos the index in sig to start with
   * @return the index following the signature of one type. */
  public final int printSignature(String sig, int pos)
  {
    int len = sig.length();
    if (pos >= len)
      {
	print("<empty signature>");
	return pos;
      }
    int sig_length = Type.signatureLength(sig, pos);
    if (sig_length > 0)
      {
	String name = Type.signatureToName(sig.substring(pos,pos+sig_length));
	if (name != null)
	  {
	    print(name);
	    return pos+sig_length;
	  }
      }
    char c = sig.charAt(pos);
    if (c != '(')
      {
	print(c);
	return pos+1;
      }
    int nargs = 0;
    pos++;
    print(c);
    for (;;)
      {
	if (pos >= len)
	  {
	    print("<truncated method signature>");
	    return pos;
	  }
	c = sig.charAt(pos);
	if (c == ')')
	  {
	    pos++;
	    print(c);
	    break;
	  }
	if (nargs > 0)
	  print(',');
	pos = printSignature(sig, pos);
      }
    return printSignature(sig, pos);

    /*
    char c = sig.charAt(pos);
    Type type = Type.signatureToPrimitive(c);
    if (type != null)
      {
	print(Type.getName());
	return pos+1;
      }
    switch (c)
      {
      case 'L':
	for (;;)
	  {
	    pos++;
	    if (pos >= len)
	      {
		print("<truncated object signature>");
		return pos;
	      }
	    c = sig.charAt(pos);
	    if (c == ';')
	      return pos+1;
	    print(c == '/' ? '.' : c);
	  }

      case '[':
	pos = printSignature(sig, pos+1);
	print("[]");
	return pos;

      case '(':
	int nargs = 0;
	pos++;
	print(c);
	for (;;)
	  {
	    if (pos >= len)
	      {
		print("<truncated method signature>");
		return pos;
	      }
	    c = sig.charAt(pos);
	    if (c == ')')
	      {
		pos++;
		print(c);
		break;
	      }
	    if (nargs > 0)
	      print(',');
	    pos = printSignature(sig, pos);
	  }
	return printSignature(sig, pos);

      default:   print(c); return pos+1;
      }
      */
  }

  /** Print a signature string  in Java source.
   * @param sig the signature string to print */
  public final void printSignature(String sig)
  {
    int pos = printSignature(sig, 0);
    int len = sig.length();
    if (pos < len)
      {
	print("<trailing junk:");
	print(sig.substring(pos));
	print('>');
      }
  }

  public final void printSignature(Type type)
  {
    if (type == null)
      print("<unknown type>");
    else
      printSignature(type.getSignature());
  }

  // THE REMAINING CODE IS THE SKELTON OF AN UNFINISHED DIS-ASSEMBLER.

  /*
  public void printIntField (int value, int width)
  {
    String str = Integer.toString(value);
    for (int i = width - str.length();  --i >= 0; ) print(' ');
    print(str);
  }

  public void printConstantRef(int index)
  {
    print('#');
    print(index);
    print('=');
    CpoolEntry[] pool = ctype.constants.pool;
    CpoolEntry entry;
    if (pool == null || index < 0 || index >= pool.length
	|| (entry = pool[index]) == null)
      print("<invalid constant index>");
    else
      {
	print('<');
	entry.print(this, 1);
	print('>');
      }
  }
  */

  /* These private variable are "global shared" state used by
   * printDisassemble and its utility methods.
   * In general, that is a bad idea.  However, since it only makes
   * sense to disassemble one method at a time in a single ClassTypeWriter
   * (because ClassTypeWriter is an extension of Writer),
   * it seems harmless.  It might be cleaner to use a nested class,
   * but that seems hard to justify. */
  /*
  private int current_pc;
  private Method current_method;

  private int get_index(int length) { ...}

  private int get_immediate_1 () { ... }

  public final void printDisassemble (Method method)
  {
    current_method = method;
    for (current_pc = 0;  current_pc < PC;  current_pc++)
      {
	printIntField(pc, 3); print(": ");
	int op = code[pc]);
        print(Op.name(op));
	switch (op)
	  {
	  case 18: // lcd1
	  case 19: // lcd2
	    int index = get_index(op == 18 ? 1 : 2);
	    print(' ');
	    printConstantRef(index);
	    break;
	  case 21: // iload
	  case 22: // lload
	    int reg = get_immediate_1();  // !!!
	    print(' ');
	    print(reg);
	    break;
	  case 95:  // iadd
	  case 96:  // ladd
	    break;
	  }
	println();
      }
  }
  */

}
