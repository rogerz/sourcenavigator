// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** A CONSTANT_Class entry in the constant pool. */

public class CpoolClass extends CpoolEntry {
  CpoolUtf8 name;

  CpoolClass () { }

  CpoolClass (ConstantPool cpool, int hash, CpoolUtf8 n)
  {
    super (cpool, hash);
    name = n;
  }

  public int getTag() { return ConstantPool.CLASS; }

  final static int hashCode (CpoolUtf8 name) { return name.hash ^ 0xF0F; }

  void write (DataOutputStream dstr) throws java.io.IOException {
    dstr.writeByte (ConstantPool.CLASS);
    dstr.writeShort (name.index);
  }

  public void print (ClassTypeWriter dst, int verbosity)
  {
    if (verbosity == 1)
      dst.print("Class ");
    else if (verbosity > 1)
      {
	dst.print("Class name: ");
	dst.printOptionalIndex(name);
      }
    dst.print(name.string.replace('/', '.'));
  }
}
