// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class Field extends Location implements AttrContainer {
  int constant_value_index; /* If non-0, cpool index of constant value. */
  int flags;
  Field next;

  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
    { this.attributes = attributes; }

  /** The class that contains this field. */
  ClassType owner;

  /** Add a new Field to a ClassType. */
  public Field (ClassType ctype)
  {
    if (ctype.last_field == null)
      ctype.fields = this;
    else
      ctype.last_field.next = this;
    ctype.last_field = this;
    ctype.fields_count++;
    owner = ctype;
  }

  void write (DataOutputStream dstr, ClassType classfile)
       throws java.io.IOException
  {
    short attributes_count = constant_value_index > 0 ? (short)1 : (short)0;
    dstr.writeShort (flags);
    dstr.writeShort (name_index);
    dstr.writeShort (signature_index);
    dstr.writeShort (attributes_count);
    if (constant_value_index > 0) {
      dstr.writeShort (classfile.ConstantValue_name_index);
      dstr.writeInt (2);  // attribute_length
      dstr.writeShort (constant_value_index);
    }
  }
  
  void assign_constants (ClassType classfile)
  {
    ConstantPool constants = classfile.constants;
    if (name_index == 0 && name != null)
      name_index = constants.addUtf8(name).index;
    if (signature_index == 0 && type != null)
      signature_index = constants.addUtf8(type.signature).index;
    if (constant_value_index > 0 && classfile.ConstantValue_name_index == 0)
      classfile.ConstantValue_name_index
	= constants.addUtf8("ConstantValue").index;
  }


}
