// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** Access flags. */
/* When using JDK 1.1, replace this class by java.lang.reflec.Modifiers. */

public class Access {
  static public final short PUBLIC      = 0x0001;
  static public final short PRIVATE     = 0x0002;
  static public final short PROTECTED   = 0x0004;
  static public final short STATIC      = 0x0008;
  static public final short FINAL       = 0x0010;
  static public final short SYNCHRONIZED= 0x0020;
  static public final short VOLATILE    = 0x0040;
  static public final short TRANSIENT   = 0x0080;
  static public final short NATIVE      = 0x0100;
  static public final short INTERFACE   = 0x0200;
  static public final short ABSTRACT    = 0x0400;

  public static String toString(int flags)
  {
    StringBuffer buf = new StringBuffer();
    if ((flags & PUBLIC) != 0)      buf.append(" public");
    if ((flags & PRIVATE) != 0)     buf.append(" private");
    if ((flags & PROTECTED) != 0)   buf.append(" protected");
    if ((flags & STATIC) != 0)      buf.append(" static");
    if ((flags & FINAL) != 0)       buf.append(" final");
    if ((flags & SYNCHRONIZED) != 0)buf.append(" synchronized");
    if ((flags & VOLATILE) != 0)    buf.append(" volatile");
    if ((flags & TRANSIENT) != 0)   buf.append(" transient");
    if ((flags & NATIVE) != 0)      buf.append(" native");
    if ((flags & INTERFACE) != 0)   buf.append(" interface");
    if ((flags & ABSTRACT) != 0)    buf.append(" abstract");
    return buf.toString();
  }
}
