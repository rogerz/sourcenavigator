package gnu.bytecode;
import java.io.*;

public class PrimType extends Type {

  public PrimType (String nam, String sig, int siz, Class reflectClass) {
    super(nam, sig);
    size = siz;
    this.reflectClass = reflectClass;
  }

  public Object coerceFromObject (Object obj)
  {
    if (obj.getClass() == reflectClass)
      return obj;
    char sig1 = (signature == null || signature.length() != 1) ? ' '
      : signature.charAt(0);
    switch (sig1)
      {
      case 'B':	return new Byte(((Number) obj).byteValue());
      case 'S':	return new Short(((Number) obj).shortValue());
      case 'I':	return new Integer(((Number) obj).intValue());
      case 'J':	return new Long(((Number) obj).longValue());
      case 'F':	return new Float(((Number) obj).floatValue());
      case 'D':	return new Double(((Number) obj).doubleValue());
      }
    throw new ClassCastException("don't know how to coerce "
				 + obj.getClass().getName() + " to "
				 + getName());
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    char sig1 = (signature == null || signature.length() != 1) ? ' '
      : signature.charAt(0);
    if (sig1 == 'Z')  // boolean
      {
	code.emitCheckcast(boolean_ctype);
	code.emitInvokeVirtual(booleanValue_method);
	return;
      }
    code.emitCheckcast(number_type);
    if (sig1 == 'I' || sig1 == 'S' || sig1 == 'B')
      code.emitInvokeVirtual(intValue_method);
    else if (sig1 == 'J')
      code.emitInvokeVirtual(longValue_method);
    else if (sig1 == 'D')
      code.emitInvokeVirtual(doubleValue_method);
    else if (sig1 == 'F')
      code.emitInvokeVirtual(floatValue_method);
    // Have left out Character -> char, since not used by Kawa.
    else
      super.emitCoerceFromObject(code);
  }
}
