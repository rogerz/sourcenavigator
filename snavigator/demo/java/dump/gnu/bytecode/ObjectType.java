package gnu.bytecode;

/**
  * Abstract class object reference types.
  * <p>
  * Extended by ClassType and ArrayType. */

public abstract class ObjectType extends Type
{
  public ObjectType ()
  {
    size = 4;
  }


  public abstract String getNameOrSignature();

  /** Get the java.lang.Class object for the representation type. */
  public Class getReflectClass()
  {
    try
      {
	if (reflectClass == null)
	  reflectClass = Class.forName(getNameOrSignature());
      }
    catch (java.lang.ClassNotFoundException ex)
      {
      }
    return reflectClass;
  }

  /** Convert an object to a value of this Type.
   * Throw a ClassCastException when this is not possible. */
  public Object coerceFromObject (Object obj)
  {
    if (this == Type.string_type)
      return obj.toString();
    if (getReflectClass().isAssignableFrom(obj.getClass()))
      return obj;
    throw new ClassCastException("don't know how to coerce "
				 + obj.getClass().getName() + " to "
				 + getName());
  }

  /** Compile (in given method) cast from Object to this Type. */
  public void emitCoerceFromObject (CodeAttr code)
  {
    if (this == Type.string_type)
      code.emitInvokeVirtual(Type.toString_method);
    else if (this != Type.pointer_type)
      code.emitCheckcast(this);
  }
}
