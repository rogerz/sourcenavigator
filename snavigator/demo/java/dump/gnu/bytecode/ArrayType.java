// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

public class ArrayType extends ObjectType
{
  public Type elements;

  public ArrayType (Type elements)
  {
    this_name = elements.getName() + "[]";
    setSignature("[" + elements.getSignature());
    this.elements = elements;
  }

  public Type getComponentType() { return elements; }

  public String getNameOrSignature() { return getSignature(); }

}
