// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.Hashtable;
import java.io.*;

/** Information about one member of a ZipArchive. */

public class ZipMember
{
  ZipMember next; 
  int compressed_size;
  int uncompressed_size;
  //  short filename_length;
  int relative_offset_local_header;  // start of local directory

  byte[] name;
  private String str_name;

  /* Where the actual data bytes start, withing the achive. */
  int fileStart ()
  {
    return relative_offset_local_header + ZipArchive.LREC_SIZE + 4 + name.length;
  }

  public void print (PrintStream ps)
  {
    ps.write (name, 0, name.length);
    ps.println (" size: "+compressed_size+" position: "+fileStart ());
  }

  public String strName ()
  {
    if (str_name == null)
      str_name = new String (name, 0);
    return str_name;
  }

  boolean matches (String match_name)
  {
    if (name.length != match_name.length ())
      return false;
    return match_name.equals (strName ());
  }

  public byte[] getData (ZipArchive archive) throws IOException
  {
    archive.file.seek (fileStart());
    byte[] result = new byte[compressed_size];
    archive.file.readFully (result);
    return result;
  }

};
