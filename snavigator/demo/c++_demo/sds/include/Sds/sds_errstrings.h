/* $Header: */

#ifndef ISTKsds_errorstring_h
#define ISTKsds_errorstring_h

char *sds_error_string[36] = 
    {
    "all ok",
    "No such SDS",
    "No space for directory",
    "Cannot open file",
    "Cannot write to file",
    "No such object",
    "Cannot read file",
    "File is not SDS",
    "File is old SDS version",
    "No open file",
    "Byte swapped SDS",
    "SDS is not assembled",
    "SDS is not initialised",
    "Type not defined",
    "SDS is not definable",
    "SDS of this name exists",
    "Transfer type unknown",
    "Element type not as requested",
    "Wrong padding type",
    "Not enough memory",
    "No db - process assigned",
    "Database access error",
    "Not a complex object",
    "Resolution lists confused",
    "Zero length object",
    "Data format is for native processor",
    "Cannot resize this object",
    "End of resolution stack",
    "Cannot find map file",
    "No such list",
    "Impossible Sds error registered",
    "Program mark",
    "Not a canned message",
    "Namelist does not match definition",
    "Unused data",
    "Zero length read() requested"
    };

char *sds_error_levels[10] = 
    {
      "Marker",
      "Return",
      "??",
      "Warning",
      "??",
      "Error",
      "??",
      "??",
      "??",
      "Fatal"
      };

#endif

