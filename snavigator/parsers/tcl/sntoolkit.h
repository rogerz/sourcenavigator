

/* int sn_insert_symbol(int id_type, char *classname, 
  char *identifier, char *filename, int start_lineno, 
  int startCol, int endLine, int endCol, unsigned long attrib,
  char *returnType, char *argTypes, char *argNames, 
  char *comment, int highStartLine, int highStartCol, 
  int highEndLine, int highEndCol);

*/

#ifndef INCLUDE_SNTOOLKIT_H
# define INCLUDE_SNTOOLKIT_H 1

#include <string>
#include <iostream>
using namespace std;
#if 1

#include <tcl.h>
#include <stdarg.h>
#include <unistd.h>
#include <list>
#include <string>

class SN_Parser_Settings {
  Tcl_Encoding encoding;

  char * cachesize;
  char * dbprefix;
  char * group;
  char * incl_to_pipe;
  char * includename;
  char * pipecmd; 
  char * sn_host;
  char * sn_pid;
  char * xref_filename;

  char includebuf[512];
  char currentFilename[512];

  /*
   * boolean values for command line switches
   */
  int case_sensitive;
  int comments;
  int dialect;
  int drop_usr_headers;
  int local_vars;
  int treat_as_cplusplus;

  FILE * listfp; 
  FILE * outfp;

  list<string> FileList;

public:
  SN_Parser_Settings(){
    encoding = NULL;
    cachesize = NULL;
    dbprefix = NULL;
    group = NULL;
    incl_to_pipe = NULL;
    includename = NULL;
    pipecmd = NULL; 
    sn_host = NULL;
    sn_pid = NULL;
    xref_filename = NULL;

    /*
     * boolean values defaults for command line switches
     */
    case_sensitive = 1;
    comments = 0;
    dialect = 0;
    drop_usr_headers = 0;
    local_vars = 0;
    treat_as_cplusplus = 0;

    listfp = NULL;
    outfp = NULL;

  }


int sn_init();
 int sn_close_db();
void sn_process_options(int argc, char *argv[]);

 const char *GetFilename();
 const char *GetNextFilename();

};
#endif

// These are a collection of C++ classes that can be
// used to collect source code data (symbols and information
// about them) and submit it to the database.
class SN_Symbol {

  public:
  SN_Symbol(){
    SymbolType = -1;
    StartLine = -1;
    EndLine = -1;
    StartColumn = -1;
    EndColumn = -1;
    Attributes = 0;
    HighlightStartLine = -1;
    HighlightEndLine = -1;
    HighlightStartColumn = -1;
    HighlightEndColumn = -1;
  }

  // id_type
  void SetType(int symbol_type);
  int GetType();

  // classname
  void SetClassName(const char *classname);
  const char* GetClassName();

  // identifier, the Symbol we are inserting.
  void SetIdentifier(const char *ident);
  const char* GetIdentifier();
  
  // filename
  void SetFilename(const char *filename);
  const char* GetFilename();

  // start_lineno, the line number the first character
  // of the symbol identifier falls on.
  void SetStartLine(int lineno);
  int GetStartLine();

  // startcol, the column of the first character of
  // of the symbol identifier.
  void SetStartColumn(int startcol);
  int GetStartColumn();

  // endline, the line number the last character of the
  // symbol identifier falls on.
  void SetEndLine(int endline);
  int GetEndLine();

  // endcol, the column of the last character of the
  // symbol identifier.
  void SetEndColumn(int endcol);
  int GetEndColumn();

  // attrib ?
  void SetAttributes(unsigned long attrib);
  unsigned long GetAttributes();

  // returnType
  void SetReturnType(const char *return_type);
  const char* GetReturnType();

  // argsTypes
  void SetArgumentTypes(const char *args_types);
  const char *GetArgumentTypes();

  // argsNames
  void SetArgumentNames(const char *args_names);
  void AddArgumentName(const char *args_name);
  const char *GetArgumentNames();

  // comment
  void SetComment(const char *comment);
  const char *GetComment();

  // highStartLine
  void SetHighlightStartLine(int start_line);
  int GetHighlightStartLine();

  // highEndLine
  void SetHighlightEndLine(int end_line);
  int GetHighlightEndLine();

  // highStartColumn
  void SetHighlightStartColumn(int start_column);
  int GetHighlightStartColumn();

  // highEndColumn
  void SetHighlightEndColumn(int end_column);
  int GetHighlightEndColumn();

  // Send data to database.
  int WriteData();
  int sn_insert_symbol();
  static string FileName;
  static SN_Parser_Settings DBSettings;

  private:
  int SymbolType;
  string ClassName;
  string Identifier;
  int StartLine;
  int EndLine;
  int StartColumn;
  int EndColumn;
  unsigned long Attributes;
  string ReturnType;
  string ArgumentTypes;
  string ArgumentNames;
  string Comment;
  int HighlightStartLine;
  int HighlightEndLine;
  int HighlightStartColumn;
  int HighlightEndColumn;

};


#endif /* INCLUDE_SNTOOLKIT_H */
