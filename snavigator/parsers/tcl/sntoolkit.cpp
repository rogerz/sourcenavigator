
#include "sntoolkit.h"
#include "snptools.h"

 
// idType
//
//

void SN_Symbol::SetType(int symbol_type) {
  SymbolType = symbol_type;
}

int SN_Symbol::GetType() {
  return SymbolType;
};


 
// classname
//
//

void SN_Symbol::SetClassName(const char *classname) {
  ClassName = classname;
}

const char* SN_Symbol::GetClassName() {
  // Warning! This return value won't point to
  // valid memory once the object is destroyed.
  return ClassName.c_str();
}



// identifier, the Symbol we are inserting.
//
//

void SN_Symbol::SetIdentifier(const char *ident) {
  Identifier = ident;
}

const char* SN_Symbol::GetIdentifier() {
  return Identifier.c_str();
}



// filename
//
//

void SN_Symbol::SetFilename(const char *filename) {
  SN_Symbol::FileName = filename;
}

const char* SN_Symbol::GetFilename() {
  return FileName.c_str();
}



// start_lineno
//
// the line number the first character
// of the symbol identifier falls on.

void SN_Symbol::SetStartLine(int lineno) {
  StartLine = lineno;
}

int SN_Symbol::GetStartLine() {
  return StartLine;
}



// startcol
//
// the column of the first character of
// of the symbol identifier.

void SN_Symbol::SetStartColumn(int startcol) {
  StartColumn = startcol;
}

int SN_Symbol::GetStartColumn() {
 return StartColumn;
}



// endline
//
// the line number the last character of the
// symbol identifier falls on.

void SN_Symbol::SetEndLine(int endline) {
 EndLine = endline;
}

int SN_Symbol::GetEndLine(){
 return EndLine;
}



// endcol
//
// the column of the last character of the
// symbol identifier.

void SN_Symbol::SetEndColumn(int endcol) {
  EndColumn = endcol;
}

int SN_Symbol::GetEndColumn() {
  return EndColumn;
}



// attrib ?
//
// What is this for?

void SN_Symbol::SetAttributes(unsigned long attrib) {
  Attributes = attrib;
}

unsigned long SN_Symbol::GetAttributes() {
  return Attributes;
}



// returnType
//
//

void SN_Symbol::SetReturnType(const char *return_type) {
  ReturnType = return_type;
}

const char* SN_Symbol::GetReturnType() {
  return ReturnType.c_str();
}



// argsTypes
//
//

void SN_Symbol::SetArgumentTypes(const char *args_types) {
  ArgumentTypes = args_types;
}

const char *SN_Symbol::GetArgumentTypes() {
  return ArgumentTypes.c_str();
}



// argsNames
//
//

// SN_Symbol::SetArgumentNames
//
// will set the ArgumentNames field over writing any previous data.
//

void SN_Symbol::SetArgumentNames(const char *args_names) {
  ArgumentNames = args_names;
}

const char *SN_Symbol::GetArgumentNames() {
  return ArgumentNames.c_str();
}

// SN_Symbol::AddArgumentName
//
// will add an argument to the ArgumentNames field without over writing
// any previously set argument names.
//

void SN_Symbol::AddArgumentName(const char *args_name) {
  if (ArgumentNames.length()==0) {
     ArgumentNames = args_name;
  } else {
     ArgumentNames = ArgumentNames + ", " + args_name;
  } 
}


// comment
//
//

void SN_Symbol::SetComment(const char *comment) {
  Comment = comment;
}

const char *SN_Symbol::GetComment() {
  return Comment.c_str();
}



// highStartLine
//
//

void SN_Symbol::SetHighlightStartLine(int start_line) {
  HighlightStartLine = start_line;
}

int SN_Symbol::GetHighlightStartLine() {
  return HighlightStartLine;
}



// highEndLine
//
//

void SN_Symbol::SetHighlightEndLine(int end_line) {
  HighlightEndLine = end_line;
}

int SN_Symbol::GetHighlightEndLine() {
  return HighlightEndLine;
}



// highStartColumn
//
//

void SN_Symbol::SetHighlightStartColumn(int start_column) {
  HighlightStartColumn = start_column;
}

int SN_Symbol::GetHighlightStartColumn() {
  return HighlightStartColumn;
}



// highEndColumn
//
//

void SN_Symbol::SetHighlightEndColumn(int end_column) {
  HighlightEndColumn = end_column;
}

int SN_Symbol::GetHighlightEndColumn() {
  return HighlightEndColumn;
}

// Write it out.
//
//

int SN_Symbol::WriteData() {
  /* cout << "(" << SymbolType << ")" 
       << FileName << ": " \
       << ClassName << " " << Identifier \
       << "(" << ArgumentNames \
       << ") ((" << StartLine << "." << StartColumn \
       << ", " << EndLine << "." << EndColumn \
       << "), (" << HighlightStartLine << "." << HighlightStartColumn \
       << ", " << HighlightEndLine << "." << HighlightEndColumn \
       << "))\n";*/
  
   sn_insert_symbol();
}

#if 1


void
SN_Parser_Settings::sn_process_options(int argc, char *argv[])
{
  int opt;

  /* Character set encoding (as defined by Tcl). */
  Tcl_FindExecutable(argv[0]);

  while ((opt = getopt(argc, argv, "I:n:s:hy:g:p:c:x:i:luB:e:tCrDS:H:O:P:")) != 
EOF)
  {
    switch (opt)
    {
      case 'B':
        /* silently ignore according to zkoppany */
        break;

      case 'c':
        cachesize = optarg;
        break;
 
      case 'C':
        treat_as_cplusplus = 1;
        break;

      case 'D':
        /* silently ignore according to zkoppany */
        break;

      case 'e':
        if ((encoding = Tcl_GetEncoding(NULL, optarg)) == NULL)
          {
            printf("Unable to locate `%s' encoding\n", optarg);
            exit(0);
          }
        break;
        
      case 'g':
        group = optarg;
        break;

      case 'h':
        break;

      case 'H':
        sn_host = optarg;
        break;

      case 'i':
        incl_to_pipe = optarg;
        break;

      case 'I':
        includename = optarg;
        break;

      case 'l':
        local_vars = 1;
        break;

      case 'n':
        dbprefix = optarg;
        break;

      case 'p':
        pipecmd = optarg;
        break;
 
      case 'P':
        sn_pid = optarg;
        break;

      case 'r':
        comments = 1;
        break;

      case 's':
        if ((outfp = fopen(optarg, "a")) == NULL)
        {
          printf("could not create %s\n", optarg);
          exit(0);
        }
        break;

      case 'S':
        /* silently ignore according to zkoppany */
        break;

      case 't':
        drop_usr_headers = 1;
        break;

      case 'u':
        case_sensitive = 0;
        break;

      case 'x':
        xref_filename = optarg;
        break;

      case 'y':
	/*        listfp = fopen(optarg, "r");
        if (listfp == NULL)
        {
          printf("could not open %s\n", optarg);
	  //  sn_panic();
	  }*/
	FileList.push_back(string(optarg));
        break;

      default:
        assert(0);
        break;
    }
  }
}

const char* SN_Parser_Settings::GetFilename() {
  return (FileList.begin())->c_str();
}

const char* SN_Parser_Settings::GetNextFilename() {

  FileList.pop_front();

  if (FileList.empty()) {
       return NULL;
  }

  return (FileList.begin())->c_str();
}

int
SN_Parser_Settings::sn_init()
{
  if (pipecmd != NULL)
  {
    if (Paf_Pipe_Create(pipecmd, dbprefix, incl_to_pipe, cachesize,
                        sn_host, sn_pid) < 0)
      { printf("ERROR Pipe error: %s\n", pipecmd);
      //      sn_message("Pipe error: %s\n", pipecmd);
    }
  }
  else
  {
    Paf_db_init_tables(dbprefix, cachesize, NULL);
  }

  /*  if (xref_filename != NULL && !(cross_ref_fp = fopen(xref_filename, "a")))
  {
    sn_message("Open error: %s\n", xref_filename);
    sn_exit();
    }*/

  return(0);
}


int
SN_Parser_Settings::sn_close_db()
{
  return(Paf_Pipe_Close());
}

int
SN_Symbol::sn_insert_symbol()
{
  return(put_symbol(SymbolType, strdup(ClassName.c_str()), strdup(Identifier.c_str()),\
                    strdup(FileName.c_str()), StartLine, StartColumn, EndLine,\
                    EndColumn, Attributes, strdup(ReturnType.c_str()), strdup(ArgumentTypes.c_str()), \
                    strdup(ArgumentNames.c_str()), strdup(Comment.c_str()),\
                    HighlightStartLine, HighlightStartColumn,\
                    HighlightEndLine, HighlightEndColumn));
}


#endif
