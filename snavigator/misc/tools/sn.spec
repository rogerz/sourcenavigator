Summary: Cygnus Source-Navigator
Vendor: Cygnus Solutions
URL: http://www.cygnus.com/
Name: source-navigator
%define SNVER 4.0.6
Version: %{SNVER}
Release: 1 
Copyright: commercial
#Buildroot: /tmp/buildroot.sn
Group: Development/Tools
#Icon: 

%description

Source-Navigator is a unique source code analysis tool that is ideal for
any situation where developers are working with an unfamiliar or complex
code base -- new engineers on projects, reengineering or code inheritance
situations, or large-scale, team driven projects.

Source-Navigator parses multi language code and builds a powerful database
of project symbol information.  Then, with S-N's intuitive GUI, developers
can navigate and browse symbol tables, class structure, cross reference
realationship, inculde structures, and more.  S-N also provides efficient
text search tools for strings in the project files as well as symbols in
the database.

With the APIs, S-N integrates into existing environments including
configuration management systems, as a framework for launching other tools
or IDEs, and allows direct access to the GUI and database for customized
tools.

%build
echo "Nothing to do for build"

# FIXME: Documentation should be listed as doc file types.
%files

/usr/cygnus/sn-4.0.6

%pre

[ -r /usr/cygnus ] || ( mkdir /usr/cygnus && chmod 755 /usr/cygnus )
