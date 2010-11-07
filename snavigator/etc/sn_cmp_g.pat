# Source-Navigator regular expressions for compiler, debugger and grep patterns.
# Source-Navigator supports also blanks, so the patterns must be so defined
# that blanks could be a part of the file name

# "filename.c", line 789
"([^"]+)",[ 	]+line[ 	]+([0-9]+)

# filename.c, line 789
([^ 	]+),[ 	]+line[ 	]+([0-9]+)

# line 789, filename.c,
line[ 	]+([0-9]+),[ 	]+([^, 	]+)

# [filename.c:789]
\[([^\[: 	]+):([0-9]+)\]

# d:/filename.c:856 - deactivated, its buggy w/ complicated patterns
#([A-Z|a-z]:[^:]+):[       ]*([0-9]+)

# filename.c:789 - most excellent for grep pane
([^:]+):[[:space:]]*([0-9]+)

# filename.c(789)
([^	]+\.[^	]+)\(([0-9]+)\)

# filename.c(789,11) or filename.c(789.11)
([^	]+\.[^	]+)\(([0-9]+[,.][ ]*[0-9]+)\)

# /dir/filename.c, 789
([^/	]+),[ 	]+([0-9]+)
