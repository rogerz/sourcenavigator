package require tcltest

proc save_file { fname data {encoding default} } {
    set fd [open $fname w]
    if {$encoding != "default"} {
        fconfigure $fd -encoding $encoding
    }
    puts -nonewline $fd $data
    close $fd
    return $fname
}

# Read data from file in utf-8 format.
# Pass "default" to use the system encoding.

proc read_file { fname {encoding utf-8} } {
    set fd [open $fname r]
    if {$encoding != "default"} {
        fconfigure $fd -encoding $encoding
    }
    set data [read -nonewline $fd]
    close $fd
    return $data
}

# Return string for id, see sn.h for declarations

proc paf_get { id } {
    array set paf_values {
0  PAF_FILE
1  PAF_TYPE_DEF
2  PAF_CLASS_DEF
3  PAF_MBR_FUNC_DEF
4  PAF_MBR_VAR_DEF
5  PAF_ENUM_DEF
6  PAF_CONS_DEF
7  PAF_MACRO_DEF
8  PAF_FUNC_DEF
9  PAF_SUBR_DEF
10 PAF_GLOB_VAR_DEF
11 PAF_COMMON_DEF
12 PAF_COMMON_MBR_VAR_DEF
13 PAF_CLASS_INHERIT
14 PAF_FILE_SYMBOLS
15 PAF_CROSS_REF_BY
16 PAF_CROSS_REF
17 PAF_MBR_FUNC_DCL
18 PAF_FUNC_DCL
19 PAF_ENUM_CONST_DEF
20 PAF_UNION_DEF
21 PAF_FRIEND_DCL
22 PAF_NAMESPACE_DEF
23 PAF_EXCEPTION_DEF
24 PAF_LOCAL_VAR_DEF
25 PAF_VAR_DCL
26 PAF_INCLUDE_DEF
27 PAF_COMMENT_DEF
28 PAF_CROSS_REF_CPP
29 PAF_REF_UNDEFINED
30 PAF_CROSS_REF_FILE
    }

    if {![info exists paf_values($id)]} {
        return "UNKNOWN"
    } else {
        return $paf_values($id)
    }
}

# Scan output from a parser and subst paf string
# values into lines that start with a number
# followed by a semicolon.

proc paf_subst { data } {
    set buf ""
    foreach line [split $data \n] {
        if {[regexp {^([0-9]+);(.*)$} $line whole num rest]} {
            set paf_str [paf_get $num]
            append buf $paf_str\;$rest\n
        } else {
            append buf $line\n
        }
    }
    return [string map {\1 <>} $buf]
}

# Return the browser that will handle the given file
# extension.

proc get_browser { fname } {
    global Parser_Info
    set type [sn_get_file_type $fname]
    if {$type == "others"} {
        error "can't find specific browser for \"$fname\""
    }
    return $Parser_Info($type,BROW)
}


proc browse { fname {options {}} } {
    set debug 0

    set browser [get_browser $fname]
    # Pass just the extension (like .c) when using -y filename
    if {[string match ".*" $fname]} {
        set cmd [list exec $browser]
        foreach opt $options {
            lappend cmd $opt
        }
    } else {
        set cmd [list exec $browser]
        foreach opt $options {
            lappend cmd $opt
        }
        lappend cmd $fname
    }
    if {$debug} {
        puts "now to eval \"$cmd\""
    }
    set results [eval $cmd]
    if {$debug} {
        puts "got results\n$results\n"
    }
    set results [paf_subst $results]
    set results [encoding convertfrom utf-8 $results]
    if {$debug} {
        puts "returning\n$results\n"
    }
    return $results
}

proc browse_xref { fname {options {}} } {
    file delete xout
    save_file xout ""
    lappend options -x xout
    browse $fname $options
    return [paf_subst [read_file xout]]
}
