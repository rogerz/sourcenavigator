# generic Tcl utility procs

proc pl { list args } {

	set len [llength $args]

	if {$len == 0} {
		foreach elem $list {
			puts stdout ${elem}
    		}
	} elseif {$len == 1} {
		#then they must have given a stream to print to

		set stream [lindex $args 0]

		foreach elem $list {
			puts $stream "${elem}"
    		}
	} elseif {$len == 2} {
		#they have given a list a stream and a
		#char to suppound each elem in the list

		set stream [lindex $args 0]
		set delim [lindex $args 1]

		foreach elem $list {
			puts $stream "${delim}${elem}${delim}"
    		}
	} elseif {$len == 3} {
		#they have given a list a stream and a
		#left surround and a right surround char

		set stream [lindex $args 0]
		set rdelim [lindex $args 1]
		set ldelim [lindex $args 2]

		foreach elem $list {
			puts $stream "${rdelim}${elem}${ldelim}"
    		}
	} else {
		error "usage : pl list ?stream? ?delim? ?delim2?"
	}
}

proc pv { args } {
    foreach var $args {
	#puts "now to do \"upvar 1 $var val\""
	upvar 1 $var val
	if { ! [info exists val] } {
	    error "$var does not exist"
	}
	puts "$var = \"$val\""
	flush stdout
    }
}



# Pause script execution for msecs

proc pause { {msecs 1000} } {
    global pause

    if {! [info exists pause(number)]} {
        set pause(number) 0
    }

    set num [incr pause(number)]
    set pause($num) 0

    after $msecs "set pause($num) 1"
    vwait pause($num)
    unset pause($num)
}

# The delay command can be used to
# run a command after a given amount
# of time or wait a given amount
# of time before continuing.

# Usage:
#
# When invoked with one or two
# arguments, delay will run the
# given commands after the
# specified amount of time
# has elapsed. The default
# delay multiplier is 1.
#
# delay {set name bob}
# delay 2 {set name doug}
#
# When invoked with 0
# or 1 arguments (where the
# argument is an integer)
# delay will pause execution
# of the script for the
# specified multiple of
# the delay time. The
# default delay multiplier
# is 1.
#
# delay
# delay 2
#

proc delay { args } {
    global delay

    if {! [info exists delay]} {
        set delay 1000
    }

    set len [llength $args]

    if {$len > 2} {
        error "usage: delay ?delay_multiplier? ?cmd?"
    } elseif {$len == 2} {
        set multiplier [lindex $args 0]
        set cmd [lindex $args 1]

        return [after [expr {$multiplier * $delay}] $cmd]
    } elseif {$len == 1} {
        set maybe_mult [lindex $args 0]
        if {! [catch {incr maybe_mult}]} {
            # It must be a wait multiplier
            set multiplier [lindex $args 0]
            pause [expr {$multiplier * $delay}]
            return
        } else {
            set cmd [lindex $args 0]
            return [after $delay $cmd]
        }
    } else {
        # If 0 arguments, pause for 1 time unit
        pause $delay
        return
    }
}

# Set the amount of time that
# a delay call will wait
# until evaluating a cmd.

proc set_delay { msecs } {
    global delay
    set delay $msecs


# FIXME: We might need to worry in the case
# that the delay is less that around a second.
# This is because a mouse_click sort of event
# will be doing its own pause in a callback
# so that could throw the timing off if the
# mouse events overlap with the delay timeout!
#
# Example:
#
# delay 1 {mouse_click .b}
# delay 2 {mouse_click .b}
# delay 3
#
# Could the delay 3 finish waiting
# before the second mouse click
# got done executing???
}


# Cat a string to a file

proc writefile { file data } {
  set fd [open $file w]
  puts $fd $data
  close $fd
}



proc enter_text { widget text } {
    if {[winfo class $widget] == "Entry"} {
        # Delete any old text
        focus -force $widget
        mouse_triple_click $widget
        _keypress $widget Delete

        type_string $widget $text
    }
}

# mouse_click : simulate a mouse being clicked in the widget

proc mouse_click { widget args } {
    raise [winfo toplevel $widget]
    pause 100
    eval event generate $widget <Enter> $args
    pause 100
    eval event generate $widget <ButtonPress-1> $args
    pause 100
    eval event generate $widget <ButtonRelease-1> $args
}

# mouse_double_click : simulate a mouse being double clicked in the widget

proc mouse_double_click { widget args } {
    pause 100
    eval event generate $widget <Enter> $args
    pause 100
    eval event generate $widget <ButtonPress-1> $args
    pause 100
    eval event generate $widget <ButtonRelease-1> $args
    pause 100
    eval event generate $widget <ButtonPress-1> $args
    pause 100
    eval event generate $widget <ButtonRelease-1> $args
}

# mouse_triple_click : Duh!

proc mouse_triple_click { widget args } {
    pause 100
    eval event generate $widget <Enter> $args
    pause 100
    eval event generate $widget <ButtonPress-1> $args
    pause 100
    eval event generate $widget <ButtonRelease-1> $args
    pause 100
    eval event generate $widget <ButtonPress-1> $args
    pause 100
    eval event generate $widget <ButtonRelease-1> $args
    pause 100
    eval event generate $widget <ButtonPress-1> $args
    pause 100
    eval event generate $widget <ButtonRelease-1> $args
}

proc kids { win } {
    set kids $win\n
    #puts "starting with $win => \"[string trim $kids]\""
    foreach child [winfo children $win] {
	#puts "child is \"$child\""
	#append kids $child\n
	set kkids [kids $child]
	if {[string trim $kkids] != ""} {
            append kids $kkids
	}
    }
    return $kids
}

# Take a snapshot of the current toplevel widgets
# we just record the name of each widget so that
# we can tell if it is still visible later

proc toplevels { } {
    set tops [list]
    foreach win [kids .] {
        #puts "checking possible toplevel $win"
        #if {[isa_toplevel $win]} {
        #    puts "window state is [wm state $win]"
        #}
	if {[isa_toplevel $win] &&
            [wm state $win] == "normal"} {
	    lappend tops $win
        }
    }
    return $tops
}

proc isa_toplevel { win } {
    if {[itcl::find object $win] == $win &&
	[$win isa itk::Toplevel]} {
	return 1
    }
    return [expr {! [catch {$win cget -screen}]}]
}


# Listbox selection utilities.

proc listbox_select_index { lb index } {
    # Map the index to x and y coords in
    # the middle of the given listbox.

    set end [$lb index end]
    if {$index >= $end} {
        error "Index $index is bigger than the number of entries $end"
    }

    foreach {x y width height} [$lb bbox $index] break

    set midx [expr {$x + $width / 2}]
    set midy [expr {$y + $height / 2}]

    mouse_click $lb -x $midx -y $midy
}

# Select the item in the listbox that matches the
# given pattern using "string match"

proc listbox_select_entry_pattern {lb pattern} {
    # Walk over each item in the listbox
    # to see if we can find one that matches the pattern.

    for {set i 0 ; set max [$lb index end]} {$i < $max} {incr i} {
        set item [$lb get $i]
        #puts "item at index $i is \"$item\""
        if {[string match $pattern $item]} {
            listbox_select_index $lb $i
        }
    }
}

# This method will close a toplevel window from the GUI
# regression testing module.

proc close_toplevel { top } {
    # We should just be able to call
    # destroy $top (or)
    # itcl::delete object $top
    # but that does not work
    # We currently fake out the window
    # by pretending to hit the escape
    # key to so the window will close.

    keyevent $top Escape
}

proc warp { top x y } {
    event generate $top <1> -x $x -y $y -warp 1

    # Snap the focus to the last window that had it
    focus -force [focus -lastfor $top]

    pause 1000 
    event generate $top <1> -x $x -y $y
    pause 100                       
    event generate $top <1> -x $x -y $y
    pause 100
    event generate $top <Escape>
}

# Reset the focus back to the targeted application
# you would need to do this before sending key
# press events. If the window you pass in is
# not a toplevel, an attempt will be made to
# force the focus to that window.

proc _refocus { window } {
    set toplevel [winfo toplevel $window]
    set last [focus -lastfor $window]
    if {$toplevel == $last} {
        #puts "_refocus(1) $window"
        focus -force $window
    } else {
        #puts "_refocus(2) $last"
        focus -force $last
    }
}

proc _keypress { toplevel key } {
    set keysym [_keypress_lookup $key]

    pause 50
    event generate $toplevel <KeyPress-$keysym>
    pause 50
    event generate $toplevel <KeyRelease-$keysym>
    pause 50
}

# FIXME: doc this function

proc keyevent { window event } {
    _refocus $window
    _keypress $window $event
}

# Type a string into the given widget using
# simulated events.

proc type_string { widget string } {
    foreach letter [split $string ""] {
        _keypress $widget $letter
    }
}

# Lookup an event in the keypress table
# keypress table. For example:
# Q -> Q
# . -> period
# / -> slash
# Delete -> Delete
# Escape -> Escape

proc _keypress_lookup { char } {
    global keypress_lookup

    if {! [info exists keypress_lookup]} {
        _init_keypress_lookup
    }

    if {$char == ""} {
        error "empty char"
    }

    if {[info exists keypress_lookup($char)]} {
        return $keypress_lookup($char)
    } else {
        return $char
    }
}

# Set up keypress table.
# Find em with [bind . <KeyPress> {puts "%K %k"}]

# FIXME: this will have internationalization issues!

proc _init_keypress_lookup { } {
    global keypress_lookup

    scan A %c start
    scan Z %c finish

    for {set i $start} {$i <= $finish} {incr i} {
        set l [format %c $i]
        set keypress_lookup($l) $l
    }

    scan a %c start
    scan z %c finish

    for {set i $start} {$i <= $finish} {incr i} {
        set l [format %c $i]
        set keypress_lookup($l) $l
    }

    scan 0 %c start
    scan 9 %c finish

    for {set i $start} {$i <= $finish} {incr i} {
        set l [format %c $i]
        set keypress_lookup($l) $l
    }

    array set keypress_lookup [list \
        " " space \
        ! exclam \
        \" quotedbl \
        \# numbersign \
        \$ dollar \
        % percent \
        & ampersand \
        ( parenleft \
        ) parenright \
        * asterisk \
        + plus \
        , comma \
        - minus \
        . period \
        / slash \
        : colon \
        \; semicolon \
        < less \
        = equal \
        > greater \
        ? question \
        @ at \
        \[ bracketleft \
        \\ backslash \
        \] bracketright \
        ^ asciicircum \
        _ underscore \
        \{ braceleft \
        | bar \
        \} braceright \
        ~ asciitilde \
        ' apostrophe \
        "\n" Return]
}
