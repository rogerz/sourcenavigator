proc About {} {
    return "Testing the TixSelect widget"
}

proc Test {} {
    set dis  [tix option get disabled_fg]
    set norm [tix option get fg]

    # Create with a normal state
    #
    #
    tixSelect .foo -allowzero 0 -radio 1 -label "Foo:" \
	-state normal
    .foo add "1" -text "One"
    .foo add "2" -text "Two"
    pack .foo

    Assert {[.foo subwidget label cget -foreground] == $norm}
    .foo config -state normal
    .foo config -state normal
    Assert {[.foo subwidget label cget -foreground] == $norm}
    .foo config -state disabled
    Assert {[.foo subwidget label cget -foreground] == $dis}
    .foo config -state normal
    Assert {[.foo subwidget label cget -foreground] == $norm}

    update
    destroy .foo

    tixSelect .foo -allowzero 0 -radio 1 -label "Foo:" \
	-state disabled
    .foo add "1" -text "One"
    .foo add "2" -text "Two"
    pack .foo

    Assert {[.foo subwidget label cget -foreground] == $dis}
    .foo config -state normal
    Assert {[.foo subwidget label cget -foreground] == $norm}
    .foo config -state normal
    Assert {[.foo subwidget label cget -foreground] == $norm}
    .foo config -state disabled
    Assert {[.foo subwidget label cget -foreground] == $dis}
    .foo config -state normal
    Assert {[.foo subwidget label cget -foreground] == $norm}
}
