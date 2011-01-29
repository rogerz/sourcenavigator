proc About {} {
    return "Testing ScrolledListBox"
}

proc Test {} {
    set w [tixScrolledListBox .listbox]
    pack $w

    foreach item {{1 1} 2 3 4 5 6} {
	$w subwidget listbox insert end $item
    }

    Click [$w subwidget listbox] 30 30

    destroy $w
}
