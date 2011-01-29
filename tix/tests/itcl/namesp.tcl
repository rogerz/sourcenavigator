# This file tests the pixmap image reader
#

proc About {} {
    return "This file performs test on name space"
}

proc Test {} {
    namespace mySpace {
	variable hsl ".hsl"
	proc creatHSL {} {
	    global hsl
	    tixScrolledHList $hsl
	}
	proc packHSL {} {
	    global hsl
	    pack $hsl
	}
    }
    mySpace::creatHSL
    mySpace::packHSL
}
