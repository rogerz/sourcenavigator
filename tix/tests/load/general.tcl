# This file tests the pixmap image reader
#

proc About {} {
    return "This file performs general test on Tix w/ Tk 4.1 dynamic loading"
}

proc Test {} {
    if [tixStrEq [info commands tix] tix] {
	return
    }

    if ![file exists ../../unix-tk4.1/libtix.so] {
	puts "File ../../unix-tk4.1/libtix.so doesn't exist."
	puts "Dynamic loading skipped."
	return
    }

    test {load ../../unix-tk4.1/libtix.so Tix}
    test {tixComboBox .c}
    test {pack .c}
}
