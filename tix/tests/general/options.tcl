proc About {} {
    return "Testing the option configuration of the Tix widgets"
}

proc Test {} {
    test {tixComboBox .c -xxxxx} 	{missing}
    test {tixComboBox .c -xxxxx xxx} 	{unknown}
    test {tixComboBox .c -d xxx} 	{ambi}
    test {tixComboBox .c -disab 0}	{ambi} 
    test {tixComboBox .c -disablecal 0}
    Assert {[.c cget -disablecallback] == 0}
    Assert {[.c cget -disableca] == 0}
    test {tixComboBox .d -histl 10} 
    Assert {[.d cget -histlimit] == 10}
    Assert {[.d cget -histlim] == 10}
    Assert {[.d cget -historylimit] == 10}
}
