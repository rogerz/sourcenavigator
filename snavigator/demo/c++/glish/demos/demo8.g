b := "largo"
print shell( "date", host=b)
print shell( "hostname ; who ", host = b )

t := client( "timer", 3, host=b )

whenever t->ready do
	print $value
