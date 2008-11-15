if ( is_boolean(script) )
	{
	print "Master"
	s := client("SUN4/glish", "demos/test4.g", "-- arg1 arg2 arg3")
	whenever s->* do
		print $name, $value

	s->foo( 1:5 )
	s->foo( "hello there" )
	}

else
	{
	print "Slave, args are:", argv
	script->fleabark("testing", "testing")

	whenever script->foo do
		{
		print "Slave", $name, $value
		count +:= 1
		if ( count == 2 )
			exit
		}
	}
