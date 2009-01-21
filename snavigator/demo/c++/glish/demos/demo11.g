t := client("timer 0.5")

whenever t->ready do
	{
	print "<1>", $value
	count +:= 1

	foo := current_whenever()

	if ( count > 5 )
		deactivate
	}

whenever t->ready do
	{
	print "<2>", $value

	if ( count > 5 )
		{
		count +:= 1

		if ( count > 10 )
			activate foo
		}
	}
