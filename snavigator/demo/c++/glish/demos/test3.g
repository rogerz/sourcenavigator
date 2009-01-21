# Simple test of "timer" client.

t := client("timer -oneshot 0")

count := 0

whenever t->ready do
	{
	count := count + 1

	if ( count <= 5 )
		{
		print count
		t->interval( count )
		}
	else
		exit
	}
