function start_hex()
	{
	return shell( "awk '{ printf( \"%x\\n\", $1 ) }'", async=T, suspend=F )
	}

function do_hex(ref hex_task)
	{
	whenever hex_task->established do
		{
		print "count = ", count
		for ( i in 1:10 )
			hex_task->stdin( i )

		hex_task->EOF()
		}

	whenever hex_task->stdout do
		{
		print hex_task.stdout
		}

	await only hex_task->done except hex_task->*;
	}

for ( count in 1:5 )
	do_hex( start_hex() )
