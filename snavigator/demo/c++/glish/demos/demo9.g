# a := shell( "man date", async=T )
a := shell( "man date", async=T, host="bigelow" )

whenever a->stdout do
	print a.stdout
