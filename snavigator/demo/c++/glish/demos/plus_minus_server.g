pm := client( "plus_minus_server" )

whenever pm->answer do
	{
	print $value.sum
	print $value.difference
	}

# Create a record with a 10 element "x" field of the integers from 1 to 10,
# and an 11 element "y" field of floats 2.0, 2.1, 2.2, ..., 3.0.
sample_record := [x=1:10, y=seq(2,3,.1)]

pm->compute( sample_record )

# We can also construct record values on the fly:

pm->compute( y=[1,3,4,6,5], x=[-5.2, sample_record.x[2:4] * 3] )

# And we're all done with the plus/minus server.
pm->terminate()
