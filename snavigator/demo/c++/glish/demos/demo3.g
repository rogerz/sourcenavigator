subsequence power( exponent )
	{
	whenever self->compute do
		self->ready( $value ^ exponent )
	}


square := power( 2 )
cube := power( 3 )

square->compute( 6 )
cube->compute( [2, 5, 10.1] )

whenever square->ready, cube->ready do
	print $value
