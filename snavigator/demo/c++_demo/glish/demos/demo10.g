function count_value(value, ...)
	{
	local i, count := 0
	for ( i in 1:num_args(...) )
		{
		local arg := nth_arg( i, ... )
		count +:= sum(arg == value)
		}

	return count
	}


print count_value(5,1:10, -5.3, [T,T,T,F], 0:20, [5,5,5,5,6])
