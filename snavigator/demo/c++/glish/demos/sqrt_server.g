# Create a glish client using the "sqrt_server" executable.  Assign the
# client to the variable "server".
server := client( "sqrt_server" )


# Anytime the server generates an "answer" event, print it.
whenever server->answer do
	print $value


# Catch all "error" events, too, and print them.
whenever server->error do
	print "sqrt_server error:", $value


# Try first getting the square-root of 2.
server->compute( 2. )


# If we had used "2" instead of "2.0" then the event value would
# be of type integer instead of double, and we'd get an "error" event
# from the square-root server:
server->compute( 2 )

# (Though with a little more work we could make sqrt_server accept
# arguments of any numeric type.)


# Now get the square-roots of the first 10 numbers.  By specifying
# the first member of the array as a "double", all the other elements
# are automatically converted to "double", too.
server->compute( [1., 2, 3, 4, 5, 6, 7, 8, 9, 10] )


# Here's another way to write that.  "1:10" generates an array of
# integer values from 1 through 10, and as_double() converts it
# to an array of doubles.
server->compute( as_double( 1:10 ) )


# Finally, if we send something other than a "compute" event, we get
# an "error" event:
server->doit( 2.0 )


# Note that at the moment you must ^C to terminate glish (this termination
# will also propagate to "server").  If "server" would terminate on its
# own (or, say, you kill it using the "kill" program) then glish will
# notice that there are no more clients running and will exit.  But as
# long as the server is running, glish assumes that it may spontaneously
# generate events in which case glish must keep running.
#
# In the near future you will be able to send a "terminate" event to
# a client which will automatically stop it:
#
#	server->terminate()
