# Create a "shell" client using the program "shell_sqrt".  The "async=T"
# means that this shell program should run *asynchronously*; i.e., Glish
# should not wait for it to produce a value and exit but rather should
# treat it like a regular Glish client that runs in parallel with Glish.
server := shell( "shell_sqrt", async=T )


# shell clients produce "stdout" events.  Whenever our server does so,
# we print the value of the event.
whenever server->stdout do
	print $value


# Note that since shell clients always produce "stdout" events, we are
# not able to detect error conditions as separate events.


# Use the server to print the square-root of 2.  Because the server is
# a shell client, it always sees its input as a string, so we don't have
# to worry about whether we're sending it an integer value or a double
# value ...

server->stdin( 2 )


# ... or for that matter, a string value:

server->stdin( "3.14159" )


# As it's presently written, if we send the server an array it just
# extracts the square-root of the first element.  Making it deal with
# arrays is not too hard, but at some point it becomes easier to
# just write the server as a true Glish client instead of as a shell client.
server->stdin( [1, 2, 3, 4] )


# It's difficult to put error-handling into shell clients.  The following
# result in a "stdout" event with a value of 0, since naively converting
# "foo" to a number yields a value of 0.
server->stdin( "foo" )


# If we send other than a "stdin" event to a shell client we get an
# error message.
server->foo( 5 )


# With shell clients, if we send an "EOF" event then an end-of-file
# appears on the client's input.  This will cause the client to exit;
# Glish will then notice that there are no running clients and exit
# itself.
server->EOF()
