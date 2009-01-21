# Here's another way of writing a shell client that's a "square-root"
# server: just use the "awk" program.  Compare this with "shell_sqrt.g".

server := shell( "awk '{ print sqrt($1) }'", async=T )

whenever server->stdout do
	print $value

server->stdin( 2 )
server->stdin( "3.14159" )
server->stdin( [1, 2, 3, 4] )
server->stdin( "foo" )
server->foo( 5 )


# Note that often when using a Unix utility such as "awk" as a shell
# client, an EOF *must* be sent before any output events show up.
# This is because the utilities usually buffer their output.  If
# the following send-event is commented out then no "server" output
# will appear.  In contrast, if the EOF event is commented out from
# "shell_sqrt.g" then all the server output event values still show
# up (because shell_sqrt.c is careful to flush its output every time
# it produces an event); all that will happen is that Glish will not
# terminate, because it is waiting for "server" to possibly generate
# some more events.

server->EOF()
