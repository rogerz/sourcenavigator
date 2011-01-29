/* shell_sqrt: a Glish "shell" client square-root server
 *
 * Simple Glish clients can be written as programs that simply read
 * event values from stdin and write event values to stdout, without
 * any knowledge of Glish.  This program is a "square-root" server
 * that expects numeric values to appear on its stdin; it writes the
 * square-root of these values to stdout.
 *
 * This program is built using "cc -o shell_sqrt shell_sqrt.c -lm".  See
 * "shell_sqrt.g" for a Glish program that uses the server.
 */

#include <stdio.h>
#include <math.h>

main()
	{
	char buf[512];

	/* While there's another line available on stdin, fetch it. */
	while ( fgets( buf, sizeof buf, stdin ) )
		{
		/* Treat the line as a double value and print its
		 * square-root.
		 */
		double val = atof( buf );
		double result = sqrt( val );

		printf( "%.6f\n", result );

		/* We have to flush the output buffer in order to
		 * send this event on its way to Glish.
		 */
		fflush( stdout );
		}
	}
