package Glish;


use Carp;		## supplies croak
require Exporter;
require AutoLoader;	## supplies AUTOLOAD... requires __END__
require DynaLoader;
@ISA = (Exporter, AutoLoader, DynaLoader);

@EXPORT = qw(nextevent postevent waitingevent standalone reply addfds
	     TYPE_ERROR TYPE_BOOL TYPE_BYTE TYPE_SHORT TYPE_INT
	     TYPE_FLOAT TYPE_DOUBLE TYPE_STRING TYPE_RECORD
	     TYPE_COMPLEX TYPE_DCOMPLEX );

sub AUTOLOAD {
    if ($AUTOLOAD =~ /::(_?[a-z])/) {
        $AutoLoader::AUTOLOAD = $AUTOLOAD;
        goto &AutoLoader::AUTOLOAD;
    }
    local $typename = $AUTOLOAD;
    $typename =~ s/.*:://;
    $val = type($typename);
    if ($! != 0) {
        if ($! =~ /Invalid/) {
            croak "$typename is not a valid Glish type";
        }
        else {
	    croak "Error no Glish type $typename.";
        }
    }
    eval "sub $AUTOLOAD { $val }";
    goto &$AUTOLOAD;
}

bootstrap Glish;

# Preloaded methods go here.  Autoload methods go after __END__, and are
# processed by the autosplit program.

1;
__END__

