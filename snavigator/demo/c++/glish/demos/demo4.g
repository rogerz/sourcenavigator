subseq foo()
	{
	whenever self->doit do
		self->done( self.doit + 3 )
	}

a := foo()
b := foo()

whenever b->done, a->done do
	print b.done, a.done

b->doit( 5 )
a->doit( 9 )
