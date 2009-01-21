subseq foo()
	{
	whenever self->feep do
		print self.feep, $name
	}

a := foo()
a->feep(1)
a->feep(2)
