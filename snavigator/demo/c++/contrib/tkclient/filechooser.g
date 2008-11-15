# Creates a file chooser for the given directory, starting with the
# given title.  Returns the name of the file chosen, or an empty (0-length)
# string if "cancel" was selected.

func filechooser(directory, title)
	{
	local p := client("tkclient gbutton.tk filechooser.tk")

	local tcl := [	paste('filechooser .c', directory, ' "*"'),
			'pack .c',
			paste('wm title . {', title, '}')	]

	local file := request p->TclN(tcl)

	send p->terminate()

	return file
	}
