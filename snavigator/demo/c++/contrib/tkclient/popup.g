func popup(msg, title = '')
	{
	local p := client("tkclient gbutton.tk popup.tk")

	local result :=
		request p->TclN( [
				paste('popup .p {', msg, '}'),
				'pack .p',
				paste('wm title . {', title, '}') ])

	send p->terminate()

	return result
	}

func create_popup(msg, title = '',
	config='-relief ridge -borderwidth 10 -font -adobe-helvetica-bold-r-normal--18-180-75-75-p-103-iso8859-1')
	{
	popup := client("tkclient")
	request popup->sync()	# make sure the popup's running

	send popup->TclN(
		[ paste('set popup_text {', msg, '}'),
		  paste('label .l -textvariable popup_text', config),
		  'pack .l',
		  'wm transient . .',
		  paste('wm title . {', title, '}') ])
	}

func message(msg)
	{
	send popup->Tcl(paste('set popup_text {', msg, '... }'))
	send popup->Tcl('wm deiconify .')
	request popup->sync()
	}

func message_done()
	{
	send popup->Tcl('wm withdraw .')
	}
