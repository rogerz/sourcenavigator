// $Header$

#ifndef channel_h
#define channel_h


// Channels manage a communication channel to a process.  The channel
// consists of two file descriptors, one for reading from the process
// and the other for writing to the process.
//
// The Channel class doesn't do much at the moment.  It used to do a lot
// more (providing buffering and line-oriented reads) but presently Glish
// clients use binary I/O to transmit messages, so Channel's have become
// little more than a way to bundle together two file descriptors.  We
// retain some of the Channel abstraction, however, in case in the future
// we wish to return to buffering.

// Channels have a state associated with them.  CHAN_VALID is an ordinary
// channel.  CHAN_IN_USE is a channel that is presently being read from.
// CHAN_INVALID marks a channel that would have been deleted except that
// it was "in use"; it should be deleted as soon as it is no longer being used.
//
// Note that the value of the channel status is managed *externally*,
// and not by the member functions of the class.  ChannelState() may
// be used to access and modify the internal state variable.
//
// The state is initialized to CHAN_VALID.

typedef enum { CHAN_VALID, CHAN_IN_USE, CHAN_INVALID } ChanState;


class Channel {
    public:
	// Create a new Channel with the given input and output fd's.
	Channel( int rfd, int wfd )
		{
		read_fd = rfd;
		write_fd = wfd;
		state = CHAN_VALID;
		}

	// True if data pending in channel read buf.  This is a vestigial
	// remnant from when the Channel class used to buffer its input.
	// It remains here so that if later we find we need to return to
	// buffering, we can do so easily.
	int DataInBuffer()
		{ return 0; }

	// Note we do *not* return a "const ChanState&"; the user is
	// free to modify the channel state.
	ChanState& ChannelState()	{ return state; }

	int ReadFD()	{ return read_fd; }
	int WriteFD()	{ return write_fd; }

    protected:
	ChanState state;
	int read_fd;
	int write_fd;
	};

#endif	/* channel_h */
