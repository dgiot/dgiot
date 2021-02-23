# Parse Erlang Crash Dumps and correlate mailbox size to the currently running
# function.
#
# Once in the procs section of the dump, all processes are displayed with
# =proc:<0.M.N> followed by a list of their attributes, which include the
# message queue length and the program counter (what code is currently
# executing).
#
# Run as:
#
#    $ awk -v threshold=$THRESHOLD -f queue_fun.awk $CRASHDUMP
#
# Where $THRESHOLD is the smallest mailbox you want inspects. Default value
# is 1000.
BEGIN {
    if (threshold == "") {
        threshold = 1000 # default mailbox size
    }
    procs = 0 # are we in the =procs entries?
    print "MESSAGE QUEUE LENGTH: CURRENT FUNCTION"
    print "======================================"
}

# Only bother with the =proc: entries. Anything else is useless.
procs == 0 && /^=proc/ { procs = 1 } # entering the =procs entries
procs == 1 && /^=/ && !/^=proc/ { exit 0 } # we're done


# Message queue length: 1210
# 1       2     3       4
/^Message queue length: / && $4 >= threshold { flag=1; ct=$4 }
/^Message queue length: / && $4 <  threshold { flag=0 }

# Program counter: 0x00007f5fb8cb2238 (io:wait_io_mon_reply/2 + 56)
# 1       2        3                  4                       5 6
flag == 1 && /^Program counter: / { print ct ":", substr($4,2) }
