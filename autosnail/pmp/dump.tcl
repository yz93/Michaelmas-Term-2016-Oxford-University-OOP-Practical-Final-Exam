# /usr/bin/env tclsh -f

proc tally {tag lnum cnt} {
    global count
    set count($tag,$lnum) $cnt
}

source pmp.out

proc getmatch {s x} {
    return [string range $s [lindex $x 0] [lindex $x 1]]
}

proc putmatch {s x t} {
    return [string replace $s [lindex $x 0] [lindex $x 1] $t]
}

set f [lindex $argv 0]
set fp [open $f r]

set re {PMP\.tally\("([^ ]+)", ([0-9]+)\);}

while {[gets $fp line] >= 0} {
    if {[regexp -indices $re $line allx tagx lnumx]} {
        set tag [getmatch $line $tagx]
        set lnum [getmatch $line $lnumx]
        set c 0
        if {[info exists count($tag,$lnum)]} {
            set c $count($tag,$lnum)
        }
        set line [putmatch $line $allx "<<<$c>>>"]
    }
    puts $line
}
