# prep.tcl

proc instrument {line tag lnum} {
    set pod "PMP.tally(\"$tag\", $lnum);"

    if {[regsub {[[:<:]]case[[:>:]].*=>} $line "\& $pod" line]} {
        return $line
    }
    
    if {[regexp {=>} $line]} {return $line}

    if {[regexp {[[:<:]]import[[:>:]]} $line]} {return $line}

    if {[regexp {[[:<:]]match \{} $line]} {return $line}

    if {[regexp {[[:<:]]reactions \+= \{} $line]} {return $line}

    if {[regsub "\{" $line "\{ $pod" line]} {return $line}

    return $line
}

proc process {fin tag} {
    set lnum 0
    while {[gets $fin line] >= 0} {
        puts [instrument $line $tag [incr lnum]]
    }
}

set f [lindex $argv 0]
set g [file rootname $f]
set fin [open $f r]
process $fin $g
close $fin
    
