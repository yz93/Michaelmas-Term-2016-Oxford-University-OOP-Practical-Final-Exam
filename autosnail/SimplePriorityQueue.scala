// SimplePriorityQueue.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.ArrayBuffer

class SimplePriorityQueue[T <% Ordered[T]] extends PriorityQueue[T] {
    private var elems = new ArrayBuffer[T]

    def isEmpty = (elems.size == 0)

    def insert(x: T) {
        elems.append(x)
    }
    
    def delMin() = { 
        require(elems.size > 0)

        var i = 1
        var j = 0
        var r = elems(0)

        while (i < elems.size) {
            if (elems(i) < r) {
                r = elems(i); j = i
            }
            i += 1
        }

        elems(j) = elems.last 
        elems.trimEnd(1)        
        r
    }
}

object SimplePriorityQueue {
    val factory = new PriorityQueue.Factory() {
        def makeQueue[T <% Ordered[T]]() = new SimplePriorityQueue[T]
    }
}    
