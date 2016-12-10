// HeapPriorityQueue.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.ArrayBuffer

class HeapPriorityQueue[T <% Ordered[T]] extends PriorityQueue[T] {
    private var elems = new ArrayBuffer[T]

    /* We store the children of elems(i) in elems(2*i+1) and elems(2*i+2). */

    def isEmpty = (elems.size == 0)

    def insert(x: T) {
        // Start at a leaf and sift towards the root
        var i = elems.size
        elems.append(x)
        var j = (i-1)/2
        while (i > 0 && x < elems(j)) {
            elems(i) = elems(j)
            i = j; j = (i-1)/2
        }
        elems(i) = x
    }
    
    def delMin() = { 
        require(elems.size > 0)

        val result = elems(0)
        val x = elems.last
        elems.trimEnd(1)

        if (elems.size > 0) {
            // Start at the root and sift towards the leaves
            var i = 0
            var j = 1
            var done = false
            while (j < elems.size && !done) {
                // Find smaller child
                if (j+1 < elems.size && elems(j) > elems(j+1)) j += 1

                if (x <= elems(j))
                    done = true
                else {
                    elems(i) = elems(j)
                    i = j; j = 2*i+1
                }
            }
            elems(i) = x
        }

        result
    }
}

object HeapPriorityQueue {
    val factory = new PriorityQueue.Factory() {
        def makeQueue[T <% Ordered[T]]() = new HeapPriorityQueue[T]
    }
}
