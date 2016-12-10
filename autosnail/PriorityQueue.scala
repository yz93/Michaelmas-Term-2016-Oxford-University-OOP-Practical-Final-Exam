// PriorityQueue.scala
// Copyright (c) 2015 J. M. Spivey

/** Interface for priority queues */
trait PriorityQueue[T] {
    /** Insert a new item */
    def insert(x: T)

    /** Delete and return the smallest item */
    def delMin(): T

    /** Test if the queue is empty */
    def isEmpty: Boolean
}

object PriorityQueue {
    trait Factory {
        def makeQueue[T <% Ordered[T]](): PriorityQueue[T]
    }
}
