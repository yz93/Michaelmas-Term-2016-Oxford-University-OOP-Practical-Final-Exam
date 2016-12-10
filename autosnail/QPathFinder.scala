// QPathFinder.scala
// Copyright (c) 2015 J. M. Spivey

import PathFinder.TownStatus
import QPathFinder._

/** A pathfinder that uses a priority queue */
class QPathFinder(map: RoadMap, from: RoadMap.Town, to: RoadMap.Town,
                  heuristic: Boolean, factory: PriorityQueue.Factory) 
        extends PathFinder(map, from, to, heuristic) {

    val queue: PriorityQueue[Agendum] = factory.makeQueue()
    
    override protected def findMin() = {
        var t: PathFinder.TownData = null;

        while (t == null && ! queue.isEmpty) {
            val a = queue.delMin()
            if (t.status != TownStatus.KNOWN) t = a.town
        }

        t
    }

    override protected def enqueue(d: Float, t: PathFinder.TownData) {
        queue.insert(new Agendum(d, t))
    }
}

object QPathFinder {
    class Agendum(val dist: Float, val town: PathFinder.TownData) 
            extends Ordered[Agendum] {
        def compare(other: Agendum) = dist.compare(other.dist)
    }
}
