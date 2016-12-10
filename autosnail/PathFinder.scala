// PathFinder.scala
// Copyright (c) 2013 J. M. Spivey

import scala.collection.mutable.HashMap
/** A runnable that can find shortest paths using Dijkstra's algorithm */
class PathFinder(map: RoadMap, from: RoadMap.Town, to: RoadMap.Town,
                 heuristic: Boolean)
        extends Runnable with Observable[PathFinder] {

    import PathFinder._

    /** Table containing dynamic data for each town */
    private val townData = new AttributeTable[RoadMap.Town, TownData]

    /** Table containing dynamic data for each road */
    private val roadData = new AttributeTable[RoadMap.Road, RoadData]

    /** Starting and finishing points for the search */
    val source = townData(from)
    val dest = townData(to)
    
    /** Whether the search is completed */
    private var done = false
    
    /** Distance of latest town visited */
    private var dist = 0.0f
    
    def isDone = done
    def distance = dist
    
    protected def findMin() = {
        var d = INFINITY
        var t: TownData = null

        for (x <- map.towns) {
            val u = townData(x)
            if (u.status != TownStatus.KNOWN && u.dist < d) {
                t = u; d = u.dist
            }
        }

        t
    }

    protected def enqueue(d: Float, t: TownData) { 
        // Nothing to do here
    }

    def run() {
        source.status = TownStatus.KNOWN
        source.dist = 0.0f
        dist = 0.0f
        visitNeighbours(source)
        notifyObservers()
        
        while (dest.status != TownStatus.KNOWN) {
            val t = findMin()

            if (t == null) {
                // Destination is unreachable
                dist = INFINITY
                done = true
                notifyObservers()
                return
            }

            dist = t.dist
            if (heuristic) dist += penalty(source) - penalty(t)

            t.status = TownStatus.KNOWN
            t.link.status = RoadStatus.TREE
            visitNeighbours(t)
            notifyObservers()
        }
        
        highlightPath()
        done = true
        notifyObservers()
    } 
    
    private def visitNeighbours(t: TownData) {
        for (a <- t.town.neighbours) {
            val u = townData(a.head)
            var d = t.dist + a.road.length
            if (heuristic) d -= penalty(t) - penalty(u)
            if (u.status != TownStatus.KNOWN && d < u.dist) {
                if (u.status == TownStatus.ACTIVE) 
                    u.link.status = RoadStatus.DEAD
                
                var r = roadData(a.road)
                u.status = TownStatus.ACTIVE; u.dist = d; u.link = r
                r.status = RoadStatus.ACTIVE; r.prev = t
                enqueue(d, u)
            }
        }
    }

    private def penalty(t: TownData) = t.town.distance(dest.town)
            
    private def highlightPath() {
        var t = dest
        while (t != source) {
            val r = t.link
            r.status = RoadStatus.PATH
            t = r.prev
        }
    }

    def townStatus(t0: RoadMap.Town) = {
        townData.get(t0) match {
            case Some(t) => t.status
            case None => TownStatus.UNSEEN
        }
    }

    def roadStatus(r0: RoadMap.Road) = {
        roadData.get(r0) match {
            case Some(r) => r.status
            case None => RoadStatus.UNSEEN
        }
    }
}

object PathFinder {
    val INFINITY = 1.0e6f
    
    /** Status for a town */
    object TownStatus extends Enumeration {
        val UNSEEN, ACTIVE, KNOWN = Value
    }
    
    class TownData(val town: RoadMap.Town) {
        var dist = INFINITY
        var status = TownStatus.UNSEEN
        var link: RoadData = null
    }

    implicit val townFactory: RoadMap.Town => TownData = (new TownData(_))

    /** Status for a road */
    object RoadStatus extends Enumeration {
        val UNSEEN, ACTIVE, DEAD, TREE, PATH = Value
    }

    class RoadData(road: RoadMap.Road) {
        var status = RoadStatus.UNSEEN
        var prev: TownData = null
    }

    implicit val roadFactory: RoadMap.Road => RoadData = (new RoadData(_))

    class AttributeTable[K, V](implicit factory: K => V) 
            extends HashMap[K, V] {
        override def apply(k: K): V = getOrElseUpdate(k, factory(k))
    }
}
