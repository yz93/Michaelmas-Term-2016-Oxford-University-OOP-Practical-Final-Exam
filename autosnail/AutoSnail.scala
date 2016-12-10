// autosnail/AutoSnail.java
// Copyright (c) 2013 J. M. Spivey

import scala.swing._

/** The main application class. */
class AutoSnail(map: RoadMap) {
    private val frame = new AppFrame(map, this)

    private var pathfinder: PathFinder = null
    private var from = RoadMap.noTown
    private var to = RoadMap.noTown
    var delay = 0

    /** Allow clicks on towns to select them as 'from' or 'to' */
    def select(t: RoadMap.Town) {
        if (from == RoadMap.noTown)
            from = t
        else if (to == RoadMap.noTown && t != from)
            to = t

        frame.refresh(pathfinder)
    }

    /** ready for Search if two towns selected */
    def canSearch = (from != RoadMap.noTown && to != RoadMap.noTown)

    /** read for Reset if any towns selected */
    def canReset = (from != RoadMap.noTown || to != RoadMap.noTown)
    
    def fromTown = from
    def toTown = to
    
    /** If all is ready, carry out a search */
    def search(heur: Boolean) { 
        require(from != RoadMap.noTown || to != RoadMap.noTown)
        pathfinder = new QPathFinder(map, from, to, heur, 
                                     HeapPriorityQueue.factory) {
            override def notifyObservers() {
                // Delay after each display update
                super.notifyObservers()
                if (delay > 0) Thread.sleep(delay)
            }
        }
        pathfinder.addObserver(frame)
        val thread = new Thread(pathfinder)
        thread.start()
    }
    
    /** Reset the selection of towns and the search */
    def reset() { 
        from = RoadMap.noTown; to = RoadMap.noTown
        pathfinder = null
        frame.refresh(pathfinder)
    }
    
    def activate() {
        frame.location = new Point(250, 50)
        frame.pack()
        frame.visible = true
        reset()
    }    
}

object AutoSnail {
    def loadMap(name: String) = {
        val stream = this.getClass.getResourceAsStream(name)
        assert(stream != null)
        RoadMap.fromStream(stream)
    }

    def main(args: Array[String]) {
        val map = loadMap("mapinfo.txt")
        Swing.onEDT {
            val app = new AutoSnail(map)
            app.activate()
        }
    }
}
    
