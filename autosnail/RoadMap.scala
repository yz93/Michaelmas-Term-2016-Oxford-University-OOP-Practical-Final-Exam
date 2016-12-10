// RoadMap.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.{ArrayBuffer, HashMap}

/** A static map */
class RoadMap {
    import RoadMap._

    private val townDict = new HashMap[String, Town]
    private val _towns = new ArrayBuffer[Town]
    private val _roads = new ArrayBuffer[Road]
    private val _coastlines = new ArrayBuffer[List[Coords]]
    
    def addTown(name: String, location: Coords) {
        require(! townDict.contains(name))
        val town = new Town(name, location)
        _towns.append(town)
        townDict.put(name, town)
    }
    
    def addRoad(name1: String, name2: String, length: Float) {
        // Create a Road object and two Arc objects
        val t1 = townDict(name1);
        val t2 = townDict(name2);
        val road = new Road(t1, t2, length);
        _roads.append(road);
        t1.addArc(t2, road);
        t2.addArc(t1, road);
    }
    
    def addCoastline(outline: List[Coords]) {
        _coastlines.append(outline)
    }
    
    def findTown(name: String) = townDict(name)

    def towns = _towns.toIterator
    def roads = _roads.toIterator
    def coastlines() = _coastlines.toIterator
}

object RoadMap {    
    class Town(val name: String, val location: Coords) {
        private val arcs = new ArrayBuffer[Arrow]
        
        private[RoadMap] def addArc(dest: Town, road: Road) { 
            arcs.append(new Arrow(dest, road))
        }
        
        def neighbours = arcs.toIterator

        def distance(other: Town) = location.distance(other.location)
    }
    
    val noTown = new Town("", new Coords(0.0f, 0.0f))

    class Arrow(val head: Town, val road: Road)
    
    class Road(val town1: Town, val town2: Town, val length: Float)
    
    class Coords(val x: Float, val y: Float) {
        def distance(other: Coords) = {
            val dx = this.x - other.x
            val dy = this.y - other.y
            Math.sqrt(dx*dx + dy*dy).toFloat
        }
    }
    
    import java.io._

    /** Construct a RoadMap from its textual form */
    def fromStream(stream: InputStream) = {
        val map = new RoadMap
        
        // Parameters to scale and translate the outline data
        var scale = 0.0f
        var offx = 0.0f
        var offy = 0.0f
        
        val outline = new ArrayBuffer[Coords]
        
        val in = new BufferedReader(new InputStreamReader(stream))
        
        var line = in.readLine()
        var lnum = 1
        while (line != null) {
            val trimmed = line.trim

            if (trimmed.length > 0 && trimmed.charAt(0) != '#') {
                val fields = trimmed.split("/")
            
                val cmd = fields(0)
                if (cmd == "town" && fields.length == 4)
                    map.addTown(fields(1), 
                        new Coords(fields(2).toFloat, fields(3).toFloat))
                else if (cmd == "road" && fields.length == 4)
                    map.addRoad(fields(1), fields(2), fields(3).toFloat)
                else if (cmd == "mapscale" && fields.length == 4) {
                    scale = fields(1).toFloat;
                    offx = fields(2).toFloat;
                    offy = fields(3).toFloat;
                } else if (cmd == "point" && fields.length == 3) {
                    val x = fields(1).toFloat
                    val y = fields(2).toFloat
                    outline.append(new Coords(scale * x + offx, 
                                              -scale * y + offy))
                } else if (cmd == "makeoutline") {
                    // Wrap up the current outline and begin a new one
                    map.addCoastline(outline.toList)
                    outline.clear()
                } else {
                    // Oops.  Don't even try to be graceful!
                    throw new Error("Map garbled on line " + lnum)
                }
            }

            line = in.readLine(); lnum += 1
        }
        
        in.close();
        map;
    }
}
