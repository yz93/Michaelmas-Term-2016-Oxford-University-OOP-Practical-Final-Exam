// autosnail/Benchmark.java
// Copyright (c) 2013 J. M. Spivey

object Benchmark {
    def loadMap(name: String) = {
        val stream = this.getClass.getResourceAsStream(name)
        assert(stream != null)
        RoadMap.fromStream(stream)
    }

    def main(args: Array[String]) {
        val map = loadMap("erewhon.txt")
        val from = map.findTown("T4003")
        val to = map.findTown("T1011")

        val cmd = if (args.length >= 1) args(0) else "none"
        val n  = if (args.length >= 2) args(1).toInt else 1 

        if (cmd != "quit") {
            var search: PathFinder = null

            for (i <- 0 until n) {
                cmd match {
                    case "simple" =>
                        search = new QPathFinder(map, from, to, false, 
                                                 SimplePriorityQueue.factory)
                    case "heap" =>
                        search = new QPathFinder(map, from, to, false, 
                                                 HeapPriorityQueue.factory)

                    case "rbt" =>
                        search = new QPathFinder(map, from, to, false, 
                                                 RbtPriorityQueue.factory)

                    case _ =>
                        search =
                            new PathFinder(map, from, to, false)
                }

                search.run()
            }

            System.out.println(search.distance)
        }
    }
}
