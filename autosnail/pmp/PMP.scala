// PMP.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.HashMap
import java.io.{PrintWriter, FileWriter}

object PMP {
    class Counter { var n = 0 }

    val tallies = new HashMap[(String, Int), Counter]

    def tally(tag: String, lnum: Int) { 
        val ctr = tallies.getOrElseUpdate((tag, lnum), new Counter)
        ctr.n += 1
    }

    def dump() {
        val out = new PrintWriter(new FileWriter("pmp.out"))
        tallies.foreach(p =>
            p match { 
                case ((tag, lnum), ctr) =>
                    out.println("tally %s %d %d".format(tag, lnum, ctr.n)) })
        out.close
    }

    val runtime = Runtime.getRuntime()
    runtime.addShutdownHook(new Thread { override def run() { dump() } })
}
