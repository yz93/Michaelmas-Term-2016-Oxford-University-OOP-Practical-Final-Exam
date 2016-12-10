// Keymap.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.Map

/** A mapping from keystrokes to commands. */
class Keymap[Command] {
    /** A Map that represents the mapping from keys to commands. */
    private val table = Map[Int, Command]()
    
    /** Add an association to the mapping. */
    def +=(pair: (Int, Command)) { table += pair }
    
    /** Find the command for a specified key, or return null. */
    def find(key: Int): Option[Command] = {
        table.get(key)
    }
}

object Keymap {
    def apply[Command](pairs: (Int, Command)*) = {
        val keymap = new Keymap[Command]
        for (pair <- pairs) keymap += pair
        keymap
    }
}
