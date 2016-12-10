// Undoable.scala
// Copyright (c) 2015 J. M. Spivey

import scala.collection.mutable.ArrayBuffer

/** An mixin that can record a history of undoable actions. */
trait Undoable[Action] {
    /** A stack of undo changes from executed actions. */
    private val history = new ArrayBuffer[Undoable.Change]

    /** Index into undo stack.  Elements history[0..u) have been executed
            but not undone, and elements history[u..) have been undone. */
    private var undoPointer = 0

    /** Do the work of an action, returning an undo change. */
    def obey(action: Action): Undoable.Change // abstract
    
    /** Beep on error. */
    def beep() // abstract

    /** Execute an action, recording undo info. */
    def perform(action: Action) {
        val change = obey(action)
        if (change != null) {
            history.reduceToSize(undoPointer)

            if (history.size > 0) {
                val prev = history.last
                if (prev.amalgamate(change)) return
            }

            history.append(change); undoPointer += 1
        }
    }

    /** Undo the latest command. */
    def undo() { 
        if (undoPointer == 0) { beep(); return }
        undoPointer -= 1
        val change = history(undoPointer)
        change.undo()
    }

    /** Redo the next command. */
    def redo() {
        if (undoPointer == history.size) { beep(); return }
        val change = history(undoPointer)
        undoPointer += 1
        change.redo()
    }

    /** Reset the history, e.g. after loading a new file */
    def reset() {
        history.clear(); undoPointer = 0
    }
}

object Undoable {
    /** An element of the undo history. */
    abstract class Change {
        /** Reset the subject to its previous state. */
        def undo() // abstract

        /** Reset the subject to the state after the change. */
        def redo() // abstract

        /** Try to amalgamate this change with another. */
        def amalgamate(other: Change) = false
    }
}
