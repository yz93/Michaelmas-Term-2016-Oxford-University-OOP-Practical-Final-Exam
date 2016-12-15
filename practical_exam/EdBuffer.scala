// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}
import Undoable.Change

/** The state of an editing session */
class EdBuffer {
    /** The text being edited. */
    private val text = new PlaneText()

    /** The display. */
    private var display: Display = null
    
    // State components that are preserver by undo and redo

    /** Current editing position. */
    private var _point = 0

    /** Current mark position. */
    private var _mark = 0
    
    /** Current buffer editing state. */
    private var _buf_state: Long = 0
    
    /** Indicates whether the current buffer
     *  contents have been saved before the
     *  most recent action. */
    private var previous_saving_state: Boolean = false

    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    /** Current file editing state. */
    private var _file_state: Long = 0

    /** Register a display */
    def register(display: Display) { this.display = display }

    /** Mark the buffer as modified */
    private def setModified() {
        previous_saving_state = _buf_state == _file_state
        if (_buf_state >= _file_state)
            _buf_state += 1
        else
            _buf_state = _file_state + 1
    }

    /** Test whether the text is modified */
    def isModified = _buf_state != _file_state
    

    // Display update
    
    /** Extent that the display is out of date. */
    private var damage = EdBuffer.CLEAN
    
    /** If damage = REWRITE_LINE, the line that should be rewritten */
    private var damage_line = 0

    /** Note damage to the display. */
    private def noteDamage(rewrite: Boolean) {
        val newdamage = 
            if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
        damage = Math.max(damage, newdamage)
        damage_line = text.getRow(point)
    }
    
    /** Force a display rewrite */
    def forceRewrite() { noteDamage(true) }

    /** Update display with cursor at point */
    def update() { update(point) }

    /** Update display with cursor at arbitrary position */
    def update(pos: Int) {
        display.refresh(damage, text.getRow(pos), text.getColumn(pos))
        damage = EdBuffer.CLEAN
    }
    
    /** Initialise display */
    def initDisplay() {
        noteDamage(true)
        update()
    }


    // Accessors

    def point = _point

    def mark = _mark

    def point_=(point: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
            damage = EdBuffer.REWRITE
        _point = point
    }

    def mark_=(mark: Int) {
        _mark = mark
    }

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }


    // Delegate methods for text
    
    def charAt(pos: Int) = text.charAt(pos)

    def getRow(pos: Int) = text.getRow(pos)

    def getColumn(pos: Int) = text.getColumn(pos)
    
    def getPos(row: Int, col: Int) = text.getPos(row, col)

    def length = text.length

    def getLineLength(row: Int) = text.getLineLength(row)

    def getRange(pos: Int, len: Int) = text.getRange(pos, len)

    def numLines = text.numLines

    def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

    def writeFile(out: Writer) { text.writeFile(out) }


    // Mutator methods

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.deleteChar(pos)
        // if deleting a character on the
        // LEFT of the mark, decrement the mark by one,
        // for which the strict inequality is necessary
        if (mark > pos) mark -= 1
        setModified()
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
        noteDamage(true)
        text.deleteRange(pos, len)
        if (mark > pos){
          // If the character that mark currently points
          // to is deleted, then set mark to the starting 
          // position of the deleting range
          if (pos+len-1 >= mark) mark = pos
          // else, simply decrement mark by length of
          // range because those many characters to the 
          // LEFT of the mark have been deleted
          else mark -= len
        }
        setModified()
    }
    
    /** Insert a character */
    def insert(pos: Int, ch: Char) {
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.insert(pos, ch)
        // if inserting a single character on the
        // LEFT of the mark, increment the mark by one.
        // Note that the >= condition is necessary
        if (mark >= pos) mark += 1
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        noteDamage(true)
        text.insert(pos, s)
        // follows the same logic as in the case of 
        // single character insertion
        if (mark >= pos) mark += s.length
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        noteDamage(true)
        text.insert(pos, s)
        // follows the same logic as in the case of 
        // single character insertion
        if (mark >= pos) mark += s.length
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        noteDamage(true)
        text.insert(pos, t)
        // follows the same logic as in the case of 
        // single character insertion
        if (mark >= pos) mark += t.length
        setModified()
    }

    /** Load a file into the buffer. */
    def loadFile(name: String) {
        filename = name
        text.clear()
        
        try {
            val in = new FileReader(name)
            text.insertFile(0, in)
            in.close()
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't read file '%s'", name)
        }
        
        // Equivalent to the original "modified = false"
        // The only time these two lines actually matter is
        // when Editor.replaceFileCommand is called. Since
        // Undoable.reset clears the undo stack while a new
        // file is being loaded, there is no previous buffer 
        // or file states we need to keep track of. So it is the best
        // to start the timestamp from 0 again.
        _buf_state = 0
        _file_state = 0
        noteDamage(true)
    }
    
    /** Save contents on a file */
    def saveFile(name: String) {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
            //Equivalent to the original "modified = false"
            _file_state = _buf_state
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write '%s'", name)
        }
    }


    /** Make a Memento that records the current editing state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of the current point and mark. */
    class Memento {
        private val pt = point
        private val mk = mark
        private val st = _buf_state
        
        /** Restore the state when the memento was created */
        def restore() { point = pt; mark = mk; _buf_state = st }
    }

    /** Change that records an insertion */
    class Insertion(pos: Int, text: Text.Immutable) extends Change {
        def undo() { deleteRange(pos, text.length) }
        def redo() { insert(pos, text) }
    }

    /** Insertion that can be amalgamated with adjacent, similar changes */
    class AmalgInsertion(val pos: Int, ch: Char) extends Change {
        /** The text inserted by all commands that have merged with this one */
        private val text = new Text(ch)

        def undo() { deleteRange(pos, text.length) }

        def redo() { insert(pos, text) }

        override def amalgamate(change: Change) = {
            change match {
                case other: AmalgInsertion =>
                    if (previous_saving_state 
                        || text.charAt(text.length-1) == '\n'
                            || other.pos != this.pos + this.text.length) 
                        false
                    else {
                        text.insert(text.length, other.text)
                        true
                    }

                case _ => false
            }
        }
    }

    /** Change that records a deletion */
    class Deletion(pos: Int, deleted: Char) extends Change {
        def undo() { insert(pos, deleted) }
        def redo() { deleteChar(pos) }
    }

    def wrapChange(before: Memento, change: Change, after: Memento) = {
        if (change == null)
            null
        else
            new EditorChange(before, change, after)
    }

    /** Wrapper for text changes that preserves other state */
    class EditorChange(before: Memento, 
            private val change: Change,
            private var after: Memento) extends Change {

        def undo() {
            change.undo(); before.restore()
        }
            
        def redo() {
            change.redo(); after.restore()
        }
        
        def amalgamate(other: EditorChange) = {
            if (! change.amalgamate(other.change))
                false
            else {
                after = other.after
                true
            }
        }

        override def amalgamate(other: Change) =
            amalgamate(other.asInstanceOf[EditorChange])
    }
}

object EdBuffer {
    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2
}
