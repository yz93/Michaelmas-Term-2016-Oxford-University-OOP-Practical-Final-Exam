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
    
    /** The binary search tree that records the interval changes*/
    private var intervalTree: IntervalBST = new IntervalBST(null)

    // State components that are preserver by undo and redo

    /** Current editing position. */
    private var _point = 0

    /** Current mark position. */
    private var _mark = 0

    /** Current buffer editing state. */
    private var _buf_state: Long = 0

    /** Indicates whether the buffer
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

    /** Returns a 2-tuple of (Int, Int) with the first element being
     *  the starting position of the word and the second element being 
     *  the length of the word.
     *  Precondition: The character at the point is a letter or digit, 
     *  which is checked by Editor.toUpperCommand() before invocation 
     *  of this method.
     */
    def getWordPosAndLen: Tuple2[Int, Int] = {
        // search backward for the start of the word
        var p_1 = _point
        var ch = text.charAt(_point)
        while(p_1 > 0 && ch.isLetterOrDigit){
            p_1 -= 1
            ch = text.charAt(p_1)
        }
        // make sure p_1 points to the first character of the word
        // if p_1 == 0 and ch is a letter or digit, do NOT increment
        // otherwise, increment by 1, including if p_1 ==0 and ch
        // is not a letter or digit
        if (p_1 > 0 || !ch.isLetterOrDigit) p_1 += 1
        // search forward for the end of the word
        var p_2 = _point
        ch = text.charAt(_point)
        while(p_2 < (text.length-1) && ch.isLetterOrDigit){
            p_2 += 1
            ch = text.charAt(p_2)
        }
        // make sure p_2 points to the last character of the word
        // special case is when p_2 is the last character of the text
        // similar in nature to the case for p_1
        if (p_2 < (text.length-1) || !ch.isLetterOrDigit) p_2 -= 1
        (p_1, p_2-p_1+1)
    }


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
        // update the positions of the encrypted blocks
        intervalTree.update_intervals(pos, -1)
        setModified()
    }

    /** Delete a range of characters. 
     *  Assumption: pos and len indicates valid intervals for deletion,
     *  including not overlapping with encrypted blocks
     */
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
        // update the positions of the encrypted blocks
        intervalTree.update_intervals(pos, -len)
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
        // update the positions of the encrypted blocks
        intervalTree.update_intervals(pos, 1)
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
        noteDamage(true)
        text.insert(pos, s)
        // follows the same logic as in the case of 
        // single character insertion
        if (mark >= pos) mark += s.length
        // update the positions of the encrypted blocks
        intervalTree.update_intervals(pos, s.length)
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
        noteDamage(true)
        text.insert(pos, s)
        // follows the same logic as in the case of 
        // single character insertion
        if (mark >= pos) mark += s.length
        // update the positions of the encrypted blocks
        intervalTree.update_intervals(pos, s.length)
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
        noteDamage(true)
        text.insert(pos, t)
        // follows the same logic as in the case of 
        // single character insertion
        if (mark >= pos) mark += t.length
        // update the positions of the encrypted blocks
        intervalTree.update_intervals(pos, t.length)
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

    /** Converts the text between first and last inclusively using ROT-13.
     *  Pre-assumption: Editor has checked that the interval is valid for
     *  conversion. This method treats each character as an integer and
     *  adds or deletes 13 depending on whether the char is in the 
     *  first half of the alphabet or the second half.
     */
    private def doROT13(first: Int, last: Int) {
        // saves mark's position
        val m = mark
        val len = last - first + 1
        var sb: StringBuilder = new StringBuilder(len)
        for (i <- first to last) {
            var ch = text.charAt(i);
            if       (ch >= 'a' && ch <= 'm') sb += (ch.toInt + 13).toChar
            else if  (ch >= 'A' && ch <= 'M') sb += (ch.toInt + 13).toChar
            else if  (ch >= 'n' && ch <= 'z') sb += (ch.toInt - 13).toChar
            else if  (ch >= 'N' && ch <= 'Z') sb += (ch.toInt - 13).toChar
            else sb += ch
        }
        // first insert the transformed text, then delete the original text
        insert(first, sb.mkString)
        deleteRange(first+len, len)
        // because insert and deleteRange changes mark's position, restore its value
        mark = m
    }

    /** Encrypts the text between first and last inclusively */
    def cipher(first: Int, last: Int) {
        doROT13(first, last)
        intervalTree.insert(first, last)
    }

    /** Decrypts the text between first and last inclusively */
    def decipher(first: Int, last: Int) {
        doROT13(first, last)
        intervalTree.delete(first, last)
    }

    /** A wrapper function that delegates work to intervalTree.intersectAny */
    def intersectAny(key: Int, value: Int, normal_pos: Boolean, for_insert_test: Boolean)
        : Tuple2[Tuple2[Int, Int], Boolean] = {
        intervalTree.intersectAny(key, value, normal_pos, for_insert_test)
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

    /** Change that records a word's conversion to uppercase*/
    class UppercaseConversion(val pos: Int, txt: Text.Immutable, txt_upper: String) extends Change{
        def undo() {
            deleteRange(pos, txt_upper.length)
            val temp = _point
            _point = pos
            insert(pos, txt)
            _point = temp
        }
        def redo() {
            deleteRange(pos, txt.length)
            val temp = _point
            _point = pos
            insert(pos, txt_upper)
            _point = temp
        }
        override def amalgamate(change: Change) = {
            change match {
                case other: UppercaseConversion => other.pos == this.pos
                case _ => false
            }
        }
    }

    /** Change that records a ROT-13 transformation */
    class ROT13Conversion(start: Int, end: Int, ciphered: Boolean) extends Change{
        def undo(){ if (ciphered) decipher(start, end) else cipher(start, end) }
        def redo(){ if (ciphered) cipher(start, end) else decipher(start, end) }
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
