// Text.scala
// Copyright (c) 2015 J. M. Spivey

/** A sequence of characters that allows (efficient) insertion and deletion
 * in the middle. */
class Text(init: Int) extends CharSequence {
    /* A Text object represents the sequence of characters
     * buffer[0..gap) ++ buffer[max-len+gap..max) */
    
    /** The gap buffer */
    private var buffer = new Array[Char](init)
    
    /** Length of the represented text */
    private var len = 0

    /** Position of the gap */
    private var gap = 0

    /** Size of the buffer, defined for convenience */
    private def max = buffer.length
    
    /** Construct a Text with a default initial capacity */
    def this() { this(1000) }
    
    /** Construct a Text containing a single character */
    def this(ch: Char) { this(4); insert(0, ch) }

    /** Construct a Text containing a specified string */
    def this(s: String) { this(Math.max(s.length, 4)); insert(0, s) }

    /** Return the length of the text */
    def length = len

    /** Return the character at a specified position */
    def charAt(pos: Int) = {
        assert(0 <= pos && pos < len)

        if (pos < gap)
            buffer(pos)
        else
            buffer(max-len+pos)
    }
    
    // Mutators: any changes or additions here require similar changes to
    // the subclass PlaneText (the fragile base class problem).
    
    /** Make the text empty. */
    def clear() {
        gap = 0; len = 0
    }

    /** Insert a single character. */
    def insert(pos: Int, ch: Char) {
        assert(0 <= pos && pos <= len)
        makeRoom(1); moveGap(pos)
        buffer(gap) = ch
        gap += 1; len += 1
    }
    
    /** Insert a string. */
    def insert(pos: Int, s: String) {
        assert(0 <= pos && pos <= len)
        val n = s.length()
        makeRoom(n); moveGap(pos)
        s.getChars(0, n, buffer, pos)
        gap += n; len += n
    }
    
    /** Insert another text. */
    def insert(pos: Int, t: Text) {
        insertRange(pos, t, 0, t.length)
    }
    
    /** Insert an immutable text */
    def insert(pos: Int, t: Text.Immutable) {
        t.getChars(this, pos)
    }

    /** Insert range [start..start+nchars) from another text */
    def insertRange(pos: Int, t: Text, start: Int, nchars: Int) {
        assert(pos >= 0 && pos <= len && nchars >= 0 && t != this)
        
        makeRoom(nchars); moveGap(pos)
        t.getChars(start, nchars, buffer, pos)
        len += nchars; gap += nchars
    }

    /** Insert the contents of a file. */
    def insertFile(pos: Int, in: java.io.Reader) {
        assert(0 <= pos && pos <= len)
        
        moveGap(pos)
        while (true) {
            makeRoom(4096)
            val nread = in.read(buffer, gap, max-len)
            if (nread < 0) return
            gap += nread; len += nread
        }
    }

    /** Write the entire text on a file. */
    def writeFile(out: java.io.Writer) {
        if (gap > 0) out.write(buffer, 0, gap)
        if (len > gap) out.write(buffer, max-len+gap, len-gap)
    }

    /** Delete a single character. */
    def deleteChar(pos: Int) {
        assert(0 <= pos && pos < len)
        moveGap(pos); len -= 1
    }
    
    /** Delete the last character. */
    def deleteLast() { deleteChar(len-1) }
        
    /** Delete a range of characters. */
    def deleteRange(start: Int, nchars: Int) {
        assert(start >= 0 && nchars >= 0 && start+nchars <= len)
        moveGap(start); len -= nchars
    }

    /** Copy range [start..start+nchars) into a char array arr[pos..) */
    def getChars(start: Int, nchars: Int, arr: Array[Char], pos: Int) {
        /* This is used for display update, so for speed we avoid
           moving the gap. */

        assert(start >= 0 && nchars >= 0 && start+nchars <= len)

        if (start+nchars <= gap)
            // Entirely in the low part
            Array.copy(buffer, start, arr, pos, nchars)
        else if (start >= gap)
            // Entirely in the high part
            Array.copy(buffer, max-len+start, arr, pos, nchars)
        else {
            val k = gap-start
            Array.copy(buffer, start, arr, pos, k)
            Array.copy(buffer, max-len+gap, arr, pos+k, nchars-k)
        }
    }

    /** Fetch the range [start..start+nchars) as an immutable text */
    def getRange(start: Int, nchars: Int) =
        new Text.Immutable(getString(start, nchars))
    
    /** Fetch a range into another text */
    def getRange(start: Int, nchars: Int, buf: Text) {
        buf.clear()
        buf.insertRange(0, this, start, nchars)
    }

    private def getString(start: Int, nchars: Int) = {
        assert(start >= 0 && nchars >= 0 && start + nchars <= len)
        if (gap < start+nchars) moveGap(start+nchars)
        new String(buffer, start, nchars)
    }
    
    /** Return the contents of the text as a String.  Be careful when using
     * this for debugging: it has the (benign) side-effect of moving the gap
     * to the end. */
    override def toString() = getString(0, len)
   
    /** Select a range [start..end).  Required for CharSequence but unused. */
    def subSequence(start: Int, end: Int): CharSequence = {
        if (start < 0 || end < start || len < end)
            throw new IndexOutOfBoundsException()
        return getString(start, end-start)
    }
    
    /** Establish gap = pos by moving characters around */
    private def moveGap(pos: Int) {
        assert(0 <= pos && pos <= len)

        if (gap < pos)
            // buffer[gap..pos) := buffer[max+gap-len..max+pos-len)
            Array.copy(buffer, max-len+gap, buffer, gap, pos-gap)
        else if (gap > pos)
            // buffer[max+pos-len..max+gap-len) := buffer[pos..gap)
            Array.copy(buffer, pos, buffer, max-len+pos, gap-pos)

        gap = pos
    }
    
    /** Ensure that there is space for n more characters. */
    private def makeRoom(n: Int) {
        assert(n >= 0)
        
        if (max-len >= n) return

        val newmax = Math.max(2*max, len+n)
        val newbuf = new Array[Char](newmax)
        if (gap > 0)
            Array.copy(buffer, 0, newbuf, 0, gap)
        if (gap < len)
            Array.copy(buffer, max-len+gap, 
                       newbuf, newmax-len+gap, len-gap)
        buffer = newbuf
    }
}

object Text {
    class Immutable private[Text] (private val contents: String) 
            extends CharSequence {
        // The implementation here is trivial: just a Java string.
        
        def charAt(index: Int) = contents.charAt(index)

        def length = contents.length

        def getChars(text: Text, pos: Int) { text.insert(pos, contents) }

        def subSequence(start: Int, end: Int) =
            contents.subSequence(start, end)

        override def equals(that: Any) = {
            that match {
                case other: Immutable => contents.equals(other.contents)
                case _ => false
            }
        }

        override def hashCode = contents.hashCode
    }
}
