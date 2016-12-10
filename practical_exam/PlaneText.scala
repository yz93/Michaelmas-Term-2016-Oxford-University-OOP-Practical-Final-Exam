// PlaneText.scala
// Copyright (c) 2015 J. M. Spivey

/** An extension of Text that keeps track of the division of the 
 * text into lines. */
class PlaneText(init: Int) extends Text() {
    /* For present purposes, we count each newline character as part of the
     * line it terminates, so that every line has non-zero length.  Let's
     * also imagine that a special terminator character is added to the end of
     * the text, so that the very last line also has non-zero length.  For an
     * ordinary text file that ends in a newline, this last line will be empty
     * and be counted as having length 1, and the editor will count the
     * file as having one more line than there are newline characters. */
    private var nlines = 1
    private var linelen = new Array[Int](init)
    linelen(0) = 1
    
    /* For efficiency, we keep track of the beginning of the most recent line
     * that was accessed.  This will help a lot if accesses are clustered. 
     * The invariant is linestart = sum linelen[0..curline) */
    private var curline = 0
    private var linestart = 0
    
    def this() { this(1000) }
    
    /** Return the number of lines, including the fictitious last line. */
    def numLines = nlines
    
    /** Return the length of a line in the file */
    def getLineLength(n: Int) = linelen(n)
    
    /** Find the line number corresponding to a character index. */
    def getRow(pos: Int) = {
        findPos(pos); curline
    }
    
    /** Find the column number of a character index in its line. */
    def getColumn(pos: Int) = {
        findPos(pos); pos - linestart
    }
    
    // Augment the mutator methods of Text to maintain the line map
    
    override def clear() {
        super.clear()
        nlines = 1
        linelen(0) = 1
        curline = 0
        linestart = 0
    }
    
    override def insert(pos: Int, ch: Char) {
        super.insert(pos, ch)
        findPos(pos)
        if (ch != '\n')
            linelen(curline) += 1
        else 
            mapLines()
    }

    override def insert(pos: Int, s: String) {
        super.insert(pos, s)
        mapLines()
    }
    
    override def insertRange(pos: Int, t: Text, start: Int, nchars: Int) {
        super.insertRange(pos, t, start, nchars)
        mapLines()
    }

    override def insertFile(pos: Int, in: java.io.Reader) {
        try { 
            super.insertFile(pos, in)
        } finally {
            // Even if an IOException is thrown, we still update the line map
            mapLines()
        }
    }
    
    override def deleteChar(pos: Int) {
        val ch = charAt(pos)
        super.deleteChar(pos)
        findPos(pos)
        if (ch != '\n')
            linelen(curline) -= 1
        else 
            mapLines()
    }

    override def deleteRange(start: Int, len: Int) {
        super.deleteRange(start, len)
        
        findPos(start)
        if (start + len < linestart + linelen(curline))
            linelen(curline) -= len
        else
            mapLines()
    }


    /** Return the editing position closest to the specified coordinates */
    def getPos(row: Int, col: Int) = {
        val r = Math.min(Math.max(row, 0), nlines-1)
        findLine(r)
        val c = Math.min(Math.max(col, 0), linelen(curline)-1)
        linestart + c
    }
    
    /** Fetch the text of line n, without the trailing newline */
    def fetchLine(n: Int, buf: Text) {
        findLine(n)
        getRange(linestart, linelen(n)-1, buf)
    }
    
    /** Refresh the line map by scanning the whole file.
     * This is always a last resort if we choose not to update the
     * line map in a faster way. */
    private def mapLines() {
        nlines = 0
        var c = 0

        for (i <- 0 until length) {
            c += 1
            if (charAt(i) == '\n') {
                lineRoom()
                linelen(nlines) = c
                nlines += 1; c = 0
            } 
        }
        
        lineRoom()
        linelen(nlines) = c+1
        nlines += 1
        
        // Reset the cache
        curline = 0; linestart = 0
    }
    
    /** Set curline to a specified line number. */
    private def findLine(n: Int) {
        assert(n >= 0 && n < nlines)
        
        // Move forwards if necessary
        while (n > curline) {
            linestart += linelen(curline)
            curline += 1
        }

        // Move backwards if necessary
        while (n < curline) {
            curline -= 1
            linestart -= linelen(curline)
        }
    }
    
    /** Set current line so that it contains a specified character index. */
    private def findPos(j: Int) {
        assert(j >= 0 && j <= length)

        // Move forwards if necessary
        while (j >= linestart + linelen(curline)) {
            linestart += linelen(curline)
            curline += 1
        }
        
        // Move backwards if necessary
        while (j < linestart) {
            curline -= 1
            linestart -= linelen(curline)
        }
        
        assert(linestart <= j && j < linestart + linelen(curline))
    }
    
    /** Find room for one more line */
    private def lineRoom() {
        if (nlines >= linelen.length) {
            val newlen = new Array[Int](2*linelen.length)
            Array.copy(linelen, 0, newlen, 0, nlines)
            linelen = newlen
        }
    }
}
