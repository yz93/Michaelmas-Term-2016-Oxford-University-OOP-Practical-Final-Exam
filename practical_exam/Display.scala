// Display.scala
// Copyright (c) 2015 J. M. Spivey

/** A view that keeps a terminal up to date with respect to a text. */
class Display(terminal: Display.Hardware) {
    /** Number of lines in the main display */
    private val LINES = Display.HEIGHT - 1

    /** Line for showing the minibuffer */
    private val MINIROW = Display.HEIGHT - 1

    /** The editor that is shown on the display. */
    private var ed: EdBuffer = null
    
    /** The minibuffer being displayed, or null if none */
    private var minibuf: MiniBuffer = null
    
    /** The top line shown on the terminal */
    private var origin = 0
    
    /** Top line from the previous time the display was refreshed */
    private var oldOrigin = 0
    
    /** A message to show on the last line */
    private var message: String = null
    
    /** Current editing position */
    private var row = 0
    private var col = 0
    
    def show(ed: EdBuffer) { this.ed = ed }

    // Delegates for keyboard methods

    /** If >= 0, a keystroke that has been pushed back 
     * to be read again later. */
    private var pbkey = -1

    /** Get a keystroke */
    def getKey() = { 
        if (pbkey >= 0) {
            val key = pbkey; pbkey = -1; key
        } else {
            terminal.getKey()
        }
    }
    
    /** Push back a keystroke to be read again later. */
    def pushBack(key: Int) { assert(pbkey == -1); pbkey = key }

    /** Flush type-ahead */
    def flush() { terminal.flush(); pbkey = -1 }
    
    /** Just beep */
    def beep() { terminal.beep() }

    // These routines rewrite parts of the display, but leave the cursor
    // where they please
    
    /** Scratch buffer for use by rewrite */
    private val line = new Text(Display.WIDTH)
    
    /** Rewrite the entire screen */
    private def rewrite() {
        terminal.clear()
        for (r <- 0 until Math.min(LINES, ed.numLines - origin)) {
            ed.fetchLine(origin+r, line)
            if (line.length > 0) {
                terminal.gotoRC(r, 0)
                terminal.write(line)
            }
        }
    }
    
    /** Rewrite just the line containing the cursor */
    private def rewriteLine() {
        terminal.gotoRC(row-origin, 0)
        terminal.clearLine()
        ed.fetchLine(row, line)
        terminal.write(line)
    }
    
    /** Rewrite just the minibuffer line */
    private def rewriteMinibuf() {
        terminal.gotoRC(MINIROW, 0)
        terminal.clearLine()

        if (minibuf != null) {
            terminal.setRevVideo(true)
            terminal.write(minibuf.prompt)
            terminal.write(':')
            terminal.setRevVideo(false)
            terminal.write(' ')
            minibuf.getText(line)
            terminal.write(line)
        } else {
            val mflag = if (ed.isModified) "*" else ""
            terminal.setRevVideo(true)
            terminal.write("--- EWOKS: %s%s ---".format(ed.filename, mflag))
            terminal.setRevVideo(false)
            if (message != null) {
                terminal.write(' ')
                terminal.write(message)
            }
        }
        
    }

    /** Move the cursor to the correct place */
    private def moveCursor() {
        if (minibuf != null && minibuf.pos >= 0)
            terminal.gotoRC(MINIROW, 
                minibuf.prompt.length + minibuf.pos + 2)
        else
            terminal.gotoRC(row-origin, Math.min(col, Display.WIDTH-1))
    }
    
    /** Update the display */
    def refresh(damage: Int, r: Int, c: Int) {
        var dmg = damage
        row = r; col = c
        checkScroll()
        if (origin != oldOrigin)
            dmg = EdBuffer.REWRITE

        dmg match {
            case EdBuffer.REWRITE => rewrite()
            case EdBuffer.REWRITE_LINE => rewriteLine()
            case EdBuffer.CLEAN => ()
            case _ => throw new Error("Display.refresh")
        }
        
        rewriteMinibuf()
        moveCursor()
        oldOrigin = origin
    }
    
    /** Update the minibuffer line */
    def refreshMinibuf() {
        rewriteMinibuf()
        moveCursor()
    }

    /** Post or (with null) remove a minibuffer */
    def setMiniBuf(minibuf: MiniBuffer) {
        this.minibuf = minibuf
        refreshMinibuf()
    }
    
    /** Post or (with null) remove a message. */
    def setMessage(message: String) {
        this.message = message
        refreshMinibuf()
    }

    /* These routines implement the scrolling policy.  External calls of
     * chooseOrigin and scroll may set an origin that does not obey
     * the scrolling policy; this will be corrected by checkScroll next
     * time the display is refreshed. */
    
    /** Check that the origin obeys the rules, and move it if not */
    private def checkScroll() {
        if (row < origin || row >= origin + LINES)
            chooseOrigin()

        /* Ensure that the origin is within the buffer, if possible by
         * half a screen at the end */
        origin = Math.max(Math.min(origin, ed.numLines - LINES/2), 0)
    }

    /** Choose display origin to centre the cursor */
    def chooseOrigin() {
        // This is used for Redraw
        origin = row - LINES/2
    }
    
    /** Suggest scrolling by a specified amount */
    def scroll(n: Int) {
        // This is used by PageUp and PageDown
        origin += n
    }
}

object Display {
    /** Dimensions of the display (fixed for now) */
    val HEIGHT = 24
    val WIDTH = 80

    /** Codes returned by the keyboard */
    val UP = 513
    val DOWN = 514
    val RIGHT = 515
    val LEFT = 516
    val HOME = 517
    val END = 518
    val PAGEUP = 519
    val PAGEDOWN = 520
    val INS = 521
    val DEL = 522
    val F1 = 523
    val F2 = 524
    val F3 = 525
    val F4 = 526
    val F5 = 527
    val F6 = 528
    val F7 = 529
    val F8 = 530
    val F9 = 531
    val F10 = 532
    val F11 = 533
    val F12 = 534
    val CTRLHOME = 535
    val CTRLEND = 536
    val TAB = 537
    val RETURN = 538
    val UNDEFINED = -1

    def ctrl(ch: Int): Int = ch ^ 0x40

    val printable = 32 until 127

    /** Interface for display hardware */
    trait Hardware {
        /** Wait for for a keystroke and return it. */
        def getKey(): Int

        /** Discard any pending input */
        def flush(): Unit

        /** Clear the whole screen */
        def clear(): Unit

        /** Clear from the cursor to the end of the line */
        def clearLine(): Unit

        /** Move the cursor to a specified row and column */
        def gotoRC(row: Int, col: Int): Unit

        /** Display a string at the cursor */
        def write(s: CharSequence): Unit

        /** Display a character at the cursor */
        def write(ch: Char): Unit

        /** Set reverse video mode for future writes */
        def setRevVideo(rev: Boolean): Unit

        /** Ring the bell */
        def beep(): Unit
    }
}
