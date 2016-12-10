// Testbed.scala
// Copyright (c) 2015 J. M. Spivey

import java.io.{BufferedReader, FileReader, Reader, PrintStream}

/** A testbed for executing canned command sequences. */
object Testbed {
    def main(args: Array[String]) {
        if (args.length != 2) {
            Console.err.println("Usage: testbed file input")
            scala.sys.exit(2)
        }
        
        val file = args(0)
        val input = args(1)
        
        val testin = new BufferedReader(new FileReader(input))
        val terminal = new Playback(testin)
        val app = new Editor()
        val display = new Display(terminal)
        app.activate(display)
        app.loadFile(file)
        app.commandLoop()
        terminal.writeScreen(Console.out)
        scala.sys.exit(0)
    }

    /** A fake terminal that takes input from a file */
    class Playback(testin: Reader) extends Display.Hardware {
        private val WIDTH = Display.WIDTH
        private val HEIGHT = Display.HEIGHT

        /** The characters displayed on the screen. */
        private val text = Array.ofDim[Char](HEIGHT, WIDTH)
    
        /** The current location of the cursor. */
        private var col = 0
        private var row = 0
    
        clear()

        /** Get a keystroke from the file. */
        def getKey() = {
            // Yawn.
            val chars = new StringBuffer()
            var ch = getch()
        
            while (ch.isSpaceChar) ch = getch()
            
            if (! ch.isDigit)
                throw new Error("Bad test input")
            
            while (ch.isDigit) {
                chars.append(ch); ch = getch()
            }
            
            chars.toString.toInt
        }
        
        /** Get a character from the file. */
        private def getch() = {
            val ch = testin.read()
            if (ch < 0) throw new Error("Unexpected EOF on test input")
            ch.toChar
        }

        /** Discard any pending input */
        def flush() { }

        /** Clear the whole screen */
        def clear() { 
            col = 0; row = 0
            for (i <- 0 until HEIGHT; j <- 0 until WIDTH)
                text(i)(j) = ' '
        }

        /** Clear from the cursor to the end of the line */
        def clearLine() { 
            for (j <- col until WIDTH)
                text(row)(j) = ' '
        }

        /** Move the cursor to a specified row and column */
        def gotoRC(r: Int, c: Int) { 
            if (r < 0 || r >= HEIGHT || c < 0 || c >= WIDTH) return
            row = r; col = c
        }
        
        /** Display a string at the cursor */
        def write(s: CharSequence) { 
            for (i <- 0 until s.length)
                write(s.charAt(i))
        }
        
        /** Display a character at the cursor */
        def write(ch: Char) { 
            text(row)(col) = ch
            if (col < WIDTH-1) col += 1
        }
        
        /** Set reverse video mode for future writes */
        def setRevVideo(rev: Boolean) { }

        /** Ring the bell */
        def beep() {
            println("BEEP!")
        }
    
        /** Write the screen contents on a file.  This is used to save the
         * results of a test. */
        def writeScreen(out: PrintStream) {
            for (i <- 0 until HEIGHT) {
                for (j <- 0 until WIDTH) {
                    if (row == i && col == j) out.print('#')
                    out.print(text(i)(j))
                }
                out.println()
            }
            out.flush()
        }
    }
}
