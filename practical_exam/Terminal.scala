// Terminal.scala
// Copyright (c) 2015 J. M. Spivey

import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import java.awt.FontMetrics
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.event.InputEvent
import java.awt.event.KeyEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent

import java.io.FileWriter
import java.io.IOException
import java.io.PrintWriter

import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.SwingUtilities
import javax.swing.{WindowConstants => Swing}

/** A simulation of a simple VDU using Swing. */
class Terminal(banner: String) extends Display.Hardware {
    /** The source for keystrokes. */
    private val queue = new Terminal.InputQueue()
    
    /** If non-null, the screen on which the display is shown. */
    private var screen: Screen = null

    /** The characters displayed on the screen. */
    private val text = Array.ofDim[Char](Display.HEIGHT, Display.WIDTH)
    
    /** Flags to say if characters on the screen are in reverse video */
    private val rev = Array.ofDim[Boolean](Display.HEIGHT, Display.WIDTH)
    
    /** The current location of the cursor. */
    private var curs_col = 0
    private var curs_line = 0
    
    /** Whether new characters are written in reverse video. */
    private var rev_video = false
    
    /** If non-null, a file used to record keystrokes for later replay */
    private var dribble: PrintWriter = null
    
    clear()
        
    scala.sys.env.get("DRIBBLE") match {
        case Some(name) => 
            dribble = new PrintWriter(new FileWriter(name))
        case None => ()
    }

    def activate() {
        SwingUtilities.invokeLater(new Runnable() {
            def run() {
                screen = new Screen()

                val gui = new JFrame(banner)
                gui.setDefaultCloseOperation(Swing.DO_NOTHING_ON_CLOSE)
                gui.getContentPane().add(screen)

                gui.addWindowListener(new WindowAdapter() {
                    override def windowClosing(e: WindowEvent) {
                        queue.enqueue(Display.ctrl('Q'))
                    }

                    override def windowOpened(e: WindowEvent) {
                        // Must delay this, as it interferes with 
                        // setLocationByPlatform
                        gui.setResizable(false)
                    }
                })

                gui.setLocationByPlatform(true)
                gui.pack()
                gui.setVisible(true)
                screen.requestFocus()
            }
        })
    }

    /** Wait for a keystroke and return it. */
    def getKey() = {
        if (screen != null) screen.repaint()
        val key = queue.getKey()
        if (dribble != null) {
            dribble.println(key)
            dribble.flush()
        }
        key
    }

    /** Discard any pending input */
    def flush() { queue.flush() }

    /** Clear the whole screen */
    def clear() { 
        curs_col = 0; curs_line = 0; rev_video = false
        for (i <- 0 until Display.HEIGHT; j <- 0 until Display.WIDTH) {
            text(i)(j) = ' '; rev(i)(j) = false
        } 
        if (screen != null) screen.repaint()
    }

    /** Clear from the cursor to the end of the line */
    def clearLine() { 
        for (j <- curs_col until Display.WIDTH) {
            text(curs_line)(j) = ' '; rev(curs_line)(j) = false
        }
        if (screen != null) screen.repaint()
    }

    /** Move the cursor to a specified row and column */
    def gotoRC(row: Int, col: Int) { 
        if (row < 0 || row >= Display.HEIGHT 
                || col < 0 || col >= Display.WIDTH) return
        curs_line = row; curs_col = col
        if (screen != null) screen.repaint()
    }
    
    /** Store a character at the cursor */
    private def putChar(ch: Char) {
        text(curs_line)(curs_col) = ch
        rev(curs_line)(curs_col) = rev_video
        if (curs_col < Display.WIDTH-1) curs_col += 1
    }
    
    /** Display a string at the cursor */
    def write(s: CharSequence) { 
        for (i <- 0 until s.length) putChar(s.charAt(i))
    }

    /** Display a character at the cursor */
    def write(ch: Char) { 
        putChar(ch)
        if (screen != null) screen.repaint()
    }

    /** Set reverse video mode for future writes */
    def setRevVideo(rev: Boolean) { rev_video = rev }

    /** Ring the bell */
    def beep() {
        java.awt.Toolkit.getDefaultToolkit().beep()

        // beep doesn't work under Gnome, so try this instead.
        // Assumes a shell script for 'beep', such as
        // aplay $HOME/lib/ping.wav >/dev/null 2>&1 &
        /* 
        val rt = java.lang.Runtime.getRuntime()
        if (rt != null) rt.exec("beep")
        */

        // If all else fails ...
        println("BEEP!")
    }
    
    /** Write the screen contents on a file.  This is used to save the
     * results of a test. */
    def writeScreen(out: PrintWriter) {
        for (i <- 0 until Display.HEIGHT) {
            for (j <- 0 until Display.WIDTH) {
                if (curs_line == i && curs_col == j) out.print('#')
                out.print(text(i)(j))
            }
            out.println()
        }
        out.flush()
    }

    /** A Swing component that simulates the screen and captures keystrokes. */
    private class Screen extends JPanel {
        /** Border between the text and the edge of the screen */
        private val border = 5
        
        private val myfont = new Font("Monospaced", Font.PLAIN, 15)
        setFont(myfont)
        private val fm = getFontMetrics(myfont)
        private val char_width = fm.charWidth('M')
        private val char_height = fm.getHeight()
        private val char_ascent = fm.getAscent()

        setPreferredSize(new Dimension(
            Display.WIDTH * char_width + 2 * border, 
            Display.HEIGHT * char_height + 2 * border))
        setFocusTraversalKeysEnabled(false)
            
        setBackground(Color.white)
        setForeground(Color.black)

        /* For simplicity, we avoid creating a UI delegate, and just implement
         * paintComponent and processComponentKeyEvent directly. */ 

        override def paintComponent(g: Graphics) {
            super.paintComponent(g)
            
            val fg = getForeground()
            val bg = getBackground()

            val g2 = g.asInstanceOf[Graphics2D]
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                                RenderingHints.VALUE_ANTIALIAS_ON)

            g.setColor(fg)
            for (i <- 0 until Display.HEIGHT) {
                val y = i * char_height + border
                for (j <- 0 until Display.WIDTH) {
                    val x = j * char_width + border
                    if (rev(i)(j) || i == curs_line && j == curs_col) {
                        g.fillRect(x, y, char_width, char_height)
                        g.setColor(bg)
                        g.drawChars(text(i), j, 1, x, y+char_ascent)
                        g.setColor(fg)
                    } else {
                        if (text(i)(j) != ' ')
                            g.drawChars(text(i), j, 1, x, y+char_ascent)
                    }
                }
            }
        }

        /** Grab all key events for the component */
        override def processComponentKeyEvent(ev: KeyEvent) {
            if (ev.getID() == KeyEvent.KEY_PRESSED) keyPressed(ev)
            ev.consume()
        }

        private def keyPressed(ev: KeyEvent) {
            var ch = Display.UNDEFINED

            ev.getKeyCode() match {
                case KeyEvent.VK_CONTROL | KeyEvent.VK_SHIFT |
                        KeyEvent.VK_ALT =>     return
                case KeyEvent.VK_UP =>         ch = Display.UP
                case KeyEvent.VK_DOWN =>       ch = Display.DOWN
                case KeyEvent.VK_LEFT =>       ch = Display.LEFT
                case KeyEvent.VK_RIGHT =>      ch = Display.RIGHT
                case KeyEvent.VK_PAGE_UP =>    ch = Display.PAGEUP
                case KeyEvent.VK_PAGE_DOWN =>  ch = Display.PAGEDOWN
                case KeyEvent.VK_INSERT =>     ch = Display.INS
                case KeyEvent.VK_DELETE =>     ch = Display.DEL
                case KeyEvent.VK_F1 =>         ch = Display.F1
                case KeyEvent.VK_F2 =>         ch = Display.F2
                case KeyEvent.VK_F3 =>         ch = Display.F3
                case KeyEvent.VK_F4 =>         ch = Display.F4
                case KeyEvent.VK_F5 =>         ch = Display.F5
                case KeyEvent.VK_F6 =>         ch = Display.F6
                case KeyEvent.VK_F7 =>         ch = Display.F7
                case KeyEvent.VK_F8 =>         ch = Display.F8
                case KeyEvent.VK_F9 =>         ch = Display.F9
                case KeyEvent.VK_F10 =>        ch = Display.F10
                case KeyEvent.VK_F11 =>        ch = Display.F11
                case KeyEvent.VK_F12 =>        ch = Display.F12
                case KeyEvent.VK_BACK_SPACE => ch = 0x7f
                case KeyEvent.VK_TAB =>        ch = Display.TAB
                case KeyEvent.VK_ENTER =>      ch = Display.RETURN

                case KeyEvent.VK_HOME =>
                    if ((ev.getModifiers() & InputEvent.CTRL_MASK) != 0)
                        ch = Display.CTRLHOME
                    else 
                        ch = Display.HOME

                case KeyEvent.VK_END =>
                    if ((ev.getModifiers() & InputEvent.CTRL_MASK) != 0)
                        ch = Display.CTRLEND
                    else 
                        ch = Display.END

                case _ =>
                    val ch1 = ev.getKeyChar()
                    if (ch1 != KeyEvent.CHAR_UNDEFINED) ch = ch1
            }

            queue.enqueue(ch)
        }
    }
}

object Terminal {    
    /** A small queue of characters that have been typed on the keyboard
     * but not consumed by the editor.
     * This class uses the concurrent features of Java to provide
     * necessary synchronization and prevent unwelcome
     * interference between the GUI update thread, which is pushing
     * characters into the buffer as they are typed, and the main application
     * thread, which is taking them out and consuming them.
     * In particular, when there are no characters waiting, a call to
     * getKey() will be suspended until a key is pressed. */
    private class InputQueue {
        val QMAX = 10

        /* The queue contains queue[head..head+len), where 
         * the subscripts are taken modulo QMAX. */
        val queue = new Array[Int](QMAX)
        var head = 0
        var len = 0

        /** Add a keycode to the input queue */
        def enqueue(ch: Int) {
            this.synchronized {
                // Just toss the character if the queue is full
                if (len >= QMAX) return
                queue((head+len)%QMAX) = ch
                len += 1
                notify()
            }
        }

        /** Wait for a keystroke to be available, then return it. */
        def getKey(): Int = {
            this.synchronized {
                // Wait for the queue to be non-empty
                while (len == 0) wait()
                val ch = queue(head)
                head = (head+1)%QMAX; len -= 1
                ch
            }
        }

        /** Discard pending input. */
        def flush() {
            this.synchronized {
                head = 0; len = 0
            }
        }
    }
}
