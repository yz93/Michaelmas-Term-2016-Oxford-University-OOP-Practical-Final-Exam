// AppFrame.scala
// Copyright (c) 2013 J. M. Spivey

import scala.swing._
import scala.swing.event._
import java.awt.{Font, Color}
import javax.swing.UIManager

/** The main application window for AutoSnail */
class AppFrame(map: RoadMap, app: AutoSnail) 
        extends MainFrame with Observable.Observer[PathFinder] {

    title = "AutoSnail Express"
    font = new Font("SansSerif", Font.PLAIN, 12)

    private val viewer = new MapViewer(map, app)

    import AppFrame.PassiveTextField
    private val fromText = new PassiveTextField(20)
    private val toText = new PassiveTextField(20)
    private val distText = new PassiveTextField(20)
    
    private val workCheck = new CheckBox("Show working")
    private val heurCheck = new CheckBox("Use heuristic")
    private val delayCheck = new CheckBox("Delay between steps")
    
    /** The slider for delay between steps */
    private val delaySlider = new Slider {
        min = 0; max = 100; value = 50
        majorTickSpacing = 100; minorTickSpacing = 10
        paintTicks = true; paintLabels = true
        visible = false
    }

    private val searchButton = 
        Button("Search") {
            app.search(heurCheck.selected)
        }

    private val resetButton = Button("Reset") { app.reset() }

    private val buttons = new FlowPanel(searchButton, resetButton)
    
    private val labelFont = new Font("SansSerif", Font.BOLD, 12)
        
    private class MyLabel(txt: String) extends Label {
        text = txt; font = labelFont
    }

    private val controls = new GridBagPanel {
        val elements = Array(new MyLabel("From:"), fromText,
            new MyLabel("To:"), toText,
            new MyLabel("Distance:"), distText,
            workCheck, heurCheck, delayCheck,
            delaySlider)

        var i = 0
        for (c <- elements) {
            layout(c) = new Constraints {
                gridx = 0; gridy = i
                anchor = GridBagPanel.Anchor.West
                insets = new Insets(5, 5, 5, 5)
            }
            i += 1
        }
    }
                
    private val column = new BorderPanel {
        add(controls, BorderPanel.Position.North)
    }

    // Basic layout: map with controls at right, buttons at bottom
    contents = new BorderPanel {
        add(viewer, BorderPanel.Position.Center)
        add(column, BorderPanel.Position.East)
        add(buttons, BorderPanel.Position.South)
    }

    // Respond to workCheck
    listenTo(workCheck)
    reactions += {
        case ButtonClicked(`workCheck`) =>
            viewer.showWork = workCheck.selected
    }

    // Respond to delay controls
    listenTo(delayCheck, delaySlider)
    reactions += {
        case ButtonClicked(`delayCheck`) =>
            delaySlider.visible = delayCheck.selected
            setDelay()

        case ValueChanged(`delaySlider`) =>        
            setDelay()
    }
    
    /** Adjust the delay to match what is selected by the controls */
    private def setDelay() {
        if (delayCheck.selected)
            app.delay = 10 * delaySlider.value
        else
            app.delay = 0
    }
    
    // React to events from viewer
    listenTo(viewer)
    reactions += { 
        case MapViewer.TownClicked(t) => app.select(t)
    }

    /** Refresh the display -- called from worker thread */
    def refresh(pathfinder: PathFinder) {
        viewer.refresh(pathfinder)
        Swing.onEDT { updateDisplay(pathfinder) }
    }
    
    /** Update the display -- called from event thread */
    private def updateDisplay(pathfinder: PathFinder) {
        fromText.text = app.fromTown.name
        toText.text = app.toTown.name
        
        if (pathfinder == null)
            distText.text = ""
        else {
            val d = pathfinder.distance
            if (d < PathFinder.INFINITY)
                distText.text = "%.1f".format(d)
            else 
                distText.text = "infinity"
        }
        
        if (pathfinder == null) {
            searchButton.enabled = app.canSearch
            resetButton.enabled = app.canReset
        } else {
            searchButton.enabled = pathfinder.isDone
            resetButton.enabled = pathfinder.isDone
        }
    }
}

object AppFrame {
    // Make tooltips appear on a light yellow b/g
    UIManager.put("ToolTip.background", new Color(255, 255, 225))

    /** An uneditable text field with a white background */
    private class PassiveTextField(w: Int) extends TextField {
        columns = w; editable = false
        background = Color.WHITE
    }
}
