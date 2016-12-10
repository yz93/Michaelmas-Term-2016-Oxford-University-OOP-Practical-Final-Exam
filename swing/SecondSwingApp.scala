import scala.swing._

object SecondSwingApp extends SimpleSwingApplication {

    def top = new MainFrame {

        title = "Second Swing App"

        val button = new Button {
            text = "Click me"
        }

        val label = new Label {
            text = "No button clicks registered"
        }       

        contents = new BoxPanel(Orientation.Vertical) {
            contents += button
            contents += label
            border = Swing.EmptyBorder(20, 20, 20, 20)
        }
    }
}    
