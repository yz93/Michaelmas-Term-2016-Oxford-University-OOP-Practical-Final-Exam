import scala.swing._

object FirstSwingApp extends SimpleSwingApplication {

    def top = new MainFrame {

        title = "First Swing App"

        contents = new Button {
            text = "Click me"
        }
    }
}
