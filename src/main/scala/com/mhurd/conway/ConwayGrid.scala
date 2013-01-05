import javafx.application.Application
import javafx.scene.paint.Color
import javafx.scene.shape.{LineTo, MoveTo, Path}
import javafx.scene.{Group, Scene}
import javafx.stage.Stage

class ConwayGrid extends Application {
  println("Test()")

  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Conway's Game of Life")
    primaryStage.setMaxHeight(400)
    primaryStage.setMinHeight(400)
    primaryStage.setMaxWidth(400)
    primaryStage.setMinWidth(400)
    val root: Group = new Group()
    val scene: Scene = new Scene(root, 400, 400, Color.WHITE)

    val w = scene.getWidth()
    val h = scene.getHeight()

    val size = 20

    val htOfRow = h / (size);

    val path: Path = new Path()

    (0 until size) foreach (
      k => {
        val moveTo: MoveTo = new MoveTo()
        moveTo.setX(0)
        moveTo.setY(k * htOfRow)
        val lineTo: LineTo = new LineTo()
        lineTo.setX(w)
        lineTo.setY(k * htOfRow)
        path.getElements().add(moveTo)
        path.getElements().add(lineTo)
      }
      )

    val wdOfRow = w / (size)
    (0 until size) foreach (
      k => {
        val moveTo: MoveTo = new MoveTo()
        moveTo.setX(k * wdOfRow)
        moveTo.setY(0)
        val lineTo: LineTo = new LineTo()
        lineTo.setX(k * wdOfRow)
        lineTo.setY(h)
        path.getElements().add(moveTo)
        path.getElements().add(lineTo)
      }

      )

    path.setStrokeWidth(1)
    path.setStroke(Color.BLACK)

    root.getChildren().add(path)

    primaryStage.setScene(scene)
    primaryStage.show()
  }

}

object ConwayGrid {
  def main(args: Array[String]) {
    Application.launch(classOf[ConwayGrid], args: _*)
  }
}