// MapViewer.scala
// Copyright (c) 2015 J. M. Spivey

import scala.swing._
import scala.swing.event._
import java.awt.{Color, BasicStroke, RenderingHints}
import java.awt.geom.{Point2D, Line2D, Rectangle2D, Path2D}
       
class MapViewer(map: RoadMap, app: AutoSnail) extends Component {

    import MapViewer._

    /** Current search */
    private var pathfinder: PathFinder = null
    
    /** Whether to show progress or only the final result */
    var showWork = false

    minimumSize = new Dimension(width, height)
    preferredSize = minimumSize
    background = new Color(127, 225, 255) // Blue sea
    foreground = new Color(255, 127, 63) // Tawny sand
        
    // Turn on tooltips
    tooltip = ""
        
    // Promise to draw on entire surface
    opaque = true
        
    // Turn on mouse events
    listenTo(mouse.clicks)
    
    /** Respond to mouse clicks */
    reactions += {
        case e: MouseClicked =>
            val t = findTown(e.point)
            if (t != null) publish(new MapViewer.TownClicked(t))
    }

    /** React to updates */
    def refresh(pf: PathFinder) {
        pathfinder = pf
        repaint()
    }
    
    /** Determine town colour according to status */
    private def townColor(t: RoadMap.Town) = {
        if (t == app.fromTown)
            Color.GREEN
        else if (t == app.toTown)
            Color.RED
        else if (pathfinder == null || ! showWork)
            Color.BLACK
        else {
            import PathFinder.TownStatus._
            pathfinder.townStatus(t) match {
                case UNSEEN => Color.BLACK
                case KNOWN => Color.WHITE
                case ACTIVE => Color.GRAY
            }
        }
    }
    
    /** Brown colour for abandoned roads */
    private val BROWN = new Color(128, 16, 16)
    
    /** Determine road colour according to status */
    private def roadColor(r: RoadMap.Road) = {
        if (pathfinder == null) 
            Color.BLACK
        else {            
            import PathFinder.RoadStatus._
            val st = pathfinder.roadStatus(r)
            if (st == PATH)
                Color.YELLOW
            else if (! showWork) 
                Color.BLACK
            else {
                st match {
                    case UNSEEN => Color.BLACK
                    case ACTIVE => Color.GREEN
                    case DEAD => BROWN
                    case TREE => Color.BLUE
                }
            }
        }
    }
    
    /** Paint all parts of the map */
    override def paintComponent(g: Graphics2D) {
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, 
                           RenderingHints.VALUE_ANTIALIAS_ON)
        
        g.setColor(background)
        g.fillRect(0, 0, size.width, size.height)
        drawCoastlines(g)
        drawRoads(g)
        drawTowns(g)
    }
    
    /** Half the size of a town */
    private val delta = 3.0f

    /** Draw all the towns */
    private def drawTowns(g: Graphics2D) {
        for (t <- map.towns) {
            val centre = transform(t.location)
            val rect = 
                new Rectangle2D.Float(centre.getX.toFloat - delta, 
                        centre.getY.toFloat - delta, 2*delta, 2*delta)
            g.setColor(townColor(t))
            g.fill(rect)
            g.setColor(Color.BLACK)
            g.draw(rect)
        }
    }

    /** Width for drawing a road */
    private val roadWidth = 2.0f
    
    /** Draw all the roads */
    private def drawRoads(g: Graphics2D) {
        val stroke0 = g.getStroke()
        g.setStroke(new BasicStroke(roadWidth, BasicStroke.CAP_ROUND, 
                                    BasicStroke.JOIN_ROUND))

        for (r <- map.roads) {
            g.setColor(roadColor(r))
            g.draw(new Line2D.Float(transform(r.town1.location), 
                    transform(r.town2.location)))
        }
        
        g.setStroke(stroke0)
    }
    
    /** Draw and fill the coastlines */
    private def drawCoastlines(g: Graphics2D) {
        for (o <- map.coastlines()) {
            val poly = new Path2D.Float

            val p0 = transform(o.head)
            poly.moveTo(p0.getX, p0.getY)
            for (p <- o.tail) {
                val pp = transform(p)
                poly.lineTo(pp.getX, pp.getY)
            }

            poly.closePath()
            g.setColor(foreground)
            g.fill(poly)
            g.setColor(Color.BLACK)
            g.draw(poly)
        }
    }

    /** Find the town that contains a given point, or return null */
    protected def findTown(p: Point2D) = {
        var town: RoadMap.Town = null

        // A linear search of the list of towns.
        for (t <- map.towns) {
            val q = transform(t.location)
            if (p.distance(q) <= tol) town = t
        }
        
        town
    }       

    /** Calculate tool tip from mouse coordinates */
    def getTooltip(e: MouseEvent) = {
        val t = findTown(e.point)
        if (t != null) t.name else null 
    }

    // This magically connects tooltips with the underlying JComponent
    override lazy val peer =
       new javax.swing.JComponent with SuperMixin {
           override def getToolTipText(e: java.awt.event.MouseEvent) =
               getTooltip(new MouseMoved(e))
       }
}

object MapViewer {
    /** Canvas size */
    val width = 350; val height = 500

    /** Parameters for transforming map coordinates to the screen */
    val scale = 0.8f
    val offx = -10.0f
    val offy = -10.0f

    /** Tolerance for mouse hits on towns */
    val tol = 5.0f

    /** Transform a point from map to window coordinates */
    def transform(p: RoadMap.Coords) =
        new Point2D.Float(scale*p.x+offx, height-scale*p.y+offy)

    /** Event when a town is clicked */
    case class TownClicked(town: RoadMap.Town) extends Event
}
