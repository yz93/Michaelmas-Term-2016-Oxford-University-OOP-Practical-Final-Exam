import org.scalacheck._
import commands._

object Test extends Properties("test") {

    class PriqueueSpec(factory: PriorityQueue.Factory) extends Commands {
        // The abstract state
        type State = List[Int]

        // The concrete state of the System Under Test
        type Sut = PriorityQueue[Int]

        // How to generate an initial abstract state        
        val genInitialState = Gen.const(List())
        def initialPreCondition(state: State) = true

        // How to create a corresponding concrete state
        def newSut(state: State) = factory.makeQueue[Int]

        // The method insert(x)
        case class Insert(x: Int) extends UnitCommand {
            def run(q: PriorityQueue[Int]) { q.insert(x) }
            def nextState(s: State) = x :: s
            def preCondition(s: State) = true
            def postCondition(s: State, success: Boolean) = success
        }

        // The method delMin()
        case object DelMin extends SuccessCommand {
            type Result = Int
            def run(q: PriorityQueue[Int]) = q.delMin()
            def nextState(s: State) = s diff List(s.min)
            def preCondition(s: State) = ! s.isEmpty
            def postCondition(s: State, r: Int) = (r == s.min)
        }

        // The method isEmpty
        case object IsEmpty extends SuccessCommand {
            type Result = Boolean
            def run(q: PriorityQueue[Int]) = q.isEmpty
            def nextState(s: State) = s
            def preCondition(s: State) = true
            def postCondition(s: State, r: Boolean) = (r == s.isEmpty)
        }

        // Generate an Insert command with random argument
        val genInsert = for (x <- Gen.choose(1, 100)) yield Insert(x)

        // Generate a command that's available in a given state
        def genCommand(s: State) = {
            var cmds: List[Gen[Command]] = 
                List(genInsert, Gen.const(IsEmpty))

            if (! s.isEmpty) 
                cmds ++= List(Gen.const(DelMin))

            for (g <- Gen.oneOf(cmds); x <- g) yield x
        }

        // Boilerplate
        def canCreateNewSut(newState: State, initSuts: Traversable[State],
            runningSuts: Traversable[Sut]) = true
        def destroySut(sut: Sut) { }
    }

    override def main(args: Array[String]) { 
        val factory =
            args(0) match {
                case "simple" => SimplePriorityQueue.factory
                case "heap" => HeapPriorityQueue.factory
                // case "rbt" => RbtPriorityQueue.factory
            }
        property("spec") = new PriqueueSpec(factory).property()
        super.main(args.tail) 
    }
}

