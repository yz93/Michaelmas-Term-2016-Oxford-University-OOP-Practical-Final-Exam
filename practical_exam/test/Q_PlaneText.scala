import org.scalacheck._
import Prop.forAll

object Q_PlaneText extends org.scalacheck.Properties("PlaneText") {

    // Characters that have a 1/5 chance of being a newline
    def mychar = Gen.frequency((1, '\n'), (4, Arbitrary.arbitrary[Char]))

    // Strings of characters from that distribution
    def mystring = Gen.containerOf[Array, Char](mychar).map(new String(_))

    // Exhaustive testing of a range
    def forEach[T](s: Iterable[T])(p: T => Boolean): Boolean = s.forall(p)

    property("one") =
        forAll(mystring) { (s: String) =>  
            val t = new PlaneText(); t.insert(0, s)
            t.numLines == s.count(_ == '\n') + 1 }

    property("two") =
        forAll(mystring) { (s: String) =>
            val t = new PlaneText(); t.insert(0, s)
            forEach(0 to s.length) { pos =>
                val r = t.getRow(pos); val c = t.getColumn(pos);
                t.getPos(r, c) == pos } }

    property("three") =
        forAll(mystring) { (s: String) =>
            val t = new PlaneText(); t.insert(0, s);
            forEach(0 to s.length) { pos =>
                t.getRow(pos) == s.substring(0, pos).count(_ == '\n') }}

    // This is needed if we want to run the test as a script
    override def main(args: Array[String]) { super.main(args) }
}
