import org.scalacheck._
import Prop.forAll

object Q_Text extends org.scalacheck.Properties("Text") {

    // Exhaustive testing of a range
    def forEach[T](s: Iterable[T])(p: T => Boolean): Boolean = s.forall(p)

    property("one") =
        forAll { (s: String) =>  
            val t = new Text(); t.insert(0, s)
            t.toString() == s }

    property("two") =
        forAll { (s: String) =>
            val t = new Text(s)
            t.insert(s.length/2, 'x'); t.deleteChar(s.length/2)
            forEach(0 until s.length) { i => 
                t.charAt(i) == s.charAt(i) } }

    property("three") = 
        forAll { (s1: String, s2: String) =>
            val t = new Text(s1); t.insert(t.length, s2)
            t.toString() == s1 + s2 }

    // This is needed if we want to run the test as a script
    override def main(args: Array[String]) { super.main(args) }
}
