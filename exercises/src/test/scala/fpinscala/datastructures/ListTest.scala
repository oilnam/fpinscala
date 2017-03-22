package fpinscala.datastructures

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  it should "test List operations" in {

    val l2: List[Int] = Cons(1, Cons(2, Nil))
    val l4 = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))

    List.tail(l2) should be(Cons(2, Nil))

    List.drop(l2, 1) should be(Cons(2, Nil))
    List.drop(l2, 2) should be(Nil)

    List.dropWhile(l4, (x: Int) => x <= 2) should be(Cons(3, Cons(4, Nil)))

    List.length(l4) should be(4)

    List.map(l2)(identity) should be(l2)

    List.setHead(l4, 9) should be(Cons(9, Cons(2, Cons(3, Cons(4, Nil)))))

    List.init(l4) should be(Cons(1, Cons(2, Cons(3, Nil))))

  }

}
