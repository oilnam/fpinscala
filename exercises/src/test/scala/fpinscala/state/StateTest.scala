package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{Matchers, FlatSpec}

class StateTest extends FlatSpec with Matchers {

  it should "check how the API works" in {
    val rng = Simple(42)

    val (n1, rng1) = rng.nextInt

    // calling nextInt with the same state should yiled the same number
    val (n, _) = rng.nextInt
    n1 should be(n)

    // calling nextInt with a different state should yiled a differnt number
    val (n3, _) = rng1.nextInt
    n3 should not be(n1)
  }

  it should "test the exercises implementations" in {

    val rng = Simple(42)

    // because of states, the order of random numbers must be the same

    val (i1, r1) = rng.nextInt
    val (i2, r2) = r1.nextInt
    val (i3, _) = r2.nextInt

    List(i3, i2, i1) should be(RNG.ints(3)(rng)._1)

    List(i1, i2, i3) should be(RNG.ints2(3)(rng)._1)
  }



}
