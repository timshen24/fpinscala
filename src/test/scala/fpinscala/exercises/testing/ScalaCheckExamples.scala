package fpinscala.exercises.testing

import org.scalacheck.{Gen, Prop}

object ScalaCheckExamples {
  @main def runMain(): Unit =
    val intList = Gen.listOf(Gen.choose(0, 100))
    val prop =
      Prop.forAll(intList)(ns =>
        ns.reverse.reverse == ns) &&
        Prop.forAll(intList)(ns =>
          ns.headOption == ns.reverse.lastOption)
    val failingProp =
      Prop.forAll(intList)(ns => ns.reverse == ns)

    prop.check()
    failingProp.check()
}



