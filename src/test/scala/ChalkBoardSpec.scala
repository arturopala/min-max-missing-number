package com.github.arturopala.minmaxmissing
import org.scalatest.{Matchers, WordSpec}

class ChalkBoardSpec extends WordSpec with Matchers {

  def findMissingMinMaxWhen(minInc: Int, maxExc: Int, missing: Set[Int]) = {

    val expectedMin = if (missing.isEmpty) minInc - 1 else missing.min
    val expectedMax = if (missing.isEmpty) maxExc else missing.max
    val description =
      if (missing.isEmpty) "none missing"
      else if (missing.size == 1) s"${missing.head} is missing"
      else if (missing.size > 10)
        missing.take(10).mkString("", ",", ",... are missing")
      else missing.mkString("", ",", " are missing")

    s"find ($expectedMin,$expectedMax) when $description from range [$minInc,$maxExc)" in {
      val cb = new ChalkBoard(minInc, maxExc)
      for (i <- minInc until maxExc) {
        if (!missing.contains(i)) {
          cb.mark(i)
          cb.has(i) shouldBe true
        }
      }
      cb.mark(minInc - 1)
      cb.has(minInc - 1) shouldBe false
      cb.mark(maxExc)
      cb.has(maxExc) shouldBe false
      missing.foreach(x => cb.has(x) shouldBe false)
      val min = cb.minMissing.getOrElse(minInc - 1)
      min shouldBe expectedMin
      val max = cb.maxMissing.getOrElse(maxExc)
      max shouldBe expectedMax
    }
  }

  "ChalkBoard" should {

    "throw an exception when upper bound is less than lower" in {
      an[Exception] shouldBe thrownBy(new ChalkBoard(0, -1))
      an[Exception] shouldBe thrownBy(new ChalkBoard(1000, 1))
      an[Exception] shouldBe thrownBy(new ChalkBoard(1000, 999))
    }

    findMissingMinMaxWhen(minInc = 0, maxExc = 0, missing = Set())
    findMissingMinMaxWhen(minInc = 1, maxExc = 64, missing = Set(47))
    findMissingMinMaxWhen(minInc = 1, maxExc = 65, missing = Set())
    findMissingMinMaxWhen(minInc = 1, maxExc = 65, missing = (1 until 65).toSet)
    findMissingMinMaxWhen(minInc = 1, maxExc = 5, missing = Set(1, 2, 3, 4))
    findMissingMinMaxWhen(minInc = 1, maxExc = 65, missing = Set(47))
    findMissingMinMaxWhen(minInc = 1, maxExc = 66, missing = Set(47, 59))
    findMissingMinMaxWhen(minInc = 1, maxExc = 128, missing = Set(47, 59))
    findMissingMinMaxWhen(minInc = 1, maxExc = 129, missing = Set(17, 99))
    findMissingMinMaxWhen(minInc = 1, maxExc = 130, missing = Set(47, 119))
    findMissingMinMaxWhen(minInc = 3, maxExc = 64, missing = Set(47, 35, 12))
    findMissingMinMaxWhen(minInc = 1,
                          maxExc = 64,
                          missing = Set(47, 35, 12, 1, 2, 3))
    findMissingMinMaxWhen(minInc = 1,
                          maxExc = 1000,
                          missing = Set(47, 35, 12, 1, 2, 3, 987, 623, 801))
    findMissingMinMaxWhen(minInc = 1,
                          maxExc = 1000,
                          missing = (1 until 1000).toSet)
    findMissingMinMaxWhen(minInc = 1,
                          maxExc = 1000,
                          missing = Set(987, 623, 801))
    findMissingMinMaxWhen(minInc = 13,
                          maxExc = 100000,
                          missing = Set(19987, 28623, 98801))
    findMissingMinMaxWhen(minInc = 1, maxExc = 100000, missing = Set())
    findMissingMinMaxWhen(minInc = 99998, maxExc = 100000, missing = Set())
    findMissingMinMaxWhen(minInc = 99998, maxExc = 100000, missing = Set(99999))
    findMissingMinMaxWhen(minInc = 2867, maxExc = 100000, missing = Set(2867))
    findMissingMinMaxWhen(minInc = -1000,
                          maxExc = 1000,
                          missing = Set(1, -221, 28, 222))
    findMissingMinMaxWhen(minInc = 1, maxExc = 2, missing = Set())
    findMissingMinMaxWhen(minInc = -100000, maxExc = -1, missing = Set())
    findMissingMinMaxWhen(minInc = -100000,
                          maxExc = -1,
                          missing = Set(-2, -28765, -983))
    findMissingMinMaxWhen(minInc = 5,
                          maxExc = 1000,
                          missing = Set(6, 7, 8, 996, 997, 998))
    findMissingMinMaxWhen(minInc = 5,
                          maxExc = 1000,
                          missing = Set(5, 7, 997, 999))
  }
}
