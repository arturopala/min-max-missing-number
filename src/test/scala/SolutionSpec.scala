package com.github.arturopala.minmaxmissing

import org.scalatest.FlatSpec

class SolutionSpec extends FlatSpec {

  it should "return 0 on an empty array" in {
    assert(Solution.solution(Array.empty[Int]) === 0)
  }

  it should "return 5 on an [1, 3, 6, 4, 1, 2]" in {
    assert(Solution.solution(Array(1, 3, 6, 4, 1, 2)) === 5)
  }

  it should "return 4 on an [1, 2, 3]" in {
    assert(Solution.solution(Array(1, 2, 3)) === 4)
  }

  it should "return 4 on an [10, 8, 7, 9, 6, 1, 2, 5, 3]" in {
    assert(Solution.solution(Array(10, 8, 7, 9, 6, 1, 2, 5, 3)) === 4)
  }

  it should "return 1 on an [−1, −3]" in {
    assert(Solution.solution(Array(-1, -3)) === 1)
  }

  it should "return 0 on an [1:99999]" in {
    assert(Solution.solution((1 to 99999).toArray) === 0)
  }

  it should "return 90001 on an [1:90000]" in {
    assert(Solution.solution((1 to 90000).toArray) === 90001)
  }
}
