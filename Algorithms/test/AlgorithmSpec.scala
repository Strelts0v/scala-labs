package com.gv.lab.utils

import org.scalatest._

class AlgorithmSpec extends FunSuite{

  val algo = new Algorithm

  test("A result array should contains a defined list of elements after" +
    " handling with iteration algorithm") {
    val arr = Array(1, 2, 3, 4, 5)

    val result = algo.handleArray(arr)

    assert(result(0) == 2)
    assert(result(1) == 1)
    assert(result(2) == 4)
    assert(result(3) == 2)
    assert(result(4) == 6)
  }

  test("A result list should contains a defined list of elements after" +
    " handling with iteration algorithm") {
    val arr = List(1, 2, 3, 4, 5)

    val result = algo.handleList(arr)

    assert(result(0) == 2)
    assert(result(1) == 1)
    assert(result(2) == 4)
    assert(result(3) == 2)
    assert(result(4) == 6)
  }


  test("A result array should contains a defined list of elements after" +
    " handling with tail recursion algorithm") {
    val arr = Array(1, 2, 3, 4, 5)

    val result = algo.handleArrayTailRec(arr)

    assert(result(0) == 2)
    assert(result(1) == 1)
    assert(result(2) == 4)
    assert(result(3) == 2)
    assert(result(4) == 6)
  }

  test("A result list should contains a defined list of elements after" +
    " handling with recursion algorithm") {
    val arr = List(1, 2, 3, 4, 5)

    val result = algo.handleListRec(arr)

    assert(result(0) == 2)
    assert(result(1) == 1)
    assert(result(2) == 4)
    assert(result(3) == 2)
    assert(result(4) == 6)
  }

  test("A result list should contains a defined list of elements after" +
    " handling with tail recursion algorithm") {
    val arr = List(1, 2, 3, 4, 5)

    val result = algo.handleListTailRec(arr)

    assert(result(0) == 2)
    assert(result(1) == 1)
    assert(result(2) == 4)
    assert(result(3) == 2)
    assert(result(4) == 6)
  }

  test("A List of result elements should correspond to sum of elements in 2 input Lists") {
    val lst1 = List(1, 2, 3, 4, 5)
    val lst2 = List(1, 2, 3, 4, 5)

    val result = algo.sumLists(lst1, lst2)

    assert(result(0) == 2)
    assert(result(1) == 4)
    assert(result(2) == 6)
    assert(result(3) == 8)
    assert(result(4) == 10)
  }

  test ("Result list should contains only simple numbers from m to n") {
    val n = 1
    val m = 20
    val lst = algo.simpleNumbers(n, m)

    assert(lst.equals(List(2,3,5,7,11,13,17,19)))
  }

  test ("Returns factorial of num") {
    assert(algo.factorialTailRec(1) == 1)
    assert(algo.factorialTailRec(2) == 2)
    assert(algo.factorialTailRec(3) == 6)
    assert(algo.factorialTailRec(4) == 24)
    assert(algo.factorialTailRec(5) == 120)
    assert(algo.factorialTailRec(6) == 720)
  }

  test ("Returns factorial of num using tail recursion algorithm") {
    assert(algo.factorial(1) == 1)
    assert(algo.factorial(2) == 2)
    assert(algo.factorial(3) == 6)
    assert(algo.factorial(4) == 24)
    assert(algo.factorial(5) == 120)
    assert(algo.factorial(6) == 720)
  }

  test("Returns pairs (element and its count of occurrences) from input List object"){
    val result = algo.getCountOccurrences(List(1, 1, 2, 3))

    assert(result(1) == 2)
    assert(result(2) == 1)
    assert(result(3) == 1)
  }

  test("Deletes element from list"){
    val result = algo.deleteElement(List(1, 1, 2, 2, 1, 3, 4, 7), 1)
    assert(result.equals(List(2,2,3,4,7)))
  }

  test("Returns set with common elements from both input sets"){
    val s1 = Set[Int](1,2,3,4)
    val s2 = Set[Int](3,4,5,6)
    val result = algo.intersectWithSets(s1, s2)
    assert(result.equals(Set[Int](3,4)))
  }

  test("Returns list with common elements from both input lists"){
    val l1 = List[Int](1,2,3,4)
    val l2 = List[Int](3,4,5,6, 1, 1)
    val result = algo.intersectWithLists(l1, l2)
    assert(result.equals(List[Int](1,3,4)))
  }

  test("Computes the average value beetween all numbers in list"){
    val l1 = List[Int](1,2,3,4,5)
    assert(algo.average(l1) == 3)
    val l2 = List[Int](1,2,3,4,5,6)
    assert(algo.average(l2) == 3.5)
    val l3 = List[Int](10,10,3,11,5,6,7,8)
    assert(algo.average(l3) == 7.5)
//    val l4 = List[Int](3,4,3)
//    assert(algo.average(l4) == 3.33)
  }

  test("Unites and sort 2 lists"){
    val l1 = List(1, 7, 4, 3)
    val l2 = List(1, 0, 2, 4)
    val result = algo.uniteAndSort(l1, l2)
    assert(result.equals(List(0,1,1,2,3,4,4,7)))
  }

  test("divide list on two, 1 - with positive numbers, 2 - negative"){
    val lst = List[Int](1, 2, -2, -1, 5, -7)
    val result = algo.divide(lst)
    assert(result.equals((List[Int](-2, -1 , -7), List[Int](1, 2 ,5))))
  }

  test("Finds GCD of 3 numbers"){
    assert(algo.GCD(1512, 621, 2322) == 27)
  }

  test("Duplicates all elements in list"){
    val lst = List[Int](1,2,3,4,1)
    assert(algo.duplicate(lst).equals(List[Int](1,1,2,2,3,3,4,4,1,1)))
  }

  test("After handling of map with names and scores of players" +
    " returns 2 sorted list, 1st - names, 2nd - scores"){
    val players = Map[String, Int]("Messi" -> 10, "Suarez" -> 7, "Neymar" -> 9)
    assert(algo.divideAndSort(players).equals((List[String]("Messi", "Neymar", "Suarez"), List[Int](10, 9, 7))))
  }

  test("Returns sum of all elements in collection"){
    val lst = List(1,2,3,4,5)
    val set = Set(1,2,3,4,5)
    val seq = Seq(1,2,3,4,5)
    assert(algo.sumCollection(lst) == 15)
    assert(algo.sumCollection(set) == 15)
    assert(algo.sumCollection(seq) == 15)
  }

  test("Returns count of pairs of similar elements in 2 lists that has similar positions"){
    val l1 = List(1,2,3,4,5)
    val l2 = List(3,1,2,4,5)

    assert(algo.countSimilarElementsOnTheSamePositions(l1, l2) == 2)
  }

  test("Removes from 1st list odd items, from 2nd even items and concatanates them"){
    val l1 = List(1,2,3,4,5,6)
    val l2 = List(1,2,3,4,5,6)

    assert(algo.deleteOddEven(l1, l2).equals(List(1,3,5,2,4,6)))

  }

  test("Sorts elements in list using quicksort algorithm"){
    val lst = List(4,1,3,7,2,9,10,5,6)
    assert(algo.quicksort(lst).equals(List(1,2,3,4,5,6,7,9,10)))
  }

  test("checks if Collats hypothesis"){
    assert(algo.checkCollatsHypothesis(5))
    assert(algo.checkCollatsHypothesis(3))
    assert(algo.checkCollatsHypothesis(333))
    assert(algo.checkCollatsHypothesis(444777))
    assert(algo.checkCollatsHypothesis(4447777))
    assert(algo.checkCollatsHypothesis(24000000))
    assert(algo.checkCollatsHypothesis(99999999))
  }

  test("куег"){
    val isOdd = (n: Int) => if(n % 2 == 0) true else false
    val lst = List(1,2,3,4,5,6,7)
    assert(algo.customFilter(lst, isOdd).equals(List(2,4,6)))
  }

  test("Returns sum of all elements in collection according input function"){
    val lst = List(1,2,3,4,5)
    val set = Set(1,2,3,4,6)
    val seq = Seq(1,2,3,4,7)

    val funSum = (coll: Iterable[Int]) => coll.sum

    assert(algo.sumColl(lst, funSum) == 15)
    assert(algo.sumColl(set, funSum) == 16)
    assert(algo.sumColl(seq, funSum) == 17)
  }

  test("Returns list of Armstrong numbers from interval [a; b]"){
    val a = 0
    val b = 400
    assert(algo.findArmstrongNums(a, b).equals(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 153, 370, 371)))
  }

  test("Replaces in input list even elements by 1 and odds by 0 "){
    assert(algo.handleListWithZeroOne(List(1,2,3,4,5,6,7,8,9,10)).equals(List(1,0,1,0,1,0,1,0,1,0)))
  }

  test("Returns absolute value of input number"){
    assert(algo.abs(47) == 47)
    assert(algo.abs(-47) == 47)
  }

  test("Finds NOK of input numbers"){
    assert(algo.lcm(12,18,36) == 36)
    assert(algo.lcm(3,6,4) == 12)
    assert(algo.lcm(2,4,8) == 8)
    assert(algo.lcm(32,16,256,512) == 512)
  }

  test("Finds max value in list"){
    assert(algo.max(List(1,2,3,1,7,9,4,47)) == 47)
    assert(algo.max(List(1,-444,3,1,7,9,4,4)) == 9)
    assert(algo.max(Nil) == Int.MinValue)
  }

  test("Takes elements from collections until input function returns true"){
    val f = (a: Int) => a > 3
    assert(algo.takeWhile(List[Int](4,7,47,44,1,7,99), f) == List(4,7,47,44))
  }

  test("Returns last element of list"){
    assert(algo.last(List(1,2,3,4,5,6)) == 6)
  }

  test("Returns new list according input function"){
    val f = (a: Double) => a.round
    assert(algo.mapViaFor(List(2.2, 3.4, 3.5, 4.6), f) == List(2, 3, 4, 5))
  }
}
