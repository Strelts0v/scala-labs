package com.gv.lab.utils

import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.collection._

class Algorithm {

  // lab 5

  // Разделить все чётные числа на два, а к нечётным прибавить один.
  def handleArray(arr: Array[Int]): Array[Int] = {
    // 1st variant
    arr.map((item: Int) => { if (item % 2 == 0) item / 2 else item + 1})

    // 2nd variant
//    for(elem <- arr) yield {
//      if(elem % 2 == 0) elem / 2
//      else elem + 1
//    }
  }

  // Разделить все чётные числа на два, а к нечётным прибавить один.
  def handleList(lst: List[Int]): List[Int] = {
    lst.map((item: Int) => if (item % 2 == 0) item / 2 else item + 1)
  }

  // Разделить все чётные числа на два, а к нечётным прибавить один.
  def handleArrayTailRec(arr: Array[Int]): Array[Int] = {
    def handleHelper(origin: Array[Int], temp: ArrayBuffer[Int], next: Int) : Array[Int] = {
        if(next == origin.length) temp.toArray
        else {
          temp.append( if(origin(next) % 2 == 0) origin(next) / 2
                       else origin(next) + 1 )
          handleHelper(origin, temp, next + 1)
        }
    }
    handleHelper(arr, new ArrayBuffer, 0)
  }

  // Разделить все чётные числа на два, а к нечётным прибавить один.
  def handleListRec(lst: List[Int]): List[Int] = lst match {
    case Nil => Nil
    case h :: t => if(h % 2 == 0)  List(h / 2) ++ handleListRec(t) else List(h + 1) ++ handleListRec(t)
  }

  // Разделить все чётные числа на два, а к нечётным прибавить один.
  def handleListTailRec(lst: List[Int]): List[Int] = {
    def handleHelper(lst: List[Int], result: List[Int]): List[Int] = lst match {
      case Nil => result
      case h :: t => if(h % 2 == 0) handleHelper(t, result ++ List[Int](h / 2)) else handleHelper(t, result ++ List(h + 1))
    }
    handleHelper(lst, List[Int]())
  }

  // Объединить 2 списка попарно сложив их элементы
  def sumLists(lst1: List[Int], lst2: List[Int]): List[Int] = {
    if (lst1.isEmpty) Nil
    else List(lst1.head + lst2.head) ++ sumLists(lst1.tail, lst2.tail)
  }

  // Найти все простые числа на интервале [m; n]
  def simpleNumbers(m: Int, n: Int) : List[Int] = {
    if (m > n) Nil
    else if (m != 1 && (2 to sqrt(m).toInt).forall(m % _ != 0))
      m :: simpleNumbers(m + 1, n)
    else
      simpleNumbers(m + 1, n)
  }

  // Найти факториал числа n
  def factorial(n: Int): BigInt = {
    if (n == 0 || n == 1) 1
    else n * factorial(n - 1)
  }

  // Найти факториал числа n
  def factorialTailRec(n: Int): BigInt = {
    def calculate(num: Int, result: BigInt): BigInt ={
      if(num == 0 || num == 1) result
      else calculate(num - 1, result * num)
    }
    calculate(n, 1)
  }

  // По списку вернуть список пар (элемент списка, количество вхождений)
  def getCountOccurrences[T](lst: List[T]) : Map[T, Int] = lst match {
    case Nil => Map()
    case h :: t => Map[T, Int](h -> lst.count((item: T) => item == h)) ++ getCountOccurrences(t.filter((elem: T) => elem != h))
  }

  // Транспонировать матрицу рекурсивно
  def transpose[T](matrix: List[List[T]]): List[List[T]] = matrix.filter(_.nonEmpty) match{
    case Nil => Nil
    case (ys: List[List[T]]) => ys.map(_.head)::transpose(ys.map(_.tail))
  }

  // Удалить число из List (матчинг)
  def deleteElement(lst: List[Int], num: Int): List[Int] = lst match {
    case Nil => lst
    case h :: t => if(h == num) deleteElement(t, num) else h :: deleteElement(t, num)
    //case _ => lst.filter((elem: Int) => elem != num)
  }

  // Из двух коллекций сделать одну, в которой будут только общие для двух коллекций элементы.
  def intersectWithSets[T](s1: Set[T], s2: Set[T]): Set[T] = {
    s1 & s2
  }

  // Из двух коллекций сделать одну, в которой будут только общие для двух коллекций элементы.
  def intersectWithLists[T](l1: List[T], l2: List[T]): List[T] = {
    l1.intersect(l2)
  }

  // Найти среднее арифметическое для всех элементов коллекции.
  def average(lst: List[Int]) : Double = {
    lst.sum.toDouble / lst.length.toDouble
  }

  // Из двух несортированных коллекций сделать одну сортированную.
  def uniteAndSort(l1: List[Int], l2: List[Int]) : List[Int] = {
    (l1 ++ l2).sortWith((_ < _))
  }

  // Разделить коллекцию на две. В первой должны быть только положительные элементы
  // а во второй только отрицательные.
  def divide(lst: List[Int]): (List[Int], List[Int]) = {
    lst.partition(_ < 0)
  }

  // Число красивое, если сумма его цифр делится на кол-во его цифр. Вывести
  // красивые числа на интервале [m,n]
  def beautyNumbers(m: Int, n: Int) : List[Int] = {
    if(m > n) Nil
    else if(m.toString.toList.sum % m.toString.length == 0) m :: beautyNumbers(m + 1, n)
    else beautyNumbers(m + 1, n)
  }

  // Найти НОД для трёх чисел (для n - также не трудно)
  def GCD(a: Int, b: Int, c: Int): Int = {
    def find(min: Int): Int = {
      if((a % min == 0) && (b % min == 0) && (c % min == 0)) min
      else find(min - 1)
    }
    find(List(a,b,c).min)
  }

  // Продублировать все элементы в списке
  def duplicate[T](lst: List[T]): List[T] = {
    lst.flatMap(item => List[T](item, item))
  }

  // Продублировать все элементы в списке
  def duplicateTailRec[T](lst: List[T]): List[T] = {
      lst.foldRight(List[T]()) {(elem, acc) => elem::elem::acc}
  }

  // lab 6

  // В коллекции хранится информация о футболистах и их голах. Создать две коллекции.
  // В одной отсортировать футболистов по голам, а в другой в алфавитном порядке.
  def divideAndSort(players: Map[String, Int]): (List[String], List[Int]) = {
    val names = players.keys.toList.sortWith(_ < _)
    val scores = players.values.toList.sortWith(_ > _)
    (List[String]() ++ names, List[Int]() ++ scores)
  }

  // Найти сумму всех элементов коллекции.
  def sumCollection(coll: Iterable[Int]): Int = {
    coll.sum
  }

  // Подсчитать количество совпадающих символов, стоящих на одинаковых позициях
  // относительно первого элемента, в двух коллекциях.
  def countSimilarElementsOnTheSamePositions[T](l1: List[T], l2: List[T]): Int = {
    l1.zip(l2).toMap.count({case(k,v) => k == v})
  }

  // Удалить в первом списке все чётные элементы, во втором все нечётные и объединить их
  def deleteOddEven(l1: List[Int], l2: List[Int]): List[Int] = {
    l1.filter((e: Int) => e % 2 == 1) ++ l2.filter((e: Int) => e % 2 == 0)
  }

  // Сортировка списка методом quicksort
  def quicksort(lst: List[Int]): List[Int] = lst match {
    case head :: tail => {
      val left = tail.filter(_ < head)
      val right = tail.filter(_ >= head)
      quicksort(left) ++ List(head) ++ quicksort(right) }
    case Nil => Nil
  }

  // Проверить гипотезу Коллатца
  def checkCollatsHypothesis(num: Int): Boolean = num match{
    case 1 => true
    case _ => if(num % 2 == 0) checkCollatsHypothesis(num / 2) else checkCollatsHypothesis(num * 3 + 1)
  }

  def customFilter(lst: List[Int], f: Int => Boolean): List[Int] = {
    lst.filter((item: Int) => f(item))
  }

  // Найти сумму всех элементов коллекции. Используя функции высших порядков
  def sumColl(coll: Iterable[Int], f: (Iterable[Int] => Int)): Int = {
    f(coll)
  }

  // вывести все чётные элементы списка
  def printOddItems(lst: List[Int]): Unit = {
    lst.foreach((item: Int) => if(item % 2 == 0) print(item + " "))
  }

  // реализовать функцию нахождения чисел Армстронга
  def findArmstrongNums(a: Int, b: Int): List[Int] = {
    if (a > b) Nil
    else {
      if (a == a.toString.toList.map(_.asDigit).map(pow(_, a.toString.toList.size).toInt).sum)
        a :: findArmstrongNums(a + 1, b)
      else findArmstrongNums(a + 1, b)
    }
  }

  // lab 7

  // Все элементы, стоящие на чётной позиции, заменить на нули, а стоящие на нечётной,
  // на единицы
  def handleListWithZeroOne(lst: List[Int]) : List[Int] = {
    def handlerHelper(lst: List[Int], next: Int): List[Int] = lst match{
      case Nil => Nil
      case h :: t => if(next == 0) 0 :: handlerHelper(t, 1) else 1 :: handlerHelper(t, 0)
    }
    handlerHelper(lst, 1)
  }

  // Найти модуль числа
  def abs(n: Int): Int = {
    if (n > 0 ) n else -n
  }

  // НОК чисел используя функцию с переменным числом параметров (хвостовая рекурсия)
  def lcm(args: Int*): Int = {

    def gcd(n: Int, m: Int): Int = {
      def find(min: Int): Int = {
        if ((n % min == 0) && (m % min == 0)) min
        else find(min - 1)
      }
      find(min(n, m))
    }

        def lcmAlgorithm(a: Int, b: Int): Int = {
          abs(a * b) / gcd(a, b)
        }

    def helper(lst: List[Int]): Int = {
      if(lst.length == 2) lcmAlgorithm(lst(0), lst(1))
      else lcmAlgorithm(lst(0), helper(lst.tail))
    }

    helper(args.toList)
  }

  def max(lst: List[Int], currentMax: Int = Int.MinValue): Int = lst match {
    case Nil => currentMax
    case h :: t => if(h > currentMax) max(t, h) else max(t, currentMax)
  }

  def takeWhile[T](lst: List[T], p: T => Boolean): List[T] = {
    lst.foldRight(List[T]())((a, b) => if(p(a)) a :: b else List.empty)
  }

  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("List is empty")
    //case Nil => throw new Error("List is empty")
    case List(x) => x
    case y :: ys => last(ys)
  }

  def mapViaFor[T, U](xs: List[T], f: T => U): List[U] = {
    val arr = new ArrayBuffer[U]
    for(e <- xs){
      arr.append(f(e))
    }
    arr.toList
  }
}

