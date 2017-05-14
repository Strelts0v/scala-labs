package com.gv.lab.utils

import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.collection._

class Algorithm {

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

  // Найти сумму всех элементов коллекции. Используя функции высших порядков
  def sumColl(coll: Iterable[Int], f: (Iterable[Int] => Int)): Int = {
    f(coll)
  }

  // Подсчитать количество совпадающих символов, стоящих на одинаковых позициях
  // относительно первого элемента, в двух коллекциях.
  def countSimilarElementsOnTheSamePositions[T](l1: List[T], l2: List[T]): Int = {
    l1.zip(l2).toMap.count({case(k,v) => k == v})
  }
}