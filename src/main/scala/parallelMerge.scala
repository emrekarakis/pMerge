import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
 * Created by emrekarakis on 16/12/15.
 */
object parallelMerge {

  def main(args: Array[String]) {
    //val firstlist = List(98, 89, 456, 546, 0, 36, 90, 23, 21, 45, 89, 2, 6, 9, 22, 0, 42, 54, 32, 26, 78, 904, 20, 123, 54, 7657, 23425, 35, 67, 16, 89, 706, 33, 8, 10, 19, 70, 53, 2, 11, 19, 40)
    //val secondlist=List(5000,67483,38745023,463,2145,78854,1001,2374,76,352,101,909,784,6230,2342,33,685,4225,464,25345,57)


    val testList: List[Int] = Random.shuffle((1 to 10).toList)
    println(s"initial list: $testList")
    val futureResult: Future[List[Int]] = mergeSort(testList)

    val finalResult: List[Int] = Await.result(futureResult, Duration(5, "seconds"))
    println(s"finalResult $finalResult")

  }

  def mergeUpper(futureOfList1: Future[List[Int]], futureOfList2: Future[List[Int]]): Future[List[Int]] = {

       val returnList: Future[List[Int]] = for {
         a <- futureOfList1
         b <- futureOfList2

       } yield{merge(a,b)}

      returnList

  }
  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    println(s"in method merge, xs: $xs, ys: $ys")
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (x < y) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)
    }
  }

  def mergeSort(list: List[Int]): Future[List[Int]] = {
    println(s"in method mergeSort, list: $list")
    if (list.size == 1) {
      Future {
        list
      }
    } else {
      val (leftList: List[Int], rightList: List[Int]) = list.splitAt(list.length / 2)
      mergeUpper(mergeSort(leftList), mergeSort(rightList))
    }
  }


}


/*

  def msort[T](less: (T, T) => Boolean) (xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs))
    }
  }*/

/*
  def mergesort(list: List[Int], left: Int, right: Int): Future[List[Int]] = {

    val sortedPart: Future[List[Int]] = Future {
      if (left < right) {
        var middle: Double = Math.floor((left + right) / 2)
        mergesort(list, left, middle.toInt)
        mergesort(list, (middle + 1).toInt, right)
        merge(list, left, middle.toInt, right)

      }
      list
    }
    sortedPart

  }
}
def merge(list: List[Int], left: Int, middle: Int, right: Int): Future[List[Int]] = {

  val futureValue: Future[List[Int]] = Future {

    val arr: Array[Int] = list.toArray //same size of a[]
    var temp: Array[Int] = Array.empty[Int]

    var k = 0
    var i = left
    var j = middle + 1
    while (i <= middle && j <= right) {
      if (arr(i) < arr(j)) {
        temp(k) = arr(i)
        k = k + 1
        i = i + 1 // same as b[k]=a[i]; k++; i++;
      }
      else {
        temp(k) = arr(j)
        k = k + 1
        j = j + 1
      }
    }

    while (i <= middle) {
      temp(k) = arr(i)
      k = k + 1
      i = i + 1
    }

    while (j <= right) {
      temp(k) = arr(j)
      k = k + 1
      j = j + 1
    }

    while (i >= left) {
      arr(i) = temp(k)
      k = k - 1
      i = i - 1 // copying back the sorted list to a[]
    }

    arr.toList

  }
  futureValue

}*/