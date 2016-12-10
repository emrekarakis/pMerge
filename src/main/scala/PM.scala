
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random


/**
 * Created by emrekarakis on 16/12/15.
 */
object PM {
  def main(args: Array[String]) {
    val list: List[Int]= Random.shuffle((1 to 10).toList)
    println( s"initalList: $list")

    val futureResult: Future[List[Int]]= mergeSort(list)

    val finalResult: List[Int]= Await.result( futureResult, Duration(5,"seconds"))
    println( s"finalResult= $finalResult")

  }



  def mergeUpper( futureOfList1: Future[List[Int]], futureOfList2: Future[List[Int]]): Future[List[Int]] = {
    val returnList: Future[List[Int]] = for {
      a: List[Int] <- futureOfList1
      b: List[Int] <- futureOfList2

    } yield {
        merge(a, b)
      }
    val returnFuture: Future[List[Int]] = returnList.recover {
      case e: Exception => {
          println("Unexpected Condition " ,e)
        List.empty
      }

    }

    returnFuture

  }

  def merge(firstList: List[Int], secondList: List[Int]): List[Int] = {
    println(s"firstList: $firstList, secondList: $secondList")
    //println(s"in method merge, xs: $xs, ys: $ys")
    (firstList, secondList) match {
      case (Nil, _) => {
        secondList
      }
      case (_, Nil) => {
        firstList
      }
      case (x :: tailOfFirstList, y :: tailOfSecondList) => {

        try{
             if (x < y) x :: merge(tailOfFirstList, secondList)
             else y :: merge(firstList, tailOfSecondList)
        }catch{
              case e: Exception =>{
              println("unexpected condition",e)
              }
            List.empty[Int]
         }

       }

    }
  }


  
  def mergeSort( list:List[Int]): Future[List[Int]]={
    if(list.length <= 1){
      Future.successful(list)
    }else{
      val (leftList:List[Int],rightList:List[Int])=list.splitAt(list.length/2)
      mergeUpper(mergeSort(leftList),mergeSort((rightList)))
    }

    
  }
  
}
