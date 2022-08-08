import java.awt.font.TransformAttribute
import scala.io.Source

object main extends App {

  //Define a case class Transaction which represents a transaction
  case class Transaction( transactionId: String, accountId:
  String, transactionDay: Int, category: String,
                          transactionAmount: Double

                        //CONSIDER IMPLENTING AN EQUALS METHOD FOR COMPARISON BY DAY AND NAME???

                        )

  //The full path to the file to import
  val fileName = "src/main/scala/transactions.txt"
  //val fileName = "src/main/scala/testtransactions.txt"
  //The lines of the CSV file (dropping the first to remove the header)
  val transactionslines = Source.fromFile(fileName).getLines().drop(1)
  //Here we split each line up by commas and construct Transactions
  val transactions: List[Transaction] = transactionslines.map { line =>
    val split = line.split(',')
    Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
  }.toList

  //transactionId String representing the id of a transaction
  //accountId String representing the id of the account which made the transaction
  //transactionDay Integer representing the day the transaction was made on
  //category String representing the type of category of the transaction
  //transactionAmount A double representing the value of the transaction

   def calculateTotal(day : List[Transaction]):Double= {
      day.foldLeft(0.0)((sum, transaction) =>
        round(sum + transaction.transactionAmount)
      )
  }

   def calculateAverage(transactions : List[Transaction]) : Double = {
     round(calculateTotal(transactions) / transactions.length) //round to avoid weird trailing
   }

  def round(d : Double): Double = {
    Math.round(d * 100.0) / 100.0
  }

  def orderTransactionsByDay(tList : List[Transaction]): List[(Int, List[Transaction])] ={
    tList.groupBy( transaction =>
      transaction.transactionDay
    ).toList.sortWith((x,y)  => x._1 < y._1)
  }

  def orderTransactionsByAcc(tList : List[Transaction]): List[(String, List[Transaction])] ={
    tList.groupBy( transaction =>
      transaction.accountId
    ).toList.sortWith((x,y)  => alphaNumericCompare(x._1,y._1))
  }

  def alphaNumericCompare(x: String, y: String): Boolean ={
    //compare by letters first, then compare by the number values
    val pattern = "([A-Za-z]+)([0-9]+)".r
    val pattern(alphX, numX) = x
    val pattern(alphY, numY) = y
    if(alphX == alphY){numX.toInt < numY.toInt} else {alphX < alphY}
  }

  def calculateAverageTransactionsPerCategory(tList : List[Transaction], category: String): Double ={
      val catTransaction = tList.filter(t => t.category == category)
      calculateAverage(catTransaction)
  }

  def calculateTotalTransactionsPerCategory(tList : List[Transaction], category: String): Double ={
    val catTransaction = tList.filter(t => t.category == category)
    calculateTotal(catTransaction)
  }

  def getAveragesPerCategory(tList : List[Transaction], categories: List[String]): List[Double] = {
    categories.map(cat => calculateAverageTransactionsPerCategory(tList,cat)).toList // I could maybe do something with case matching on t.categoryid to to return them all in a list in o(n) instead of running seven times, but that's to look at later
  }

  def getTotalsPerCategory(tList : List[Transaction], categories: List[String]): List[Double] = {
    categories.map(cat => calculateTotalTransactionsPerCategory(tList, cat)).toList // I could maybe do something with case matching on t.categoryid to to return them all in a list in o(n) instead of running seven times, but that's to look at later
  }

  def getAccTransactionStatistics(tList : List[(String,List[Transaction])], categories: List[String] ) = List {
    tList.map{ acc =>

      val accId = acc._1
      val acc5DayTransactions = acc._2

      val perCategoryResults = getTotalsPerCategory(acc5DayTransactions,categories)
      val max = acc5DayTransactions.map(_.transactionAmount).max
      val average = calculateAverage(acc5DayTransactions)

      val resultOutput: List[Double] = List(max,average)++perCategoryResults
      List(accId)++resultOutput //List[Any] is ehhhhh, tuple would be better for real-life use?
    }
  }

  def getLastFiveDaysOrLess(tList : List[(Int,List[Transaction])], currentDay: Int) : List[Transaction] = {
    val lookback = if(currentDay < 5) currentDay else 5
    val startDay = currentDay - lookback
    val range = startDay to (currentDay-1) // 1 to 0 maybe a bit sketchy, but seems to work here.
    tList.filter(x => range.contains(x._1)).map(x => x._2).flatten
  }

  def printRollingWindowStatistics(tList : List[(Int,List[Transaction])]) : Unit = {

    val categories = List("AA","CC","FF")

    tList.foreach { day =>
      val currentDay: Int = day._1.toInt
      val currentTransacationsWindow: List[Transaction] = getLastFiveDaysOrLess(tList, currentDay)
      //println("Day: " + currentDay + " " + currentTransacationsWindow.map(x => x.transactionDay).toSet) // correct days window debug

      val accOrdered = orderTransactionsByAcc(currentTransacationsWindow)
      val accResults = getAccTransactionStatistics(accOrdered,categories) //probably a better way to flatten it without going through .head below

      if(accResults.head.nonEmpty) accResults.head.foreach( row =>
        println(s"| Day: $currentDay | Acc: ${row(0)} | Max: ${row(1)} | Avg: ${row(2)} | AA Tot: ${row(3)} | CC Tot: ${row(4)} | FF Tot: ${row(5)} | "))
      else
        println(s"| Day: $currentDay | NO RESULTS FOR ANY ACCOUNT")
    }

  }

def printTransactionsPerAcc(tList : List[(String,List[Transaction])]): Unit = {

  tList.foreach(u =>
    val acc : String = u._1
    val transactions:List[Transaction] = u._2
    val categories = List("AA","BB","CC","DD","EE","FF","GG")
    val results: List[Double] = getAveragesPerCategory(transactions,categories)
    println(s"| Acc: $acc | AA Avg: ${results(0)} | BB Avg: ${results(1)} | CC Avg: ${results(2)} | DD Avg: ${results(3)} | EE Avg: ${results(4)} | FF Avg: ${results(5)} | GG Avg: ${results(6)} | ") //messy?
  )
}

  def printTotalsPerDay(tList : List[(Int,List[Transaction])]): Unit = {
    tList.foreach(day =>
      val date = day._1
      val total = calculateTotal(day._2)
        println(s"| Day: $date | Tot: £$total |")
    )
  }

  //val test = transactions.sortBy(t => (t.transactionDay, t.transactionId))

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  //Question 1
  //Calculate the total transaction value for all transactions for each day.
  //The output should contain one line for each day and each line should include the day and the total value


  println("///////////////////////////////////////////////////////////////////////////////////////////////////////////")
  val transactionsPerDay: List[(Int,List[Transaction])] = orderTransactionsByDay(transactions)
  printTotalsPerDay(transactionsPerDay)
  println("///////////////////////////////////////////////////////////////////////////////////////////////////////////")

  //Question 2
  //Calculate the average value of transactions per account for each type of transaction (there are seven in total).
  //The output should contain one line per account, each line should include the account id and the average
  //value for each transaction type (ie 7 fields containing the average values).


  println("///////////////////////////////////////////////////////////////////////////////////////////////////////////")
  val transactionsPerAcc: List[(String,List[Transaction])] = orderTransactionsByAcc(transactions)
  printTransactionsPerAcc(transactionsPerAcc)
  println("///////////////////////////////////////////////////////////////////////////////////////////////////////////")


  //Question 3

  //For each day, calculate statistics for each account number for the previous five days of transactions, not
  //including transactions from the day statistics are being calculated for. For example, on day 10 you should
  //consider only the transactions from days 5 to 9 (this is called a rolling time window of five days). The
  //statistics we require to be calculated are:
  //• The maximum transaction value in the previous 5 days of transactions per account
  //• The average transaction value of the previous 5 days of transactions per account
  //• The total transaction value of transactions types “AA”, “CC” and “FF” in the previous 5 days per account

  println("///////////////////////////////////////////////////////////////////////////////////////////////////////////")
  val transactionsPerDayQ3: List[(Int, List[Transaction])] = orderTransactionsByDay(transactions)
  printRollingWindowStatistics(transactionsPerDayQ3)
  println("///////////////////////////////////////////////////////////////////////////////////////////////////////////")



}
