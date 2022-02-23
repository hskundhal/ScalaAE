
package koans
import koans.support.KoanSuite
import org.scalatest.{Matchers, SeveredStackTraces}

import java.io.{BufferedReader, FileReader}
import java.net.{MalformedURLException, URL}
import scala.io.Source

class TestCases extends KoanSuite with Matchers with SeveredStackTraces {

  test ("Parameterize arrays with type") {
    val greetStrings = new Array[String](3)
    greetStrings(0) = "Hello"
    greetStrings(1) = ", "
    greetStrings(2) = "World"

    // what happens if you replace the above line with:
    // greetStrings(2) = 2

    // join the strings together
    val concat = greetStrings(0) + greetStrings(1) + greetStrings(2)

    // and what should this equal?
    concat should be ("Hello, World")
  }

  test ("Array creation") {
    // Calling Array() directly invokes the apply method - the following two statements are equivalent
    val numNames = Array("zero", "one", "two")
    val numNames2 = Array.apply("zero", "one", "two")

    numNames.length should be (3)
    numNames2.length should be (3)

    (numNames == numNames2) should be (false)    
    // Why? Because the underlying implementation for Array is Java and uses Java's .equals method
    // for arrays - if you want to test array equality in scala, you can use
    (numNames.deep == numNames2.deep) should be (true)
  }

  test ("List immutability") {
    val oneTwo = List(1,2)
    val threeFour = List(3,4)
    
    val oneTwoThreeFour = oneTwo ::: threeFour
    
    oneTwo should be (List(1, 2))
    threeFour should be (List(3, 4))
    oneTwoThreeFour should be (List(1, 2, 3, 4))
  }

  test ("List cons") {
    val twoThree = List(2,3)
    val newList = 1 :: twoThree
    val newList2 = twoThree.::(1)

    newList should be (List(1, 2, 3))
    newList2 should be (List(1, 2, 3))
  }

  test ("Create a list and convert to Array") {
    // fill in the method below to concatenate the two lists and then convert them to an array
    // so that it satisfies the tests below
    //

    def concatListsToArray(l1 : List[Int], l2 : List[Int]) : Array[Int] = {
      // replace this with the correct implementation
      (l1 ++ l2).toArray  // or you could use :::
    }

    val oneTwo = List(1,2)
    val threeFour = List(3,4)

    concatListsToArray(oneTwo, threeFour) should equal (Array(1,2,3,4))
  }

  test ("Take two arrays, and concatenate them in a list") {
    def concatArraysToList(a1 : Array[Int], a2 : Array[Int]) : List[Int] = {
      // replace this with the correct implementation
      (a1 ++ a2).toList
    }

    val oneTwo = Array(1,2)
    val threeFour = Array(3,4)

    concatArraysToList(oneTwo, threeFour) should equal (List(1,2,3,4))
  }

  test ("Exploring Tuples") {
    val t = (0, 'u', 8, 1, "too")

    t._1 should be (0)
    t._2 should be ('u')
    t._3 should be (8)
    t._4 should be (1)
    t._5 should be ("too")

    // Arity is the number of arguments
    t.productArity should be (5)

    // and you can iterate over the arguments too
    t.productIterator.next should be (0)
  }

  test ("Map a tuple to strings") {
    val t = (0, 'u', 8, 1, "too")

    // replace the following with the correct code to convert tuple t
    // to a list of strings
    val l = List(t._1.toString, t._2.toString, t._3.toString, t._4.toString, t._5.toString)  // better ways to do this
    // but we will cover those later.

    l should be (List("0", "u", "8", "1", "too"))
  }

  test ("Immutable set in var") {
    var getSet = Set("Ready", "Steady")

    // Add a line below to satisfy the test
    getSet += "Go!"

    getSet should be (Set("Ready", "Steady", "Go!"))
    
    // What happens if you make the var a val above? Why?
  }

  test ("Mutable set in a val") {
    var getSet = scala.collection.mutable.Set("Ready", "Steady")

    // Add a line below to satisfy the test
    getSet += "Go!"

    getSet should be (Set("Ready", "Steady", "Go!"))

    // what happens if you make the var a val above? Why? Is this a good idea?
  }

  test ("Immutable map in a var") {
    var mutMap = Map[Int, String]()

    mutMap += (1 -> "Uno")
    mutMap += (2 -> "Dos")
    mutMap += (3 -> "Tres")

    mutMap(2) should be ("Dos")

    mutMap += (2 -> "Two")

    mutMap(2) should be ("Two")

    // What happens if you uncomment the line below? Why?
//    mutMap += (2 -> 2)
    mutMap(2) should be ("Two")
  }

  test ("Mutable map in a val") {
    val mutMap = scala.collection.mutable.Map[Int, String]()

    mutMap += (1 -> "Uno")
    mutMap += (2 -> "Dos")
    mutMap += (3 -> "Tres")

    mutMap(2) should be ("Dos")

    mutMap(2) = "Two"

    mutMap(2) should be ("Two")

    mutMap += (2 -> "Deux")

    mutMap(2) should be ("Deux")

    // What happens if you uncomment the line below? Why?
//     mutMap += (2 -> 2)
    mutMap(2) should be ("Deux")
  }

  class ComplexNum(r: Double, i: Double) {
    val real: Double = r
    val imaginary: Double = i
    def this(r: Double) = this(r, 0.0)

    override def toString(): String = "" + real +
      (if (imaginary < 0.0) " - " else " + ") + math.abs(imaginary) + "i"

    def +(other: ComplexNum): ComplexNum = new ComplexNum(real + other.real, imaginary + other.imaginary)
    def +(other: Double): ComplexNum = new ComplexNum(real + other, imaginary)
  }

  // UNCOMMENT BELOW
  test ("Create a new Complex number and check the values for the real/imaginary parts") {
    val complex = new ComplexNum(4, 2)

    complex.real should be (4)
    complex.imaginary should be (2)
  }

  test ("Create a new complex number with double values and check those values") {
    val complex = new ComplexNum(6.2, -1.5)

    complex.real should be (6.2)
    complex.imaginary should be (-1.5)
  }

  test ("Create a complex number from a real number, imaginary part should be 0") {
    val complex = new ComplexNum(-3.2)

    complex.real should be (-3.2)
    complex.imaginary should be (0)
  }

  test ("Complex number should be printed in the form R.R + I.Ii") {
    val complex = new ComplexNum(6, 3)
    val complex2 = new ComplexNum(5.4, 7.8)

    complex.toString should be ("6.0 + 3.0i")
    complex2.toString should be ("5.4 + 7.8i")
  }

  test ("Adding complex numbers") {
    val complex = new ComplexNum(6, 3)
    val complex2 = new ComplexNum(5.4, 7.8)
    val complex3 = complex + complex2

    complex3.real should be (11.4)
    complex3.imaginary should be (10.8)
  }

  test ("Adding complex number to a double") {
    val complex = new ComplexNum(6.5, 3.2) + 5.5

    complex.real should be (12)
    complex.imaginary should be (3.2)
  }

  // Extra credit - numbers with a negative imaginary part should be output
  // as 6.0 - 5.0i instead of 6.0 + -5.0i - if you have time, write a new test
  // for this outcome, and then adapt the toString in the class to work correctly
  // Hint: scala.math.abs will give the absolute value of a double

  // UNCOMMENT BELOW
  test ("Format for negative imaginary part should be R.R - I.Ii") {
    val complex = new ComplexNum(5, -6)
    val complex2 = new ComplexNum(5.5, -6.6)

    complex.toString should be ("5.0 - 6.0i")
    complex2.toString should be ("5.5 - 6.6i")
  }

  // Extra extra credit - adding a double to a complex works, but adding a complex
  // to a double does not. If you add an implicit conversion you can make this work
  // / if you have time, write a test, and add an implicit to make it work

  implicit def doubleToComplex(d: Double): ComplexNum = new ComplexNum(d)

  // UNCOMMENT BELOW
  test ("Add a complex to a double") {
    val complex = 5.6 + new ComplexNum(3.4, 4.5)

    complex.real should be (9)
    complex.imaginary should be (4.5)
  }


  test ("Refactoring var to val") {

    // this method passes, but it's not very "scala-y", see if you can rid it of the need
    // to use a var but still pass the tests
    //
    // Hint: maybe recursion? List.head will give you the first value, List.tail will give you the rest

    def max(numbers : List[Int]): Int = {
      if (numbers.isEmpty) Int.MinValue
      else {
        val maxOfRest = max(numbers.tail)
        if (numbers.head > maxOfRest) numbers.head else maxOfRest
      }
    }

    max(List(1, 2, 3, 4, 5)) should be (5)
    max(List(5, 4, 3, 2, 1)) should be (5)
    max(List(-5, -1, -3)) should be (-1)
    max(Nil) should be (Int.MinValue)
  }


  // there are files in the "." directory that have the extension .shkspr, complete the function below
  // using a for to filter out all files with the .shkspr extension and return a list of their filenames
  // so that the tests below are satisfied
  def listShakespeareFiles(dirPath: String) : Array[String] = {
    val fileList = (new java.io.File(dirPath)).listFiles

    // you need to replace this line with the real implementation
    for (file <- fileList if file.getName.endsWith(".shkspr")) yield file.getName
  }
  // question: can you guess why we define this method outside of the test below?

  test ("List Shakespeare files in the given directory") {
    val shakespeareFilenames = listShakespeareFiles(".").toList

    shakespeareFilenames.length should be (3)
    shakespeareFilenames should contain theSameElementsAs (List("caesar.shkspr", "hamlet.shkspr", "romeo.shkspr"))
  }

  // complete the method definition below to open a file, read the first line, close the file
  // and return the first line that was read. Make sure that the file is always closed, even
  // if there is an exception. new BufferedReader(new FileReader(filePath)) should give you
  // what you need
  def firstLineOfFile(filePath: String) : String = {
    // replace with the real implementation
    Source.fromFile(filePath).getLines.next
  }

  test ("First line of file") {
    firstLineOfFile("caesar.shkspr") should be ("Beware the ides of March.")
    firstLineOfFile("romeo.shkspr") should be ("O Romeo, Romeo, wherefore art thou Romeo?")
    firstLineOfFile("hamlet.shkspr") should be ("The lady doth protest too much, methinks.")

    intercept[java.io.FileNotFoundException] { // nice, huh?
      firstLineOfFile("macbeth.shkspr") should be ("")
    }
  }

  // finish this method so that the string is first trimmed (get rid of leading and trailing space)
  // and then the last character is matched to either a '?' ("Question") or anything else ("Statement")
  // so that the test below passes
  def statementOrQuestion(str : String) : String =
    if (str.trim.endsWith("?")) "Question" else "Statement"

  test ("statement or question?") {
    statementOrQuestion("hello") should be ("Statement")
    statementOrQuestion("hello.") should be ("Statement")
    statementOrQuestion("hello. ") should be ("Statement")
    statementOrQuestion("  hello?") should be ("Question")
    statementOrQuestion("hello? ") should be ("Question")
  }

  // extra credit
  test ("shakespeare files contain question or statement?") {
    // combine the methods you have created here to define and test a method that creates a map of
    // the shakespeare quotation file names in the working directory (".") as the key, and whether
    // they contain a statement or question, for example:
    // "caesar.shkspr" -> "Statement"
    // "romeo.shkspr" -> "Question"
    // "hamlet.shkspr" -> "Statement"
    // hint - consider using a scala.collection.immutable.HashMap[String, String] and a var
    // or a scala.collection.mutable.HashMap[String, String] and a val
    // can you add a test for a key of "macbeth.shkspr" that ensures a java.util.NoSuchElementException?
    // extra extra credit - can you find a way to do it without using either a var or a mutable Map?

    val shksprMap = {
      for {
        file <- listShakespeareFiles(".")
        which = statementOrQuestion(firstLineOfFile(file))
      } yield file -> which
    }.toMap

    shksprMap("caesar.shkspr") should be ("Statement")
    shksprMap("romeo.shkspr") should be ("Question")
    shksprMap("hamlet.shkspr") should be ("Statement")

    intercept[java.util.NoSuchElementException] {
      shksprMap("macbeth.shkspr") should be ("")
    }
  }

  test ("Multiply numbers") {
    var multiplier = 3

    // Uncomment the tests below, then write a local function, mult, to satisfy the tests below.
    // Remember that local functions can access variables from the method space without the
    // need to pass them in. The function should multiply the argument passed in by the multiplier var

    def mult(x: Int): Int = x * multiplier

    // UNCOMMENT TESTS
    mult(5) should be (15)
    mult(3) should be (9)

    // why do the results of mult change here?
    multiplier = 5
    mult(5) should be (25)
    mult(3) should be (15)
  }

  test ("Filter numbers") {
    val allNumbers = List(0,1,2,3,4,5,6,7)

    // alter the two filters below to filter only odd and even numbers out of the list respectively,
    // to make the tests pass.
    // Just like in Java, % is the modulo operator

    val onlyOdd = allNumbers.filter(x => x % 2 != 0)
    val onlyEven = allNumbers.filter(x => x % 2 == 0)

    onlyOdd should be (List(1,3,5,7))
    onlyEven should be (List(0,2,4,6))
  }

  test ("Function with placeholder syntax") {
    // using placeholder syntax, define a val "mult" that multiplies 2 Ints together, then uncomment
    // the tests below and make sure they pass

    val mult = (_: Int) * (_: Int)

    mult(2, 4) should be (8)
    mult(10, 10) should be (100)
  }

  test ("Bounds limiter partial function") {
    def boundToLimits(lower: Int, v: Int, upper: Int) : Int = {
      // if the upper is higher than the lower, reverse the bounds
      val l = lower min upper
      val u = lower max upper

      if (v < l) l
      else if (v > u) u
      else v
    }

    // tests - make sure the bound to limits works
    boundToLimits(10, 5, 90) should be (10)
    boundToLimits(10, 50, 90) should be (50)
    boundToLimits(10, 100, 90) should be (90)

    // now create a partially applied function from the above called waterAsLiquid with lower bounds of 0
    // and upper bounds of 100, but with the middle value (to test) not yet bound (use a placeholder)
    // Then uncomment the tests below and make sure they pass

    val waterAsLiquid = boundToLimits(0, _: Int, 100)

    waterAsLiquid(34) should be (34)
    waterAsLiquid(-10) should be (0)
    waterAsLiquid(400) should be (100)
  }

  test ("Multiply variable number of arguments") {
    // create a multipleDoubles method to satisfy the tests below, uncomment the tests and run them
    // to ensure it works

    def multiplyDoubles(dbls: Double*): Double = {
      if (dbls.isEmpty) 1.0
      else dbls.head * multiplyDoubles(dbls.tail: _*)
    }

    multiplyDoubles(1.0, 2.0, 3.0) should be (6.0 +- 0.00001)
    multiplyDoubles(1.1, 2.2, 3.3, 4.4, 5.5, 6.6) should be (1275.52392 +- 0.00001)
    multiplyDoubles() should be (1.0)

    // extra credit, can you re-write the multiplyDoubles method to work without needing to use any vars?
  }

  test ("Recurse with varargs") {
    // write a listOfLists recursive method that takes a number of strings as varargs and then
    // creates a list of lists of strings, with one less string in each, so for example:
    //
    // listOfLists("3","2","1") should give back: List(List("3","2","1"), List("2","1"), List("1"))
    // If you have trouble with the recursive call, check the argument expansion slide for help
    // Uncomment the tests below to make sure the method works.

    def listOfLists(theList: String*): List[List[String]] = {
      if (theList.isEmpty) Nil
      else theList.toList :: listOfLists(theList.tail: _*)
    }

    listOfLists("Hello", "World") should be (List(List("Hello", "World"), List("World")))
    listOfLists("Hello", "There", "World") should be (List(List("Hello", "There", "World"), List("There", "World"), List("World")))

    // is this implementation of listOfLists properly recursive? If not, why not?
  }

  test ("URL cleaner") {
    def urlClean(urlAsString : String) : URL = {
      try {
        new URL(urlAsString)
      }
      catch {
        case ex: MalformedURLException => new URL("http://badurl.com")
      }
    }

    // fix the method above to catch a malformed URL and replace it with a URL made out of
    // "http://badurl.com" so that the tests below pass

    urlClean("http://artima.com") should be (new URL("http://artima.com"))
    urlClean("http://javaposse.com") should be (new URL("http://javaposse.com"))
    urlClean("bad:url:malformed") should be (new URL("http://badurl.com"))
  }

  test ("Opposites matcher") {
    // convert the following method so that instead of returning the same string passed in, it returns
    // the opposite using the following heuristics:
    // North should return South
    // Hot should return Cold
    // Cool should return Square
    // anything else should return the same value with "Not " in front of it - e.g. A Lot should return Not A Lot
    // Use pattern matching

    def oppositeOf(item: String) =
      item match {
        case "North" => "South"
        case "Hot" => "Cold"
        case "Cool" => "Square"
        case anythingElse => "Not " + anythingElse
      }

    oppositeOf("North") should be ("South")
    oppositeOf("Hot") should be ("Cold")
    oppositeOf("Cool") should be ("Square")
    oppositeOf("Hip") should be ("Not Hip")
    oppositeOf("Funny") should be ("Not Funny")
  }
  test ("Detect any odds/evens in a list") {
    // fix the two functions below so that they return true if the list of numbers passed in has any
    // odd numbers (for the first functions) and even numbers (for the second function) so that the
    // tests pass

    def containsOdd(nums: List[Int]): Boolean = nums.exists(n => n % 2 != 0)
    def containsEven(nums: List[Int]): Boolean = nums.exists(n => n % 2 == 0)

    containsOdd(List(2,4,6)) should be (false)
    containsEven(List(1,3,5)) should be (false)

    containsOdd(List(1,2,3)) should be (true)
    containsEven(List(1,2,3)) should be (true)

    containsOdd(Nil) should be (false)
    containsEven(Nil) should be (false)
  }

  test ("With file contents control structure") {
    // uncomment the tests below, and then write a new function withFileContents that takes a file name
    // as input, opens the file, reads the first line in, and provides it to a function that does something
    // with it - the tests below will exercise a couple of options based on files in the working directory
    // to make sure your implementation works. Of course, your function should close the file after use.
    // For now, you can assume that the function passed in takes a String and returns another String.
    // Hint - to make the tests pass, you might need to clean up the string that is read in from the file,
    // try .trim()

    def withFileContents(fileName: String)(fn: String => String): String = {
      val file = new BufferedReader(new FileReader(fileName))
      try {
        val line = file.readLine.trim
        fn(line)
      }
      finally {
        file.close()
      }
    }

    val palindrome = withFileContents("quote.txt") { str => str.reverse }
    palindrome should be ("Madam, I'm Adam")

    val total = withFileContents("sum.txt") { str =>
      str.split(",").map(_.toInt).reduceLeft(_ + _).toString   // make sure to understand what this is doing
    }
    total should be ("20")
  }

  test ("onlyIfTrue - your own predicate guard") {
    // This is a bit of a contrived exercise, but make a function onlyIfTrue that takes a predicate (by-name
    // function that returns a Boolean), and an operation to carry out if and only if the predicate
    // is true, otherwise do nothing. For now, assume that the operation takes no arguments and has no
    // return (Unit).

    def onlyIfTrue(cond: => Boolean)(fn: => Unit) {
      if (cond) fn
    }

    val numList = List (-1, 0, -2, 3, -4, 5)
    var numberBelowZero = 0

    numList.foreach { n => onlyIfTrue(n < 0) {numberBelowZero += 1 } }

    numberBelowZero should be (3)
  }

  // extra credit - the above exercise is only present to get you used to by-name and curried functions
  // you could do the same using Scala's build in features, without using a var, and in a much more compact
  // way. Write a test to find the number of negative numbers in the list using a filter, and no vars.
  // Why is this a better solution? Where might you want to do as above anyway?

  test ("Count negatives in list") {
    val numList = List(-1, 0, -2, 3, -4, 5)
    val count = numList.filter(n => n < 0).length

    count should be (3)
  }
}
