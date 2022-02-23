val x = 1 + 2

val y = 1.+(2)

val s = "hello"

s.charAt(1)
s charAt 1
List(1,2,3).map( _ * 2)
List(1,2,3) map { _ * 3 }
// println "hello" // will not compile

System.out println "hello"

// --- apply and update

val arr = Array("scooby", "dooby", "doo")
Array.apply("scooby", "dooby", "doo")

arr(1)
arr.apply(1)

arr(0)


arr.update(1, "scrappy")

arr(1) = "scrappy"

println(arr.deep)

val arr2 = Array.apply(1,2,3)

val z = 10
// z(2) // does not compile

val xs = List(1,2,3)
xs(1)  // works
// xs(1) = 10 // does not compile


