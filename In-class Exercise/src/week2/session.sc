package week2

object session {
   def sum(f: Int => Int, a: Int, b: Int): Int = {
      def loop(a: Int, acc: Int): Int = {
         if (a > b) acc
         else loop(a + 1, acc + f(a))
      }
      loop(a, 0)
   }                                              //> sum: (f: Int => Int, a: Int, b: Int)Int
   sum((x: Int) => x, 1, 4)                       //> res0: Int = 10
}