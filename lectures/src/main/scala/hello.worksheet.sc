println("Hello World")

val x = 1
x + x





def factorial(n: Int): Int = product(x => x)(1, n)

factorial(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    def recur(a: Int): Int = 
        if a > b then zero
        else combine(f(a), recur(a + 1))
    recur(a)

def sum(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)
def product(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)
sum(factorial)(1, 5)
product(identity)(1, 6)

