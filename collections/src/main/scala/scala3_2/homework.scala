package scala3_2

object homework1 {
  extension (x: Int)
    def ++(y: Int): String = x.toString + y.toString

  @main def part1Ex() = {
    println(1 ++ 33)
  }
}

object Completions {
  enum CompletionArg {
    case StrType(s: String)
    case IntType(i: Int)
    case FloatType(f: Float)
  }

  object CompletionArg {
    given fromString: Conversion[String, CompletionArg] = StrType(_)

    given fromInt: Conversion[Int, CompletionArg] = IntType(_)

    given fromFloat: Conversion[Float, CompletionArg] = FloatType(_)
  }

  import CompletionArg.*

  def complete[T](arg: CompletionArg) = arg match
    case StrType(s) => s"It is String = $s"
    case IntType(i) => s"It is Int = $i"
    case FloatType(f) => s"It is Float = $f"

  @main def part2Ex(): Unit = {
    println(Completions.complete("String"))
    println(Completions.complete(1))
    println(Completions.complete(7f))
  }
}


object homework3 {
  opaque type Logarithm = Double

  object Logarithm {
    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None
  }

  extension (x: Logarithm)

    def toDouble: Double = math.exp(x)

    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))

    def *(y: Logarithm): Logarithm = x + y


  @main def part3Ex(): Unit = {
    import Logarithm.*

    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2
    println(s"l=$l")
    println(s"l2=$l2")
    println(s"l3=$l3")
    println(s"l4=$l4")

  }
}