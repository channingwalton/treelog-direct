package treelog.direct

class QuadraticRootsSpec extends munit.FunSuite:
  test("renders the success tree"):
    val output = QuadraticRoots.root(Parameters(2, 5, 3))

    assertEquals(output.result, Right(-1.0))
    assertEquals(
      output.tree.render,
      """Extracting root
        |  Calculating Numerator
        |    Calculating Determinant
        |      Calculating b^2
        |        Got b: 5.0
        |        Got b^2: 25.0
        |      Calculating 4ac
        |        Got a: 2.0
        |        Got c: 3.0
        |        Got 4ac: 24.0
        |      Got b^2 - 4ac: 1.0
        |    Calculating sqrt(determinant)
        |      Determinant (1.0) is >= 0
        |      Got sqrt(determinant): 1.0
        |    Got b: 5.0
        |    Got -b: -5.0
        |    Got -b + sqrt(determinant): -4.0
        |  Calculating Denominator
        |    Got a: 2.0
        |    Got 2a: 4.0
        |  Got root = numerator / denominator: -1.0""".stripMargin
    )

  test("renders the failure tree"):
    val output = QuadraticRoots.root(Parameters(2, 5, 10))

    assertEquals(output.result, Left("Negative determinant: -55.0"))
    assertEquals(
      output.tree.render,
      """Extracting root: Failed
        |  Calculating Numerator: Failed
        |    Calculating Determinant
        |      Calculating b^2
        |        Got b: 5.0
        |        Got b^2: 25.0
        |      Calculating 4ac
        |        Got a: 2.0
        |        Got c: 10.0
        |        Got 4ac: 80.0
        |      Got b^2 - 4ac: -55.0
        |    Calculating sqrt(determinant): Failed
        |      Determinant (-55.0) is < 0: Failed""".stripMargin
    )

final case class Parameters(a: Double, b: Double, c: Double)

object QuadraticRoots:
  def root(p: Parameters): Logged[String, Double] =
    branch("Extracting root"):
      for
        num <- numerator(p)
        den <- denominator(p)
        r   <- success(num / den, r => s"Got root = numerator / denominator: $r")
      yield r

  private def numerator(p: Parameters): Logged[String, Double] =
    branch("Calculating Numerator"):
      for
        det    <- determinant(p)
        sqrt   <- sqrtDeterminant(det)
        b      <- success(p.b, b => s"Got b: $b")
        minusB <- success(-b, b => s"Got -b: $b")
        sum    <- success(minusB + sqrt, s => s"Got -b + sqrt(determinant): $s")
      yield sum

  private def denominator(p: Parameters): Logged[String, Double] =
    branch("Calculating Denominator"):
      for
        a    <- success(p.a, a => s"Got a: $a")
        twoA <- success(2 * a, twoA => s"Got 2a: $twoA")
      yield twoA

  private def determinant(p: Parameters): Logged[String, Double] =
    branch("Calculating Determinant"):
      for
        b2 <- bSquared(p)
        ac <- fourac(p)
        d  <- success(b2 - ac, d => s"Got b^2 - 4ac: $d")
      yield d

  private def bSquared(p: Parameters): Logged[String, Double] =
    branch("Calculating b^2"):
      for
        b  <- success(p.b, b => s"Got b: $b")
        b2 <- success(b * b, b2 => s"Got b^2: $b2")
      yield b2

  private def fourac(p: Parameters): Logged[String, Double] =
    branch("Calculating 4ac"):
      for
        a  <- success(p.a, a => s"Got a: $a")
        c  <- success(p.c, c => s"Got c: $c")
        ac <- success(4 * a * c, ac => s"Got 4ac: $ac")
      yield ac

  private def sqrtDeterminant(det: Double): Logged[String, Double] =
    branch("Calculating sqrt(determinant)"):
      for
        _ <- require(
          det >= 0,
          error = s"Negative determinant: $det",
          successLabel = s"Determinant ($det) is >= 0",
          failureLabel = s"Determinant ($det) is < 0"
        )
        r <- success(Math.sqrt(det), r => s"Got sqrt(determinant): $r")
      yield r
