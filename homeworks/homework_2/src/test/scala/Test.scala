import Exercises._
import utest._

object Test extends TestSuite{

    val epsilon = 0.000001
    val nonZeroVectors = List(
        Vector2D(0,1),
        Vector2D(1,0),
        Vector2D(0, -1),
        Vector2D(-1,0),
        Vector2D(1,1),
        Vector2D(2,1),
        Vector2D(-1,2),
        Vector2D(-2, -1),
        Vector2D(1, -2)
    )
    val allVectors : List[Vector2D] = Vector2D(0,0) +: nonZeroVectors

    def testSum(vectors : List[Vector2D],
                func : (Vector2D, Vector2D) => Double,
                sumFunc : (Vector2D, Vector2D, Vector2D, Vector2D) => Double) : Unit = {
        for (v0 <- vectors; v1 <- vectors; v2 <- vectors; v3 <- vectors) {
            assert(Math.abs(sumFunc(v0, v1, v2, v3) - (func(v0, v1) + func(v2, v3))) < epsilon)
        }
    }

    val tests = Tests{
        'test_divBy3Or7 - {
            assert(divBy3Or7(1, 3) == Seq(3))
            assert(divBy3Or7(5, 9) == Seq(6, 7, 9))
            assert(divBy3Or7(0, 100) == Seq(0, 3, 6, 7, 9, 12, 14, 15, 18, 21, 24, 27, 28, 30, 33, 35, 36, 39, 42, 45, 48, 49, 51, 54, 56, 57, 60, 63, 66, 69, 70, 72, 75, 77, 78, 81, 84, 87, 90, 91, 93, 96, 98, 99))
        }
        'test_sumOfDivBy3Or5 - {
            assert(sumOfDivBy3Or5(0, 2) == 0)
            assert(sumOfDivBy3Or5(0, 3) == 3)
            assert(sumOfDivBy3Or5(0, 5) == 8)
            assert(sumOfDivBy3Or5(0, 6) == 14)
            assert(sumOfDivBy3Or5(0, 10) == 33)
            assert(sumOfDivBy3Or5(-6, 0) == -14)
            assert(sumOfDivBy3Or5(-10, 10) == 0)
        }
        'test_primeFactor - {
            assert(primeFactor(-2) == Seq())
            assert(primeFactor(0) == Seq())
            assert(primeFactor(1) == Seq())
            assert(primeFactor(2) == Seq(2))
            assert(primeFactor(3) == Seq(3))
            assert(primeFactor(10) == Seq(2,5))
            assert(primeFactor(15) == Seq(3,5))
            assert(primeFactor(20) == Seq(2,5))
            assert(primeFactor(80) == Seq(2,5))
            assert(primeFactor(98) == Seq(2,7))
            assert(primeFactor(525) == Seq(3,5,7))
        }
        'test_sumScalars - {
            testSum(allVectors, scalar, sumScalars)
        }
        'test_sumCosines - {
            testSum(nonZeroVectors, cosBetween, sumCosines)
        }
        'test_sortByHeavyweight - {
            assert(Math.abs(getMass(("Light", (5, 0.5))) - 261.7993877) < epsilon)

            val lightVal = ("Light", (5, 0.5))
            val mediumVal = ("Medium", (6, 2.3))
            val heavyVal = ("Heavy", (10, 12.34))
            val lightVal2 = ("", (5, 0.5))

            val emptyMap : Map[String, (Int, Double)] = Map()
            val singleValueMap = Map(lightVal)
            val twoValueMap = Map(mediumVal, lightVal)
            val threeValueMap = Map(mediumVal, lightVal, heavyVal)
            val repeatMassMap = Map(lightVal, heavyVal, lightVal2, mediumVal)

            assert(sortByHeavyweight(emptyMap) == Seq())
            assert(sortByHeavyweight(singleValueMap) == Seq("Light"))
            assert(sortByHeavyweight(twoValueMap) == Seq("Light", "Medium"))
            assert(sortByHeavyweight(threeValueMap) == Seq("Light", "Medium", "Heavy"))
            assert(sortByHeavyweight(repeatMassMap) == Seq("Light", "", "Medium", "Heavy"))
        }
    }
}
