package homework_3

import utest._
import Exercises._

object Test extends TestSuite{

    val tests = Tests{
        'formatter_tests - {
            def formatterTest(formatterFunc: Any => String) : Unit = {
                assert(formatterFunc(true) == TRUE_STR)
                assert(formatterFunc(false) == FALSE_STR)
                assert(formatterFunc("true") == "true")
                assert(formatterFunc(0) == "0")
            }
            'formatter1 - {
                formatterTest(prettyBooleanFormatter1)
            }
            'formatter2 - {
                formatterTest(prettyBooleanFormatter2)
            }
            'formatter3 - {
                formatterTest(prettyBooleanFormatter3)
            }
        }
        'max_tests - {
            'max1 - {
                intercept[Exception] {
                    max1(Seq())
                }
                assert(max1(Seq(0)) == 0)
                assert(max1(Seq(2, 1)) == 2)
                assert(max1(Seq(-2, 1)) == 1)
                assert(max1(Seq(-2, 1, 1)) == 1)
                assert(max1(Seq(-100, -1, -5)) == -1)
            }
            'max2 - {
                assert(max2(Seq()) == Seq())
                assert(max2(Seq(0)) == Seq(0))
                assert(max2(Seq(2, 1)) == Seq(2))
                assert(max2(Seq(-2, 1)) == Seq(1))
                assert(max2(Seq(-2, 1, 1)) == Seq(1))
                assert(max2(Seq(-100, -1, -5)) == Seq(-1))
            }
            'max3 - {
                assert(max3(Seq()).isEmpty)
                assert(max3(Seq(0)).get == 0)
                assert(max3(Seq(2, 1)).get == 2)
                assert(max3(Seq(-2, 1)).get == 1)
                assert(max3(Seq(-2, 1, 1)).get == 1)
                assert(max3(Seq(-100, -1, -5)).get == -1)
            }
        }
        'sum_tests - {
            def sumTest(sumFunc: (Int, Int) => Int) : Unit = {
                assert(sumFunc(0, 0) == 0)
                assert(sumFunc(0, 1) == 1)
                assert(sumFunc(-1, 0) == -1)
                assert(sumFunc(-3, 3) == 0)
                assert(sumFunc(7, -5) == 2)
                assert(sumFunc(11, 99989) == 100000)
            }
            'sum1 - {
                sumTest(sum1)
            }
            'sum2 - {
                sumTest(sum2)
            }
            'sum3 - {
                sumTest(sum3)
            }
        }
    }
}
