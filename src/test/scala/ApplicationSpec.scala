import Application._
import org.scalatest._

import scala.collection.mutable

class ApplicationSpec extends FunSpec with Matchers {

    var ingredients: mutable.HashMap[String, Ingredient] = mutable.HashMap(
        "tomate" -> Ingredient("tomate"),
        "salade" -> Ingredient("salade"),
        "ognon" -> Ingredient("ognon"),
        "cheese" -> Ingredient("cheese"),
        "viande" -> Ingredient("viande", hasMeat = true),
        "poisson" -> Ingredient("poisson", hasFish = true)
    )

    val oneKebabViande = Kebab(List(
        ingredients("salade"),
        ingredients("ognon"),
        ingredients("tomate"),
        ingredients("ognon"),
        ingredients("viande")
    ))
    val oneKebabVegetable = Kebab(List(
        ingredients("salade"),
        ingredients("cheese"),
        ingredients("tomate"),
        ingredients("ognon")
    ))
    val oneKebabPoisson = Kebab(List(
        ingredients("salade"),
        ingredients("cheese"),
        ingredients("tomate"),
        ingredients("ognon"),
        ingredients("cheese"),
        ingredients("poisson"),
        ingredients("poisson")
    ))

    val oneKebabViande2 =
        CompositeKebab("salade",
            CompositeKebab("ognon",
                CompositeKebab("tomate",
                    CompositeKebab("ognon",
                        MeatIngredient("viande",
                            EmptyIngredient
                        )
                    )
                )
            )
        )

    describe("Kebab") {

        it("should tell if it's vegetarian") {
            oneKebabVegetable.isVegetarian shouldBe true
            oneKebabViande.isVegetarian shouldBe false
            oneKebabPoisson.isVegetarian shouldBe false
        }

        it("should tell if it's pescetarian") {
            oneKebabVegetable.isPescetarian shouldBe true
            oneKebabViande.isPescetarian shouldBe false
            oneKebabPoisson.isPescetarian shouldBe true
        }

        it("should remove onions") {
            oneKebabViande.removeOnions shouldEqual Kebab(List(
                ingredients("salade"),
                ingredients("tomate"),
                ingredients("viande")
            ))
            oneKebabVegetable.removeOnions shouldEqual Kebab(List(
                ingredients("salade"),
                ingredients("cheese"),
                ingredients("tomate")
            ))
            oneKebabPoisson.removeOnions shouldEqual Kebab(List(
                ingredients("salade"),
                ingredients("cheese"),
                ingredients("tomate"),
                ingredients("cheese"),
                ingredients("poisson"),
                ingredients("poisson")
            ))
        }


        it("should double cheese") {
            oneKebabViande.doubleCheese shouldEqual oneKebabViande
            oneKebabVegetable.doubleCheese shouldEqual Kebab(List(
                ingredients("salade"),
                ingredients("cheese"),
                ingredients("cheese"),
                ingredients("tomate"),
                ingredients("ognon")
            ))
            oneKebabPoisson.doubleCheese shouldEqual Kebab(List(
                ingredients("salade"),
                ingredients("cheese"),
                ingredients("cheese"),
                ingredients("tomate"),
                ingredients("ognon"),
                ingredients("cheese"),
                ingredients("cheese"),
                ingredients("poisson"),
                ingredients("poisson")
            ))
        }
    }

    describe("CompositeKebab") {
        it("should be similar to kebab") {
            oneKebabViande2.toString shouldEqual oneKebabViande.toString
        }
    }
}
