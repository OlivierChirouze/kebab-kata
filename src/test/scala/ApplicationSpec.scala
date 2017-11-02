import Application.{EmptyIngredient, _}
import org.scalatest._

import scala.collection.mutable

class ApplicationSpec extends FunSpec with Matchers {

    var ingredients: mutable.HashMap[String, Ingredient] = mutable.HashMap(
        "tomate" -> Ingredient("tomate"),
        "salade" -> Ingredient("salade"),
        "oignon" -> Ingredient("oignon", isOnion = true),
        "cheese" -> Ingredient("cheese", isCheese = true),
        "viande" -> Ingredient("viande", hasMeat = true),
        "poisson" -> Ingredient("poisson", hasFish = true)
    )

    // Initialize composite ingredients as if they were alone in the kebab (all with empty next)
    var compositeIngredients: mutable.HashMap[String, CompositeKebab] = mutable.HashMap(
        "tomate" -> CompositeKebab("tomate", EmptyIngredient),
        "salade" -> CompositeKebab("salade", EmptyIngredient),
        "oignon" -> CompositeKebab("oignon", EmptyIngredient),
        "cheese" -> CompositeKebab("cheese", EmptyIngredient),
        "viande" -> MeatIngredient("viande", EmptyIngredient),
        "poisson" -> FishIngredient("poisson", EmptyIngredient)
    )

    // Build a kebab from the names of the ingredients
    private def getKebab(names: List[String]): InheritenceKebab = {
        InheritenceKebab(names.map(ingredients(_)))
    }

    // Build a composite kebab from a list of ingredients
    private def getCompositeKebab(names: List[String]): CompositeKebab = {
        if (names.isEmpty) {
            EmptyIngredient
        } else {
            val head :: rest = names
            compositeIngredients(head).copy(next = getCompositeKebab(rest))
        }
    }

    def test(name: String, getKebab: List[String] => Kebab) = {
        describe(name) {

            it("should tell if it's vegetarian") {
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon"
                )).isVegetarian shouldBe true
                getKebab(List(
                    "salade",
                    "oignon",
                    "tomate",
                    "oignon",
                    "viande"
                )).isVegetarian shouldBe false
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon",
                    "cheese",
                    "poisson",
                    "poisson"
                )).isVegetarian shouldBe false
            }

            it("should tell if it's pescetarian") {
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon"
                )).isPescetarian shouldBe true
                getKebab(List(
                    "salade",
                    "oignon",
                    "tomate",
                    "oignon",
                    "viande"
                )).isPescetarian shouldBe false
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon",
                    "cheese",
                    "poisson",
                    "poisson"
                )).isPescetarian shouldBe true
            }

            it("should remove onions") {
                getKebab(List(
                    "salade",
                    "oignon",
                    "tomate",
                    "oignon",
                    "viande"
                )).removeOnions shouldEqual getKebab(List(
                    "salade",
                    "tomate",
                    "viande"
                ))
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon"
                )).removeOnions shouldEqual getKebab(List(
                    "salade",
                    "cheese",
                    "tomate"
                ))
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon",
                    "cheese",
                    "poisson",
                    "poisson"
                )).removeOnions shouldEqual getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "cheese",
                    "poisson",
                    "poisson"
                ))
            }

            it("should double cheese") {
                getKebab(List(
                    "salade",
                    "oignon",
                    "tomate",
                    "oignon",
                    "viande"
                )).doubleCheese shouldEqual getKebab(List(
                    "salade",
                    "oignon",
                    "tomate",
                    "oignon",
                    "viande"
                ))
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon"
                )).doubleCheese shouldEqual getKebab(List(
                    "salade",
                    "cheese",
                    "cheese",
                    "tomate",
                    "oignon"
                ))
                getKebab(List(
                    "salade",
                    "cheese",
                    "tomate",
                    "oignon",
                    "cheese",
                    "poisson",
                    "poisson"
                )).doubleCheese shouldEqual getKebab(List(
                    "salade",
                    "cheese",
                    "cheese",
                    "tomate",
                    "oignon",
                    "cheese",
                    "cheese",
                    "poisson",
                    "poisson"
                ))
            }
        }
    }

    test("kebab", getKebab)

    describe("CompositeKebab") {
        it("should be similar to kebab") {
            getCompositeKebab(List(
                "salade",
                "oignon",
                "tomate",
                "oignon",
                "viande"
            )).toString shouldEqual getKebab(List(
                "salade",
                "oignon",
                "tomate",
                "oignon",
                "viande"
            )).toString
        }
    }

    test("composite kebab", getCompositeKebab)
}
