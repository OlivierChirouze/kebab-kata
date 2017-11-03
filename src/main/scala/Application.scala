import java.lang.reflect.Field

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty
import scala.collection.mutable.ListBuffer

object Application {

    trait Kebab {
        def isVegetarian: Boolean

        def isPescetarian: Boolean

        def removeOnions(): Kebab

        def doubleCheese(): Kebab

        def ingredientsList: List[String]
    }

    case class Ingredient(name: String, hasMeat: Boolean = false, hasFish: Boolean = false, isOnion: Boolean = false, isCheese: Boolean = false) {

        override def toString: String = name

        val isVegeratian: Boolean = !hasMeat && !hasFish
        val isPescetarian: Boolean = !hasMeat
    }

    case class InheritenceKebab(ingredients: List[Ingredient]) extends Kebab {
        val isVegetarian: Boolean = ingredients.forall(_.isVegeratian)
        val isPescetarian: Boolean = ingredients.forall(_.isPescetarian)

        def removeOnions(): Kebab = InheritenceKebab(ingredients.filterNot(_.isOnion))


        def doubleCheese: InheritenceKebab = {
            InheritenceKebab(InheritenceKebab.addCheese(ingredients, Nil))
        }

        def ingredientsList: List[String] = ingredients.map(_.name)

        override def toString: String = ingredientsList.mkString(", ")
    }

    object InheritenceKebab {
        @tailrec
        private def addCheese(ingredients: List[Ingredient], acc: List[Ingredient]): List[Ingredient] = ingredients match {
            case Nil => acc.reverse
            case head :: tail =>
                if (head.isCheese)
                    addCheese(tail, head :: head.copy() :: acc)
                else
                    addCheese(tail, head :: acc)
        }

    }

    // ------------------------------------------------------------------- Composite pattern version
    class CompositeKebab(name: String, next: CompositeKebab) extends Kebab {
        def isVegetarian: Boolean = next.isVegetarian

        def isPescetarian: Boolean = next.isPescetarian

        def ingredientsList: List[String] = name :: next.ingredientsList

        override def toString: String = {
            //            val nextString = next.toString
            //            // Implode with "," until empty
            //            name + (
            //                nextString match {
            //                    case empty if nextString.isEmpty => empty
            //                    case _ => ", " + nextString
            //                })

            ingredientsList.mkString(", ")
        }

        def copy(name: String = name, next: CompositeKebab = next) = CompositeKebab(name, next)

        def removeOnions(): CompositeKebab = ???

        def doubleCheese(): CompositeKebab = copy(name, next.doubleCheese())
    }

    object CompositeKebab {
        def apply(name: String, next: CompositeKebab): CompositeKebab = new CompositeKebab(name, next)
    }

    //    case class OnionIngredient(name: String, next: CompositeKebab) extends CompositeKebab(name: String, next: CompositeKebab) {
    //        override def copy(name: String = name, next: CompositeKebab = next) = OnionIngredient(name, next)
    //
    //        override def removeOnions(): Kebab = super.removeOnions()
    //    }

    case class CheeseIngredient(name: String, next: CompositeKebab) extends CompositeKebab(name: String, next: CompositeKebab) {
        override def copy(name: String = name, next: CompositeKebab = next) = CheeseIngredient(name, next)

        override def doubleCheese(): CompositeKebab = {
            copy(name, super.doubleCheese())
        }
    }

    case class MeatIngredient(name: String, next: CompositeKebab) extends CompositeKebab(name: String, next: CompositeKebab) {
        override def isVegetarian: Boolean = false

        override def isPescetarian: Boolean = false

        override def copy(name: String = name, next: CompositeKebab = next) = MeatIngredient(name, next)
    }

    case class FishIngredient(name: String, next: CompositeKebab) extends CompositeKebab(name: String, next: CompositeKebab) {
        override def isVegetarian: Boolean = false

        override def copy(name: String = name, next: CompositeKebab = next) = FishIngredient(name, next)
    }

    // Null object
    // TODO should probably use None for next
    object EmptyIngredient extends CompositeKebab(name = "empty", next = null) {
        override def isVegetarian: Boolean = true

        override def isPescetarian: Boolean = true

        override def toString: String = ""

        override def ingredientsList: List[String] = Nil

        override def doubleCheese(): CompositeKebab = this
    }

}