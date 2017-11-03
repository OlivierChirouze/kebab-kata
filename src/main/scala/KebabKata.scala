import scala.annotation.tailrec

object KebabKata {

    trait Kebab {
        /**
          * Is this kebab completely vegetarian? (only vegetables, eggs, etc)
          * @return
          */
        def isVegetarian: Boolean

        /**
          * Is this kebab pescetarian? (vegetarian or fish)
          * @return
          */
        def isPescetarian: Boolean

        /**
          * Remove any onions from this kebab
          * @return
          */
        def removeOnions(): Kebab

        /**
          * "duplicate" all cheese ingredients
          * @return
          */
        def doubleCheese(): Kebab

        /**
          * Get the list of ingredient names
          * @return
          */
        def ingredientsList: List[String]
    }

    case class Ingredient(name: String, hasMeat: Boolean = false, hasFish: Boolean = false, isOnion: Boolean = false, isCheese: Boolean = false) {

        override def toString: String = name

        val isVegeratian: Boolean = !hasMeat && !hasFish
        val isPescetarian: Boolean = !hasMeat
    }

    case class TraditionalKebab(ingredients: List[Ingredient]) extends Kebab {
        val isVegetarian: Boolean = ingredients.forall(_.isVegeratian)
        val isPescetarian: Boolean = ingredients.forall(_.isPescetarian)

        def removeOnions(): Kebab = TraditionalKebab(ingredients.filterNot(_.isOnion))

        def doubleCheese(): TraditionalKebab = {
            TraditionalKebab(TraditionalKebab.addCheese(ingredients, Nil))
        }

        def ingredientsList: List[String] = ingredients.map(_.name)

        override def toString: String = ingredientsList.mkString(", ")
    }

    object TraditionalKebab {
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
    // TODO should probably use Some(CompositeKebab) for next
    class CompositeKebab(name: String, next: CompositeKebab) extends Kebab {
        def isVegetarian: Boolean = next.isVegetarian

        def isPescetarian: Boolean = next.isPescetarian

        def ingredientsList: List[String] = name :: next.ingredientsList

        override def toString: String = ingredientsList.mkString(", ")

        def copy(name: String = name, next: CompositeKebab = next) = CompositeKebab(name, next)

        def removeOnions(): CompositeKebab = copy(name, next.removeOnions())

        def doubleCheese(): CompositeKebab = copy(name, next.doubleCheese())

        // Consider two kebabs are the same if their ingredients have the same names in the same order
        override def equals(obj: scala.Any): Boolean = obj match {
            case kebab if obj.isInstanceOf[CompositeKebab] => obj.asInstanceOf[CompositeKebab].ingredientsList == ingredientsList
            case _ => super.equals(obj)
        }
    }

    object CompositeKebab {
        def apply(name: String, next: CompositeKebab): CompositeKebab = new CompositeKebab(name, next)
    }

    case class CheeseIngredient(name: String, next: CompositeKebab) extends CompositeKebab(name: String, next: CompositeKebab) {
        override def copy(name: String = name, next: CompositeKebab = next) = CheeseIngredient(name, next)

        // Need to duplicate this very ingredient => let's copy itself
        override def doubleCheese(): CompositeKebab = copy(name, super.doubleCheese())
    }

    case class OnionIngredient(name: String, next: CompositeKebab) extends CompositeKebab(name: String, next: CompositeKebab) {
        override def copy(name: String = name, next: CompositeKebab = next) = OnionIngredient(name, next)

        // Want to remove this very ingredient ==> let's replace it by next
        override def removeOnions(): CompositeKebab = next.removeOnions()
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

        override def removeOnions(): CompositeKebab = this
    }

}