package hurril.qush
package model

object Graphql {
  object Intrinsic {
    object ScalarType {
      sealed trait T
      case object String
        extends T
    }
  }

  object TypeDefinition {
    sealed trait T
    case class Scalar(concreteType: Intrinsic.ScalarType.T)
      extends T
    case class Object()
      extends T
    case class Interface()
      extends T
    case class Union()
      extends T
    case class Enum()
      extends T
    case class InputObject()
      extends T
  }

  object Literal {
    sealed trait T
    case class Text(value: String)
      extends T
    case class Integer(value: Int)
      extends T
    case class Enumeration(value: String)
      extends T
  }

  case class Binding(name: String,
                    value: Expression.T)

  object Expression {
    sealed trait T
    case class Select(symbol: String)
      extends T
    case class Alias(name: String, 
               expression: T)
      extends T
    case class Variable(name: String)
      extends T
    case class Constant(value: Literal.T)
      extends T
    case class Apply(symbol: String,
                  arguments: List[Binding])
      extends T
  }

  case class Query(name: String, 
                 select: List[Field.T])

  object Field {
    sealed trait T

  }
}