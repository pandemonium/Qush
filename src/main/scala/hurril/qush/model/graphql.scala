package hurril.qush
package model


case class Document(defs: List[ExecutableDefinition.T])

object ExecutableDefinition {
  sealed trait T
  case class Operation(operation: OperationDefinition.T)
    extends T
  case class Fragment(fragment: FragmentDefinition)
    extends T
}

object OperationDefinition {
  sealed trait T
  case class Query(name: Name.T,
              variables: List[Variable],
                queries: List[QueryDefinition])
    extends T
  case class Mutation()
    extends T
  case class Subscription()
    extends T
}

object TypeReference {
  sealed trait T
  // This name here, that is a term
  case class Named(name: String)
    extends T
  case class List(unwrappedType: T)
    extends T
  case class NonNull(unwrappedType: T)
    extends T
}

case class Variable(name: String, 
               inputType: TypeReference.T,
            defaultValue: Option[Value.T])

case class FragmentDefinition(name: String, 
                            onType: String,
                      selectionSet: List[Selection.T])

object Name {
  sealed trait T
  case class Named(name: String)
    extends T
  case object Unnamed
    extends T
}

case class QueryDefinition(query: Selection.T)

object Value {
  sealed trait T
  case class Text(constant: String)
    extends T
  case class Integer(constant: Int)
    extends T
  case class Array(values: List[Value.T])
    extends T
  case class Object(fields: Map[String, Value.T])
    extends T
  case class Enum(literal: String)
    extends T
  case class VariableRef(name: String)
    extends T
}

object Field {
  sealed trait T
  sealed trait Aliasee
  case class Select(name: String)
    extends T
       with Aliasee
  case class Apply(name: String,
              arguments: Map[String, Value.T])
    extends T
       with Aliasee
  case class Aliased(alias: String, 
                   aliasee: Aliasee)
    extends T    
}

object Selection {
  sealed trait T
  case class Scalar(field: Field.T)
    extends T
  case class Object(field: Field.T,
             selectionSet: List[T])
    extends T
  case class FragmentSpread(name: String)
    extends T
  case class InlineFragment(onType: String, 
                      selectionSet: List[T])
    extends T
}

// Who owns the type system? The parser needs access to this
// to typecheck the query.
object RunGraphQl extends App {
  val source = """
    |query HeroForEpisode($ep: Episode!) {
    |  hero(episode: $ep) {
    |    name
    |    ... on Droid {
    |      primaryFunction
    |    }
    |    ... on Human {
    |      height
    |    }
    |  }
    |}
  """.stripMargin

  val ast = Document(
    ExecutableDefinition.Operation(
      OperationDefinition.Query(
        Name.Named("HeroForEpisode"),
        Variable("ep", TypeReference.NonNull(TypeReference.Named("Episode")), Option.empty) :: Nil,
        QueryDefinition(
          Selection.Object(
            Field.Apply("hero", Map(
              "episode" -> Value.VariableRef("ep")
            )),
            Selection.Scalar(Field.Select("name")) :: 
            Selection.InlineFragment("Droid", List(
              Selection.Scalar(
                Field.Select("primaryFunction")
              )
            )) ::
            Selection.InlineFragment("Human", List(
              Selection.Scalar(
                Field.Select("height")
              )
            )) ::
            Nil
          )

        ) :: Nil
      )
    ) :: Nil
  )

  println(source)
}