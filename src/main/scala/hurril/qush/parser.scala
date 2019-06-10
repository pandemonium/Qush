package hurril.qush
package parser

import cats.implicits._
import scala.util.parsing.combinator._
import model._


object GraphQlParser extends JavaTokenParsers {
  def blockOpen: Parser[String] = "{"
  def blockClose: Parser[String] = "}"

  def argumentsOpen = "("
  def argumentsClose = ")"

  def listOpen: Parser[String] = "["
  def listClose: Parser[String] = "]"

  def document = executableDefinition.* ^^ Document
  def executableDefinition: Parser[ExecutableDefinition.T] = 
    queryOperation ^^ ExecutableDefinition.Operation |
    fragment       ^^ ExecutableDefinition.Fragment

  def fragmentKeyword = "fragment"

  def fragment = 
    ((fragmentKeyword ~> ident) <~ "on") ~ ident ~ selectionSetBlock ^^ {
    case name ~ onType ~ selectionSet =>
      FragmentDefinition.Def(name, onType, selectionSet)
  }

  def queryOperationType = "query"

  def queryOperation = namedQuery | anonymousQuery
  def namedQuery = for {
    (name, vars) <- queryPreamble
    query        <- queryEpilogue(name, vars)
  } yield query
  def anonymousQuery = queryEpilogue(Name.Unnamed, List.empty)

  def queryPreamble = queryOperationType ~> queryName ~ variables.? ^^ {
    case queryName ~ variables => queryName -> variables.sequence.flatten
  }
  def queryEpilogue(name: Name.T, 
               variables: List[Variable]) = 
    blockOpen ~> (queryDefinition.* <~ blockClose) ^^ { queries =>
      OperationDefinition.Query(name, variables, queries)
    }
  def queryName: Parser[Name.T] = ident.? ^^ {
    case Some(name) => Name.Named(name)
    case _          => Name.Unnamed
  }

  // These are separated by either >>nothing>> or >>,>>
  def variables = (argumentsOpen ~> variable.*) <~ argumentsClose
  def variable = (variableRef <~ ":") ~ typeRef ^^ {
    case name ~ typeRef => Variable(name, typeRef, Option.empty)
  }

  def typeRef: Parser[TypeReference.T] = nonNullNamedType | listType
  def nonNullNamedType = namedType ~ "!".? ^^ {
    case unwrapped ~ nonNull 
        if nonNull.nonEmpty =>
      TypeReference.NonNull(unwrapped)
    case unwrapped ~ _ => unwrapped
  }
  def namedType = ident ^^ TypeReference.Named
  def listType = for {
    _            <- listOpen
    unwrapped    <- typeRef ^^ TypeReference.List
    elementType  <- listClose ~> "!".? ^^ {
      case nonNull if nonNull.nonEmpty =>
        TypeReference.NonNull(unwrapped)
      case _ => unwrapped
    }
  } yield elementType

  def queryDefinition = selectObject ^^ QueryDefinition

  def selectionSet = selectObject | selectScalar
  def selectObject: Parser[Selection.T] = field ~ selectionSetBlock ^^ {
    case field ~ selectionSet => Selection.Object(field, selectionSet)
  }

  def selectionSetBlock = for {
    _      <- blockOpen
    selSet <- selectionSet.*
    _      <- blockClose
  } yield selSet

  def selectScalar = field ^^ Selection.Scalar

  def field: Parser[Field.T] = (ident <~ ":").? ~ aliaseeField ^^ {
    case Some(alias) ~ field => Field.Aliased(alias, field)
    case _ ~ field           => field
  }
  def aliaseeField = apply | select
  def select = ident ^^ Field.Select
  def apply = ident ~ arguments ^^ {
    case name ~ arguments => Field.Apply(name, arguments.toMap)
  }

  def arguments = argumentsOpen ~> (argument.* <~ argumentsClose)
  def argument = (ident <~ ":") ~ value ^^ {
    case name ~ value => name -> value
  }

  def value: Parser[Value.T] =              // this can fail.                                     
    wholeNumber ^^ { image => Value.Integer(image.toInt) } |
    text        ^^ Value.Text        |
    variableRef ^^ Value.VariableRef |
    enumLiteral ^^ Value.Enum

  def text = stringLiteral ^^ (_.drop(1).dropRight(1))

  def variableRef = "$" ~> ident
  def enumLiteral = ident
}

object RunParser extends App {
  val source = """
    |query Robban ($foo: String! $bar: [Int!]!) {
    |  blom: foo(id: 5) {
    |    rutabaga: name
    |  }
    |  rojne: bar {
    |    orrefors:wibble {
    |      lol:lol
    |    }
    |  }
    |}
    |fragment someFields on SomeType {
    |  name
    |  someObject(some: $bar) {
    |    withAField
    |  }
    |}
  """.stripMargin

  val result = GraphQlParser.parseAll(GraphQlParser.document, source)

  println(result)
}