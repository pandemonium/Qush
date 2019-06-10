import sbt._

object Dependencies {
  object Version {
    val Cats            = "1.6.0"
    val CatsEffect      = "1.2.0"
  }

  lazy val catsCore            = cats("cats-core")
  lazy val catsFree            = cats("cats-free")

  lazy val catsEffect          = "org.typelevel"     %% "cats-effect"             % Version.CatsEffect

  lazy val parserCombinators   =  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1" withSources()

  private def cats(artifact: String) =
    "org.typelevel" %% artifact % Version.Cats withSources()
}
