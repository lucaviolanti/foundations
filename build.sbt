import Dependencies._

lazy val baseSettings: Seq[Setting[_]] = Seq(
  scalaVersion := "2.13.2",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:postfixOps",
    "-unchecked",
    "-Ywarn-value-discard",
//    "-Wconf:cat=unchecked:e", // uncomment to transform type erasure warnings into errors
  ),
  addCompilerPlugin(kindProjector),
  libraryDependencies ++= Seq(
    cats,
    circe,
    scalacheck,
    disciplineTest
  )
)

lazy val foundation = project
  .in(file("."))
  .settings(moduleName := "foundation")
  .settings(baseSettings: _*)
  .aggregate(exercises, answers, slides)
  .dependsOn(exercises, answers, slides)

lazy val exercises = project
  .settings(moduleName := "foundation-exercises")
  .settings(baseSettings: _*)

lazy val answers = project
  .settings(moduleName := "foundation-answers")
  .settings(baseSettings: _*)

lazy val slides = project
  .dependsOn(answers)
  .settings(moduleName := "foundation-slides")
  .settings(baseSettings: _*)
  .settings(
    mdocIn := baseDirectory.value / "mdoc",
    mdocOut := baseDirectory.value / "docs",
  )
  .enablePlugins(MdocPlugin)


addCommandAlias("testAnswers", "answers/test")
