lazy val fancadedsl = (project in file("fancadedsl")).settings(
  organization := "tf.bug",
  name := "fancadedsl",
  version := "0.1.0",
  scalaVersion := "2.13.8",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.7.0",
    "org.typelevel" %% "cats-effect" % "3.3.11",
    "com.chuusai" %% "shapeless" % "2.3.9",
    "io.chrisdavenport" %% "fuuid" % "0.8.0-M2",
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
)
