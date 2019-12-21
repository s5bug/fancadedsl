lazy val fancadedsl = (project in file("fancadedsl")).settings(
  organization := "tf.bug",
  name := "fancadedsl",
  version := "0.1.0",
  scalaVersion := "2.13.1",
  resolvers += Resolver.bintrayRepo("alexknvl", "maven"),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.0.0",
    "org.typelevel" %% "cats-effect" % "2.0.0",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.alexknvl" %% "polymorphic" % "0.5.0",
    "io.chrisdavenport" %% "fuuid" % "0.3.0-M5",
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
)
