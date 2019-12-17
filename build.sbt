lazy val fancadedsl = (project in file("fancadedsl")).settings(
  organization := "tf.bug",
  name := "fancadedsl",
  version := "0.1.0",
  scalaVersion := "2.13.1",
  resolvers += Resolver.bintrayRepo("alexknvl", "maven"),
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.alexknvl" %% "polymorphic" % "0.5.0",
  ),
)
