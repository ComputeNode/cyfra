ThisBuild / organization := "com.computenode.cyfra"
ThisBuild / scalaVersion := "3.6.4"
ThisBuild / version := "0.1.0-RC1"

val lwjglVersion = "3.4.0"
val jomlVersion = "1.10.0"

lazy val osName = System.getProperty("os.name").toLowerCase
lazy val osArch = System.getProperty("os.arch")
lazy val lwjglNatives =
  osName.toLowerCase match {
    case mac if mac.contains("mac") =>
      if (osArch.startsWith("aarch64")) "natives-macos-arm64" else "natives-macos"
    case win if win.contains("win") =>
      val is64 = osArch.contains("64")
      val isArm = osArch.contains("aarch64")
      s"natives-windows${if (isArm) "-arm64" else if (is64) "" else "-x86"}"
    case linux if linux.contains("linux") =>
      if (osArch.startsWith("arm") || osArch.startsWith("aarch64"))
        if (osArch.contains("64") || osArch.contains("armv8"))
          "natives-linux-arm64"
        else
          "natives-linux-arm32"
      else if (osArch.startsWith("ppc"))
        "natives-linux-ppc64le"
      else if (osArch.startsWith("riscv"))
        "natives-linux-riscv64"
      else
        "natives-linux"
    case osName => throw new RuntimeException(s"Unknown operating system $osName")
  }

lazy val vulkanNatives =
  if (osName.toLowerCase.contains("mac"))
    Seq("org.lwjgl" % "lwjgl-vulkan" % lwjglVersion classifier lwjglNatives)
  else Seq.empty

lazy val commonSettings = Seq(
  moduleName := s"cyfra-${thisProject.value.id}",
  scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-language:implicitConversions"),
  resolvers += "maven snapshots" at "https://central.sonatype.com/repository/maven-snapshots/",
  resolvers += "OSGeo Release Repository" at "https://repo.osgeo.org/repository/release/",
  resolvers += "OSGeo Snapshot Repository" at "https://repo.osgeo.org/repository/snapshot/",
  libraryDependencies ++= Seq(
    "dev.zio" % "izumi-reflect_3" % "3.0.5",
    "com.lihaoyi" % "pprint_3" % "0.9.0",
    "com.diogonunes" % "JColor" % "5.5.1",
    "org.lwjgl" % "lwjgl" % lwjglVersion,
    "org.lwjgl" % "lwjgl-vulkan" % lwjglVersion,
    "org.lwjgl" % "lwjgl-vma" % lwjglVersion,
    "org.lwjgl" % "lwjgl" % lwjglVersion classifier lwjglNatives,
    "org.lwjgl" % "lwjgl-vma" % lwjglVersion classifier lwjglNatives,
    "org.joml" % "joml" % jomlVersion,
    "commons-io" % "commons-io" % "2.16.1",
    "org.scalameta" % "munit_3" % "1.0.0" % Test,
    "com.lihaoyi" %% "sourcecode" % "0.4.3-M5",
    "org.slf4j" % "slf4j-api" % "2.0.17",
    "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.24.3" % Test,
  ) ++ vulkanNatives,
)

lazy val runnerSettings = Seq(libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j2-impl" % "2.24.3")

lazy val fs2Settings = Seq(libraryDependencies ++= Seq("co.fs2" %% "fs2-core" % "3.12.0", "co.fs2" %% "fs2-io" % "3.12.0"))

lazy val tapirVersion = "1.11.10"
lazy val http4sVersion = "0.23.30"
lazy val circeVersion = "0.14.10"

lazy val tapirSettings = Seq(
  libraryDependencies ++= Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % tapirVersion,
    "org.http4s" %% "http4s-ember-server" % http4sVersion,
    "org.http4s" %% "http4s-ember-client" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test,
  ),
)

lazy val utility = (project in file("cyfra-utility"))
  .settings(commonSettings)

lazy val spirvTools = (project in file("cyfra-spirv-tools"))
  .settings(commonSettings)
  .dependsOn(utility)

lazy val vulkan = (project in file("cyfra-vulkan"))
  .settings(commonSettings)
  .dependsOn(utility)

lazy val dsl = (project in file("cyfra-dsl"))
  .settings(commonSettings)
  .dependsOn(utility)

lazy val compiler = (project in file("cyfra-compiler"))
  .settings(commonSettings)
  .dependsOn(dsl, utility)

lazy val core = (project in file("cyfra-core"))
  .settings(commonSettings)
  .dependsOn(compiler, dsl, utility, spirvTools)

lazy val runtime = (project in file("cyfra-runtime"))
  .settings(commonSettings)
  .dependsOn(core, vulkan)

lazy val foton = (project in file("cyfra-foton"))
  .settings(commonSettings)
  .dependsOn(compiler, dsl, runtime, utility)

lazy val fluids = (project in file("cyfra-fluids"))
  .settings(commonSettings, runnerSettings)
  .dependsOn(foton, runtime, dsl, utility)

lazy val analytics = (project in file("cyfra-analytics"))
  .settings(commonSettings, runnerSettings, fs2Settings, tapirSettings)
  .dependsOn(foton, runtime, dsl, utility, fs2interop)

lazy val examples = (project in file("cyfra-examples"))
  .settings(commonSettings, runnerSettings)
  .settings(libraryDependencies += "org.scala-lang.modules" % "scala-parallel-collections_3" % "1.2.0")
  .dependsOn(foton)

lazy val vscode = (project in file("cyfra-vscode"))
  .settings(commonSettings)
  .dependsOn(foton)

lazy val fs2interop = (project in file("cyfra-fs2"))
  .settings(commonSettings, fs2Settings)
  .dependsOn(runtime)

lazy val e2eTest = (project in file("cyfra-e2e-test"))
  .settings(commonSettings, runnerSettings)
  .dependsOn(runtime, fs2interop, foton)

lazy val root = (project in file("."))
  .settings(name := "Cyfra")
  .settings(publish / skip := true)
  .aggregate(compiler, dsl, foton, core, runtime, vulkan, examples, fs2interop, fluids, analytics, utility, spirvTools, vscode)

e2eTest / Test / javaOptions ++= Seq("-Dorg.lwjgl.system.stackSize=1024", "-DuniqueLibraryNames=true")

e2eTest / Test / fork := true

lazy val formatAll = taskKey[Unit]("Rewrites and formats all of the code for passing in CI")
lazy val formatCheckAll = taskKey[Unit]("Fails if a any code is mis-formatted. Does not write to files.")

formatAll := {
  (Compile / scalafmtSbt).value
  scalafmtAll.all(ScopeFilter(inAnyProject)).value
}

formatCheckAll := {
  (Compile / scalafmtSbtCheck).value
  scalafmtCheckAll.all(ScopeFilter(inAnyProject)).value
}

inThisBuild(List(
  organization := "io.computenode",
  homepage := Some(url("https://github.com/ComputeNode/cyfra")),
  licenses := List("LGPL-2.1" -> url("http://www.gnu.org/licenses/lgpl-2.1.html")),
  developers := List(
    Developer(
      id = "szymon-rd",
      name = "Szymon Rodziewicz",
      email = "xjacadev@gmail.com",
      url = url("https://github.com/szymon-rd")
    ),
    Developer(
      id = "MarconZet",
      name = "Marcin Złakowski",
      email = "25779550+MarconZet@users.noreply.github.com",
      url = url("https://github.com/MarconZet")
    )
  )
))

// Don't publish these projects
examples / publish / skip := true
e2eTest / publish / skip := true
vscode / publish / skip := true
fluids / publish / skip := true
analytics / publish / skip := true
