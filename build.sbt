ThisBuild / organization := "com.computenode.cyfra"
ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val osName = System.getProperty("os.name").toLowerCase
lazy val osArch = System.getProperty("os.arch")
lazy val lwjglNatives = {
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
}

val lwjglVersion = "3.3.3"
val jomlVersion = "1.10.0"

lazy val root = (project in file("."))
  .settings(
    name := "Cyfra",
    libraryDependencies ++= Seq(
      "dev.zio" % "izumi-reflect_3" % "2.3.10",
      "com.lihaoyi" % "pprint_3" % "0.9.0",
      "com.diogonunes" % "JColor" % "5.5.1",
      "org.lwjgl" % "lwjgl" % lwjglVersion,
      "org.lwjgl" % "lwjgl-vulkan" % lwjglVersion,
      "org.lwjgl" % "lwjgl-vma" % lwjglVersion,
      "org.lwjgl" % "lwjgl" % lwjglVersion classifier lwjglNatives,
      "org.lwjgl" % "lwjgl-vma" % lwjglVersion classifier lwjglNatives,
      "org.joml" % "joml" % jomlVersion,
      "commons-io" % "commons-io" % "2.16.1",
      "org.slf4j" % "slf4j-api" % "1.7.30",
      "org.slf4j" % "slf4j-simple" % "1.7.30" % Test,
      "org.scalameta" % "munit_3" % "1.0.0" % Test,
      "org.junit.jupiter" % "junit-jupiter" % "5.6.2" % Test,
      "org.junit.jupiter" % "junit-jupiter-engine" % "5.7.2" % Test,
      "com.lihaoyi" %% "sourcecode" % "0.4.3-M5"
    )
  )

// lazy val vulkanSdk = System.getenv("VULKAN_SDK")
// javaOptions +=  s"-Dorg.lwjgl.vulkan.libname=$vulkanSdk/lib/libvulkan.1.dylib"
