ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

enablePlugins(ScalaNativePlugin)

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

// import to add Scala Native options
import scala.scalanative.build._

name := "soon"

// defaults set with common options shown
nativeConfig ~= { c =>
  c.withLTO(LTO.thin)
    .withMode(Mode.debug)
    .withGC(GC.immix)
    .withBuildTarget(BuildTarget.application)
//.withLinkingOptions(c.linkingOptions ++ Seq("-fuse-ld=mold"))
}

nativeLinkingOptions += "-static"
nativeCompileOptions += "-static"

libraryDependencies ++= List(
  "com.monovore"   %%% "decline"                 % "2.4.1",
  "org.typelevel"  %%% "cats-effect-cps"         % "0.4.0",
  "org.typelevel"  %%% "cats-parse"              % "1.0.0",
  "org.typelevel"  %%% "cats-effect"             % "3.6.1",
  "io.circe"       %%% "circe-parser"            % "0.14.8",
  "io.circe"       %%% "circe-generic"           % "0.14.8",
  "net.andimiller" %%% "cats-parse-interpolator" % "0.1.0",
  "net.andimiller" %%% "decline-completion"      % "0.0.3",
  "co.fs2"         %%% "fs2-io"                  % "3.13.0-M3",
  "com.lihaoyi"    %%% "fansi"                   % "0.4.0",
  "org.typelevel"  %%% "munit-cats-effect"       % "2.1.0"     % Test,
  "org.scalameta"  %%% "munit-scalacheck"        % "1.0.0-M11" % Test,
  "org.typelevel"  %%% "cats-effect-testkit"     % "3.6.1"     % Test
)

val stageBinary = taskKey[Unit]("Copy the binary to the top level")

stageBinary := {
  IO.copyFile((Compile / nativeLink).value, file("soon"))
}
