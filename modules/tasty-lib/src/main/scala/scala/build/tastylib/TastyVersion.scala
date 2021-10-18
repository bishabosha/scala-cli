/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

// Originally adapted from https://github.com/scala/scala/blob/20ac944346a93ba747811e80f8f67a09247cb987/src/compiler/scala/tools/tasty/TastyFormat.scala

package scala.build.tastylib

case class TastyVersion(major: Int, minor: Int, experimental: Int) { tool =>

  def canRead(fileMajor: Int, fileMinor: Int, fileExperimental: Int) =
    TastyFormat.isVersionCompatible(
      fileMajor,
      fileMinor,
      fileExperimental,
      tool.major,
      tool.minor,
      tool.experimental
    )
}

object TastyVersion {

  private val Scala3Version = raw"3.(\d+?).(d+?)(.*)".r
  private val PreRelease = raw"-(?:RC|M)\d+".r

  /** This method parses a TastyVersion from a Scala version string, according
   *  to the scheme defined at https://github.com/lampepfl/dotty/issues/13447.
   */
  def parse(scalaVersion: String): Option[TastyVersion] = {
    val (minor, exp) = scalaVersion match {
      case Scala3Version(minorStr, patch, extra) =>
        def isPreReleaseMinor =
          PartialFunction.cond(extra) { case PreRelease() => patch == "0" }
        val minor0 = minorStr.toInt
        val isNightly = extra.contains("NIGHTLY")
        val exp =
          if (isNightly) 1 // All nightlies have experimental 1
          else if (isPreReleaseMinor) 2 // A pre-release for a minor bump has experimental 2
          else 0 // final releases, or a pre-release for a patch bump, have experimental 0
        val minor =
          if (isNightly) minor0 + 1 else minor0 // nighlies preview the "next" minor version
        (minor, exp)

      case _ => return None
    }
    Some(TastyVersion(28, minor, exp)) // hard code Major until Scala `3.T`
  }
}
