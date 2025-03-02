package scala.cli.commands

import caseapp.{Group, Help, HelpMessage, Name, Parser, ValueDescription}

// format: off
final case class PackagerOptions(
  @HelpMessage("The version of generated package")
    version: String = "1.0.0",
  @HelpMessage(
    "Path to application logo in png format, it will be used to generate icon and banner/dialog in msi installer"
  )
    logoPath: Option[String] = None,
  @HelpMessage("Set launcher app name which will be linked to PATH")
    launcherApp: Option[String] = None,
  @ValueDescription("Description")
    description: Option[String] = None,
  @HelpMessage("It should contains names and email addresses of co-maintainers of the package")
  @Name("m")
    maintainer: Option[String] = None,
  @Group("Debian")
  @HelpMessage(
    "The list of debian package that this package is absolute incompatibility"
  )
  @ValueDescription("debian dependencies conflicts")
    debianConflicts: List[String] = Nil,
  @Group("Debian")
  @HelpMessage("The list of debian package that this package depends on")
  @ValueDescription("debian dependencies")
    debianDependencies: List[String] = Nil,
  @Group("Debian")
  @HelpMessage(
    "Architecture that are supported by the repository, default: all"
  )
    debArchitecture: String = "all",
  @Group("MacOS")
  @HelpMessage(
  "CF Bundle Identifier"
  )
    identifier: Option[String] = None,
  @Group("RedHat")
  @HelpMessage(
    "License that are supported by the repository - list of licenses https://fedoraproject.org/wiki/Licensing:Main?rd=Licensing"
  )
    license: Option[String] = None,
  @Group("RedHat")
  @HelpMessage(
    "The number of times this version of the software was released, default: 1"
  )
    release: String = "1",
  @HelpMessage("Architecture that are supported by the repository, default: noarch")
    rpmArchitecture: String = "noarch",
  @Group("Windows")
  @HelpMessage("Path to license file")
    licensePath: Option[String] = None,
  @Group("Windows")
  @HelpMessage("Name of product, default: Scala packager")
    productName: String = "Scala packager",
  @Group("Windows")
  @HelpMessage("Text will be displayed on exit dialog")
    exitDialog: Option[String] = None,
  @Group("Windows")
  @HelpMessage("Suppress Wix ICE validation (required for users that are neither interactive, not local administrators)")
    suppressValidation: Option[Boolean] = None,
  @Group("Windows")
  @HelpMessage("Path to extra WIX config content")
  @ValueDescription("path")
    extraConfig: List[String] = Nil,
  @Group("Windows")
  @HelpMessage("Whether a 64-bit executable is getting packaged")
  @Name("64")
    is64Bits: Boolean = true,
  @Group("Windows")
  @HelpMessage("WIX installer version")
    installerVersion: Option[String] = None,
  @Group("Docker")
  @HelpMessage(
    "Building the container from base image"
  )
  dockerFrom: Option[String] = None,
  @Group("Docker")
  @HelpMessage(
    "The image registry, if will be empty it will be used default registry"
  )
  dockerImageRegistry: Option[String] = None,
  @Group("Docker")
  @HelpMessage(
    "The image repository"
  )
  dockerImageRepository: Option[String] = None,
  @Group("Docker")
  @HelpMessage(
    "The image tag, the default tag is latest"
  )
  dockerImageTag: Option[String] = None,
)
// format: on

object PackagerOptions {
  lazy val parser: Parser[PackagerOptions]                           = Parser.derive
  implicit lazy val parserAux: Parser.Aux[PackagerOptions, parser.D] = parser
  implicit lazy val help: Help[PackagerOptions]                      = Help.derive
}
