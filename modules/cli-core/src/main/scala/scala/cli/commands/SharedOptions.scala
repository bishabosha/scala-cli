package scala.cli.commands

import java.io.File

import caseapp._
import caseapp.core.help.Help
import coursier.cache.FileCache

import scala.build.bloop.bloopgun
import scala.build.{Bloop, BuildOptions, LocalRepo, Os}
import scala.build.internal.{CodeWrapper, Constants, CustomCodeClassWrapper}
import scala.scalanative.{build => sn}
import scala.util.Properties
import scala.build.Inputs
import java.io.InputStream
import scala.build.Logger
import java.io.ByteArrayOutputStream

final case class SharedOptions(
  @Recurse
    logging: LoggingOptions = LoggingOptions(),
  @Recurse
    js: ScalaJsOptions = ScalaJsOptions(),
  @Recurse
    native: ScalaNativeOptions = ScalaNativeOptions(),

  @Recurse
    directories: SharedDirectoriesOptions = SharedDirectoriesOptions(),

  @Group("Scala")
  @HelpMessage("Set Scala version")
  @ValueDescription("version")
  @Name("scala")
  @Name("S")
    scalaVersion: Option[String] = None,
  @Group("Scala")
  @HelpMessage("Set Scala binary version")
  @ValueDescription("version")
  @Name("scalaBinary")
  @Name("scalaBin")
  @Name("B")
    scalaBinaryVersion: Option[String] = None,

  @Group("Java")
  @HelpMessage("Set Java home")
  @ValueDescription("path")
    javaHome: Option[String] = None,

  @Group("Java")
  @HelpMessage("Use a specific JVM, such as 14, adopt:11, or graalvm:21, or system")
  @ValueDescription("jvm-name")
  @Name("j")
    jvm: Option[String] = None,

  @Group("Java")
  @HelpMessage("Add extra JARs in the class path")
  @ValueDescription("paths")
  @Name("jar")
  @Name("jars")
  @Name("extraJar")
    extraJars: List[String] = Nil,

  @Hidden
    classWrap: Boolean = false,

  @HelpMessage("Watch sources for changes")
  @Name("w")
    watch: Boolean = false,

  @Group("Scala")
  @Hidden
    scalaLibrary: Option[Boolean] = None,
  @Group("Java")
  @Hidden
    java: Option[Boolean] = None,
  @Hidden
    runner: Option[Boolean] = None,

  @HelpMessage("Pass configuration files")
  @Name("conf")
  @Name("C")
    config: List[String] = Nil,

  @HelpMessage("Generate SemanticDBs")
    semanticDb: Option[Boolean] = None,
  @Hidden
    addStubs: Option[Boolean] = None
) {

  def logger = logging.logger

  private def codeWrapper: Option[CodeWrapper] =
    if (classWrap) Some(CustomCodeClassWrapper)
    else None

  def nativeWorkDir(root: os.Path, projectName: String) = root / ".scala" / projectName / "native"

  def scalaNativeLogger: sn.Logger =
    new sn.Logger {
      def trace(msg: Throwable) = ()
      def debug(msg: String) = logger.debug(msg)
      def info(msg: String) = logger.log(msg)
      def warn(msg: String) = logger.log(msg)
      def error(msg: String) = logger.log(msg)
    }

  def buildOptions(jmhOptions: Option[BuildOptions.RunJmhOptions], jmhVersion: Option[String]): BuildOptions =
    BuildOptions(
      scalaVersion = scalaVersion.map(_.trim).filter(_.nonEmpty),
      scalaBinaryVersion = scalaBinaryVersion.map(_.trim).filter(_.nonEmpty),
      codeWrapper = codeWrapper,
      scalaJsOptions = js.buildOptions,
      scalaNativeOptions = native.buildOptions,
      javaHomeOpt = javaHome.filter(_.nonEmpty),
      jvmIdOpt = jvm.filter(_.nonEmpty),
      addStubsDependencyOpt = addStubs,
      addJmhDependencies =
        if (jmhOptions.nonEmpty) jmhVersion.orElse(Some(Constants.jmhVersion))
        else None,
      runJmh = jmhOptions,
      addScalaLibrary = scalaLibrary.orElse(java.map(!_)),
      addRunnerDependencyOpt = runner,
      generateSemanticDbs = semanticDb,
      extraJars = extraJars.flatMap(_.split(File.pathSeparator).toSeq).filter(_.nonEmpty).map(os.Path(_, os.pwd)),
      extraRepositories = LocalRepo.localRepo(directories.directories.localRepoDir).toSeq,
      cache = Some(coursierCache)
    )

  // This might download a JVM if --jvm … is passed or no system JVM is installed
  def bloopgunConfig(): bloopgun.BloopgunConfig =
    bloopgun.BloopgunConfig.default(() => Bloop.bloopClassPath(logging.logger)).copy(
      javaPath = buildOptions(None, None).javaCommand()
    )

  lazy val coursierCache = FileCache().withLogger(logging.logger.coursierLogger)

  def inputsOrExit(args: RemainingArgs, defaultInputs: Option[Inputs] = None): Inputs =
    Inputs(args.remaining, Os.pwd, directories.directories, defaultInputs = defaultInputs, stdinOpt = SharedOptions.readStdin(logger = logger), acceptFds = !Properties.isWin) match {
      case Left(message) =>
        System.err.println(message)
        sys.exit(1)
      case Right(i) =>
        val configFiles = config.map(os.Path(_, Os.pwd)).map(Inputs.ConfigFile(_))
        i.add(configFiles)
    }
}

object SharedOptions {
  implicit val parser = Parser[SharedOptions]
  implicit val help = Help[SharedOptions]

  def readStdin(in: InputStream = System.in, logger: Logger): Option[Array[Byte]] =
    if (in == null) {
      logger.debug("No stdin available")
      None
    } else {
      logger.debug("Reading stdin")
      val baos = new ByteArrayOutputStream
      val buf = Array.ofDim[Byte](16*1024)
      var read = -1
      while ({
        read = in.read(buf)
        read >= 0
      }) {
        if (read > 0)
          baos.write(buf, 0, read)
      }
      val result = baos.toByteArray
      logger.debug(s"Done reading stdin (${result.length} B)")
      Some(result)
    }

}
