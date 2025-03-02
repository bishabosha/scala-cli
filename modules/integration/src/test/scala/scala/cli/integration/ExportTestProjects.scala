package scala.cli.integration

import com.eed3si9n.expecty.Expecty.expect

object ExportTestProjects {

  def jvmTest(scalaVersion: String): TestInputs = {

    val testFile =
      if (scalaVersion.startsWith("3."))
        s"""using scala $scalaVersion
           |using org.scala-lang::scala3-compiler:$scalaVersion
           |
           |object Test {
           |  def main(args: Array[String]): Unit = {
           |    val message = "Hello from " + dotty.tools.dotc.config.Properties.simpleVersionString
           |    println(message)
           |  }
           |}
           |""".stripMargin
      else
        s"""using scala $scalaVersion
           |
           |object Test {
           |  def main(args: Array[String]): Unit = {
           |    val message = "Hello from " + scala.util.Properties.versionNumberString
           |    println(message)
           |  }
           |}
           |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala" -> testFile
      )
    )
  }

  def jsTest(scalaVersion: String): TestInputs = {

    val testFile =
      if (scalaVersion.startsWith("3."))
        s"""using scala $scalaVersion
           |using scala-js
           |
           |import scala.scalajs.js
           |
           |object Test:
           |  def main(args: Array[String]): Unit =
           |    val console = js.Dynamic.global.console
           |    console.log("Hello from " + "exported Scala CLI project")
           |""".stripMargin
      else
        s"""using scala $scalaVersion
           |using scala-js
           |
           |import scala.scalajs.js
           |
           |object Test {
           |  def main(args: Array[String]): Unit = {
           |    val console = js.Dynamic.global.console
           |    console.log("Hello from " + "exported Scala CLI project")
           |  }
           |}
           |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala" -> testFile
      )
    )
  }

  def nativeTest(scalaVersion: String): TestInputs = {
    val nl = "\\n"
    val testFile =
      if (scalaVersion.startsWith("3."))
        s"""using scala $scalaVersion
           |using scala-native
           |
           |import scala.scalanative.libc._
           |import scala.scalanative.unsafe._
           |
           |object Test:
           |  def main(args: Array[String]): Unit =
           |    val message = "Hello from " + "exported Scala CLI project" + "$nl"
           |    Zone { implicit z =>
           |      stdio.printf(toCString(message))
           |    }
           |""".stripMargin
      else
        s"""using scala $scalaVersion
           |using scala-native
           |
           |import scala.scalanative.libc._
           |import scala.scalanative.unsafe._
           |
           |object Test {
           |  def main(args: Array[String]): Unit = {
           |    val message = "Hello from " + "exported Scala CLI project" + "$nl"
           |    Zone { implicit z =>
           |      stdio.printf(toCString(message))
           |    }
           |  }
           |}
           |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala" -> testFile
      )
    )
  }

  def repositoryScala3Test(scalaVersion: String): TestInputs = {
    val testFile =
      s"""using scala $scalaVersion
         |using com.github.jupyter:jvm-repr:0.4.0
         |using repository jitpack
         |import jupyter._
         |object Test:
         |  def main(args: Array[String]): Unit =
         |    val message = "Hello from " + "exported Scala CLI project"
         |    println(message)
         |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala" -> testFile
      )
    )
  }

  def mainClassScala3Test(scalaVersion: String): TestInputs = {
    val testFile =
      s"""using scala $scalaVersion
         |
         |object Test:
         |  def main(args: Array[String]): Unit =
         |    val message = "Hello from " + "exported Scala CLI project"
         |    println(message)
         |""".stripMargin
    val otherTestFile =
      s"""object Other:
         |  def main(args: Array[String]): Unit =
         |    val message = "Hello from " + "other file"
         |    println(message)
         |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala"  -> testFile,
        os.rel / "Other.scala" -> otherTestFile
      )
    )
  }

  def scalacOptionsScala2Test(scalaVersion: String): TestInputs = {
    val testFile =
      s"""using scala $scalaVersion
         |using org.scala-lang.modules::scala-async:0.10.0
         |using org.scala-lang:scala-reflect:$scalaVersion
         |import scala.async.Async.{async, await}
         |import scala.concurrent.{Await, Future}
         |import scala.concurrent.duration.Duration
         |import scala.concurrent.ExecutionContext.Implicits.global
         |
         |object Test {
         |  def main(args: Array[String]): Unit = {
         |    val messageF = Future.successful(
         |      "Hello from " + "exported Scala CLI project"
         |    )
         |    val f = async {
         |      val message = await(messageF)
         |      println(message)
         |    }
         |    Await.result(f, Duration.Inf)
         |  }
         |}
         |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala" -> testFile
      )
    )
  }

  def pureJavaTest: TestInputs = {
    val testFile =
      s"""public class ScalaCliJavaTest {
         |  public static void main(String[] args) {
         |    String className = "scala.concurrent.ExecutionContext";
         |    ClassLoader cl = Thread.currentThread().getContextClassLoader();
         |    boolean found = true;
         |    try {
         |      cl.loadClass(className);
         |    } catch (ClassNotFoundException ex) {
         |      found = false;
         |    }
         |    if (found) {
         |      throw new RuntimeException("Didn't expect " + className + " to be in class path.");
         |    }
         |    System.out.println("Hello from " + "exported Scala CLI project");
         |  }
         |}
         |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "ScalaCliJavaTest.java" -> testFile
      )
    )
  }

  def testFrameworkTest(scalaVersion: String): TestInputs = {
    val testFile =
      s"""using scala $scalaVersion
         |using "com.lihaoyi::utest:0.7.10"
         |using test-framework "utest.runner.Framework"
         |
         |import utest._
         |
         |object MyTests extends TestSuite {
         |  val tests = Tests {
         |    test("foo") {
         |      assert(2 + 2 == 4)
         |      println("Hello from " + "exported Scala CLI project")
         |    }
         |  }
         |}
         |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "MyTests.scala" -> testFile
      )
    )
  }

  def customJarTest(scalaVersion: String): TestInputs = {
    val shapelessJar = {
      val res = os.proc(
        TestUtil.cs,
        "fetch",
        "--intransitive",
        "com.chuusai::shapeless:2.3.7",
        "--scala",
        scalaVersion
      )
        .call()
      val path  = res.out.text().trim
      val path0 = os.Path(path, os.pwd)
      expect(os.isFile(path0))
      path0
    }
    val shapelessJarStr =
      "\"" + shapelessJar.toString.replace("\\", "\\\\") + "\""
    val testFile =
      s"""using scala $scalaVersion
         |using jar $shapelessJarStr
         |
         |import shapeless._
         |
         |object Test {
         |  def main(args: Array[String]): Unit = {
         |    val l = "exported Scala CLI project" :: 2 :: true :: HNil
         |    val messageEnd: String = l.head
         |    println("Hello from " + messageEnd)
         |  }
         |}
         |""".stripMargin
    TestInputs(
      Seq(
        os.rel / "Test.scala" -> testFile
      )
    )
  }
}
