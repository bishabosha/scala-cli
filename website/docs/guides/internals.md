---
title: Internals
sidebar_position: 15
---

Even though Scala CLI exposes a simple interface to users, quite a number of steps happen
under-the-hood when compiling or running even a single source file.


### Bloop

Scala CLI uses Bloop to compile code. That way, it doesn't interface directly with scalac / dotty,
and newly released Scala versions work out-of-the-box - no need to update Scala CLI itself.

In more detail, Scala CLI connects to Bloop on the local machine via the default Bloop port (`8212`).
If no Bloop instance listens on this port, Scala CLI fetches Bloop (via coursier), and starts it.

Right now, Scala CLI connects to Bloop via an open TCP port on your local machine, although this may change in
the future for security reasons (see [`scalacenter/bloop#1562`](https://github.com/scalacenter/bloop/pull/1562)).

Once connected to Bloop, Scala CLI writes a Bloop project file (under `.scala/.bloop`), describing
the current Scala CLI project (its Scala version, its dependencies, its compiler plugins and options, etc.).

It then initiates a [BSP](https://github.com/build-server-protocol/build-server-protocol) connection with Bloop
Even though the request to open a BSP connection is initiated on the TCP port above, BSP communication itself
happens on a domain socket (Linux / macOS) or named pipe (Windows).

That BSP connection then allows Scala CLI to ask Bloop to compile sources, and get diagnostics (warnings / errors)
and byte code.

### `.scala` directory

Scala CLI uses a directory named `.scala` to write:
- [Bloop project files](#bloop)
- [generated sources](#preprocessing)
- byte code and TASTy files resulting from compiling the user sources

The typical content of `.scala` looks like
```text
.scala
├── .bloop
│   ├── project_940fb43dce
│   │   ├── bloop-internal-classes
│   │   │   └── main-ZWP3jgllS6y93V4HoGYa2g==
│   │   │       ├── test$.class
│   │   │       ├── test.class
│   │   │       ├── test.tasty
│   │   │       ├── test_sc$.class
│   │   │       ├── test_sc.class
│   │   │       └── test_sc.tasty
│   │   └── project_940fb43dce-analysis.bin
│   ├── project_940fb43dce.json
│   └── project_f643cb0bc2-test.json
└── project_940fb43dce
    ├── classes
    │   └── main
    │       ├── test$.class
    │       ├── test.class
    │       ├── test.tasty
    │       ├── test_sc$.class
    │       ├── test_sc.class
    │       └── test_sc.tasty
    └── src_generated
        └── main
            └── test.scala
```

In particular, `.scala/.bloop` contains Bloop project files and Bloop's own working directories, and
`.scala/project_*` contains byte code and TASTy files, and generated sources.

## Preprocessing

Some sources that Scala CLI accepts cannot be passed as is to scalac / dotty. That's the case for
- `.sc` files, that can contain top-level definitions not accepted by scalac or dotty
- `.scala` files with non-commented `using` directives
- `.sc` or `.scala` files with `import $ivy`s.

In all of those cases, Scala CLI parses the top of those files, and looks for `import $ivy` or `using` directives.
It then replaces those either by dummy imports (for the `import $ivy`s) or space characters
(for non-commented `using` directives).

`.sc` files are also "wrapped" in an `object`, and a `main` class is added to them, so that `.sc` files
can be run as is and can access arguments via `args`.

The resulting processed sources are written in the `.scala/project_…/src_generated` directory, and passed
to Bloop from there.

## Postprocessing

Because of [preprocessing](#preprocessing), some outputs we get from scalac / dotty might not match
the original sources. Processed sources might have shifted line numbers (for `.sc` files, because
of the wrapping in an `object`), or wrong relative paths (as they're written in `src_generated`).

For those files, most outputs from scalac / dotty is postprocessed, to match the original sources. That
includes:
- diagnostics (errors / warning, whose file names and line / column numbers are adjusted)
- byte code (whole line numbers, reported in exception stack traces or used by debuggers, needs to be shifted)
- semantic DBs (used for IDE support, whose path fields and positions need to be adjusted)
- TASTy files (whole path fields need to be adjusted)

## Runner

When running your code, if your code crashes, Scala CLI processes the stack traces of the exception to make them
more readable. This is achieved by adding to the class path a module (called `runner`), that is actually used as
entry point. The [`Runner` class](https://github.com/VirtusLab/scala-cli/blob/60eae701abc74bdd634efa5157740578bd6c4162/modules/runner/src/main/scala/scala/cli/runner/Runner.scala)
of the runner module starts your main class, catches any exception it might throw,
and pretty-prints it. (With Scala 3, pretty printing of stack traces is handled by the
[pretty-stacktraces](https://github.com/Virtuslab/pretty-stacktraces) library.)

## Logging

To have a glimpse at what Scala CLI is doing, increase its verbosity with `-v`. `-v` can be specified up to 3 times,
and prints numerous details. These details are mostly meant to help debug issues.
When reporting bugs, increasing the verbosity to its maximum can be helpful.

```text
$ scala-cli . -v
Running /Users/alexandre/Library/Caches/Coursier/jvm/adopt@1.11.0-7/Contents/Home/bin/java -cp /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar:/Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/runner_3/0.0.5+43-g60eae701-SNAPSHOT/jars/runner_3.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/oss.sonatype.org/content/repositories/snapshots/org/virtuslab/pretty-stacktraces_3/0.0.0%2B27-b9d69198-SNAPSHOT/pretty-stacktraces_3-0.0.0%2B27-b9d69198-SNAPSHOT.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-tasty-inspector_3/3.0.0/scala3-tasty-inspector_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.0/scala3-compiler_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.0/scala3-interfaces-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.0/tasty-core_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar:/Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar scala.cli.runner.Runner test_sc
Hello
```

```text
$ scala-cli . -v -v
Fetching List(ch.epfl.scala:bloop-frontend_2.12:1.4.8-124-49a6348a)
Found 127 artifacts:
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-frontend_2.12/1.4.8-124-49a6348a/bloop-frontend_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.12.13/scala-library-2.12.13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/sockets/1.4.8-124-49a6348a/sockets-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-shared_2.12/1.4.8-124-49a6348a/bloop-shared_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-backend_2.12/1.4.8-124-49a6348a/bloop-backend_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-config_2.12/1.4.8-124-49a6348a/bloop-config_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalaz/scalaz-core_2.12/7.2.20/scalaz-core_2.12-7.2.20.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix_2.12/2.3.3/monix_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/case-app_2.12/2.0.6/case-app_2.12-2.0.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/scala-debug-adapter_2.12/1.1.3/scala-debug-adapter_2.12-1.1.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.8.0/jna-5.8.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna-platform/5.8.0/jna-platform-5.8.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bsp4s_2.12/2.0.0-M13/bsp4s_2.12-2.0.0-M13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc_2.12/1.3.0-M4%2B46-edbe573e/zinc_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/jpountz/lz4/lz4/1.3.0/lz4-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/soc/directories/10/directories-10.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/test-interface/1.0/test-interface-1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/test-agent/1.4.4/test-agent-1.4.4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/googlecode/java-diff-utils/diffutils/1.3.0/diffutils-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/pprint_2.12/0.5.3/pprint_2.12-0.5.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/com-microsoft-java-debug-core/0.21.0%2B1-7f1080f1/com-microsoft-java-debug-core-0.21.0%2B1-7f1080f1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/nailgun-server/ee3c4343/nailgun-server-ee3c4343.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalaz/scalaz-concurrent_2.12/7.2.20/scalaz-concurrent_2.12-7.2.20.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier_2.12/2.0.16/coursier_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier-cache_2.12/2.0.16/coursier-cache_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/librarymanagement-ivy_2.12/1.0.0/librarymanagement-ivy_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/sourcecode_2.12/0.1.4/sourcecode_2.12-0.1.4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/directory-watcher/0.8.0%2B6-f651bd93/directory-watcher-0.8.0%2B6-f651bd93.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/zeroturnaround/zt-zip/1.13/zt-zip-1.13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/brave/brave/5.6.1/brave-5.6.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/reporter2/zipkin-sender-urlconnection/2.7.15/zipkin-sender-urlconnection-2.7.15.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm/9.2/asm-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm-util/9.2/asm-util-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/plokhotnyuk/jsoniter-scala/jsoniter-scala-core_2.12/2.4.0/jsoniter-scala-core_2.12-2.4.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-types_2.12/2.3.3/monix-types_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-execution_2.12/2.3.3/monix-execution_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-eval_2.12/2.3.3/monix-eval_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-reactive_2.12/2.3.3/monix-reactive_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/case-app-annotations_2.12/2.0.6/case-app-annotations_2.12-2.0.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/case-app-util_2.12/2.0.6/case-app-util_2.12-2.0.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-core_2.12/0.9.3/circe-core_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-derivation_2.12/0.9.0-M4/circe-derivation_2.12-0.9.0-M4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/lsp4s_2.12/0.2.0/lsp4s_2.12-0.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-core_2.12/1.3.0-M4%2B46-edbe573e/zinc-core_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-persist_2.12/1.3.0-M4%2B46-edbe573e/zinc-persist_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-compile-core_2.12/1.3.0-M4%2B46-edbe573e/zinc-compile-core_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-classfile_2.12/1.3.0-M4%2B46-edbe573e/zinc-classfile_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fansi_2.12/0.2.5/fansi_2.12-0.2.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.6/commons-lang3-3.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/code/gson/gson/2.7/gson-2.7.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/reactivex/rxjava2/rxjava/2.1.1/rxjava-2.1.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/reactivestreams/reactive-streams/1.0.0/reactive-streams-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/commons-io/commons-io/2.5/commons-io-2.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.26/slf4j-api-1.7.26.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalaz/scalaz-effect_2.12/7.2.20/scalaz-effect_2.12-7.2.20.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier-core_2.12/2.0.16/coursier-core_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/argonaut-shapeless_6.2_2.12/1.2.0/argonaut-shapeless_6.2_2.12-1.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier-util_2.12/2.0.16/coursier-util_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/alexarchambault/windows-ansi/windows-ansi/0.0.3/windows-ansi-0.0.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/librarymanagement-core_2.12/1.0.0/librarymanagement-core_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/sjson-new-core_2.12/0.8.2/sjson-new-core_2.12-0.8.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/ivy/ivy/2.3.0-sbt-a3314352b638afbf0dca19f127e8263ed6f898bd/ivy-2.3.0-sbt-a3314352b638afbf0dca19f127e8263ed6f898bd.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/zipkin2/zipkin/2.12.1/zipkin-2.12.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/reporter2/zipkin-reporter/2.7.15/zipkin-reporter-2.7.15.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm-tree/9.2/asm-tree-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm-analysis/9.2/asm-analysis-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jctools/jctools-core/2.0.1/jctools-core-2.0.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/chuusai/shapeless_2.12/2.3.3/shapeless_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-numbers_2.12/0.9.3/circe-numbers_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-core_2.12/1.1.0/cats-core_2.12-1.1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/jsonrpc_2.12/0.2.0/jsonrpc_2.12-0.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/beachape/enumeratum_2.12/1.5.13/enumeratum_2.12-1.5.13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/beachape/enumeratum-circe_2.12/1.5.17/enumeratum-circe_2.12-1.5.17.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-apiinfo_2.12/1.3.0-M4%2B46-edbe573e/zinc-apiinfo_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-classpath_2.12/1.3.0-M4%2B46-edbe573e/zinc-classpath_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/compiler-interface/1.3.0-M4%2B46-edbe573e/compiler-interface-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/thesamet/scalapb/scalapb-runtime_2.12/0.8.0-RC1/scalapb-runtime_2.12-0.8.0-RC1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/io_2.12/1.2.0/io_2.12-1.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-logging_2.12/1.2.2/util-logging_2.12-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-relation_2.12/1.2.2/util-relation_2.12-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/sbinary_2.12/0.5.0/sbinary_2.12-0.5.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/launcher-interface/1.0.0/launcher-interface-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-parser-combinators_2.12/1.0.5/scala-parser-combinators_2.12-1.0.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-control_2.12/1.2.2/util-control_2.12-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/alexarchambault/concurrent-reference-hash-map/1.0.0/concurrent-reference-hash-map-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.12/1.3.0/scala-xml_2.12-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/argonaut/argonaut_2.12/6.2.5/argonaut_2.12-6.2.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-collection-compat_2.12/2.2.0/scala-collection-compat_2.12-2.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/fusesource/jansi/jansi/1.18/jansi-1.18.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.12.11/scala-compiler-2.12.11.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/jcraft/jsch/0.1.46/jsch-0.1.46.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.12.11/scala-reflect-2.12.11.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/gigahorse-okhttp_2.12/0.3.0/gigahorse-okhttp_2.12-0.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/squareup/okhttp3/okhttp-urlconnection/3.7.0/okhttp-urlconnection-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-position_2.12/1.0.0/util-position_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-cache_2.12/1.0.0/util-cache_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/macro-compat_2.12/1.1.1/macro-compat_2.12-1.1.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-macros_2.12/1.1.0/cats-macros_2.12-1.1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_2.12/1.1.0/cats-kernel_2.12-1.1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/machinist_2.12/0.6.2/machinist_2.12-0.6.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/scribe_2.12/2.5.0/scribe_2.12-2.5.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-parser_2.12/0.9.3/circe-parser_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/beachape/enumeratum-macros_2.12/1.5.9/enumeratum-macros_2.12-1.5.9.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/compiler-bridge_2.12/1.3.0-M4%2B46-edbe573e/compiler-bridge_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.2.2/util-interface-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/thesamet/scalapb/lenses_2.12/0.8.0-RC1/lenses_2.12-0.8.0-RC1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fastparse_2.12/1.0.0/fastparse_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/swoval/apple-file-events/1.3.2/apple-file-events-1.3.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/jline/jline/2.14.4/jline-2.14.4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/logging/log4j/log4j-api/2.8.1/log4j-api-2.8.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.8.1/log4j-core-2.8.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lmax/disruptor/3.3.6/disruptor-3.3.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/sjson-new-scalajson_2.12/0.8.2/sjson-new-scalajson_2.12-0.8.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/gigahorse-core_2.12/0.3.0/gigahorse-core_2.12-0.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/squareup/okhttp3/okhttp/3.7.0/okhttp-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/sjson-new-murmurhash_2.12/0.8.0/sjson-new-murmurhash_2.12-0.8.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/scribe-macros_2.12/2.5.0/scribe-macros_2.12-2.5.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/perfolation_2.12/1.0.2/perfolation_2.12-1.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-jawn_2.12/0.9.3/circe-jawn_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fastparse-utils_2.12/1.0.0/fastparse-utils_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/shaded-scalajson_2.12/1.0.0-M4/shaded-scalajson_2.12-1.0.0-M4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/spire-math/jawn-parser_2.12/0.11.1/jawn-parser_2.12-0.11.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/ssl-config-core_2.12/0.2.2/ssl-config-core_2.12-0.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/squareup/okio/okio/1.12.0/okio-1.12.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/perfolation-macros_2.12/1.0.2/perfolation-macros_2.12-1.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/config/1.2.0/config-1.2.0.jar

Fetching List(org.scala-lang::scala3-compiler:3.0.2), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 13 artifacts:
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.2/scala3-compiler_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.2/scala3-interfaces-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.2/tasty-core_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

Fetching List(org.scala-lang::scala3-library::3.0.2, org.virtuslab.scala-cli::runner:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Fetching List(org.virtuslab.scala-cli:stubs:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 1 artifacts:
  /Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar

Writing bloop project in /Users/alexandre/projects/scala-cli/test/.scala/.bloop/project_940fb43dce.json
Listing BSP build targets
Compiling project_940fb43dce with Bloop
Received onBuildTaskStart from bloop: TaskStartParams [
  taskId = TaskId [
    id = "1"
    parents = null
  ]
  eventTime = 1634309123019
  message = "Compiling project_940fb43dce (1 Scala source)"
  dataKind = "compile-task"
  data = {"target":{"uri":"file:/Users/alexandre/projects/scala-cli/test/.scala/?id=project_940fb43dce"}}
]
Compiling project (Scala 3.0.2, JVM)
Received onBuildTaskFinish from bloop: TaskFinishParams [
  taskId = TaskId [
    id = "1"
    parents = null
  ]
  eventTime = 1634309127394
  message = "Compiled 'project_940fb43dce'"
  status = OK
  dataKind = "compile-report"
  data = {"target":{"uri":"file:/Users/alexandre/projects/scala-cli/test/.scala/?id=project_940fb43dce"},"originId":null,"errors":0,"warnings":0,"time":null,"isNoOp":false,"isLastCycle":true,"clientDir":"file:///Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/","analysisOut":"file:///Users/alexandre/projects/scala-cli/test/.scala/.bloop/project_940fb43dce/project_940fb43dce-analysis.bin"}
]
Compiled project (Scala 3.0.2, JVM)
Compilation succeeded
Post-processing class files of pre-processed sources
Overwriting .scala/project_940fb43dce/classes/main/test$.class
Overwriting .scala/project_940fb43dce/classes/main/test.class
Overwriting .scala/project_940fb43dce/classes/main/test_sc$.class
Overwriting .scala/project_940fb43dce/classes/main/test_sc.class
Moving semantic DBs around
Reading TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test.tasty
Parsed TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test.tasty
Overwriting .scala/project_940fb43dce/classes/main/test.tasty
Reading TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test_sc.tasty
Parsed TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test_sc.tasty
Overwriting .scala/project_940fb43dce/classes/main/test_sc.tasty
Fetching List(org.scala-lang::scala3-compiler:3.0.2), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 13 artifacts:
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.2/scala3-compiler_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.2/scala3-interfaces-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.2/tasty-core_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

Fetching List(org.scala-lang::scala3-library::3.0.2, org.virtuslab.scala-cli::runner:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Fetching List(org.virtuslab.scala-cli:stubs:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 1 artifacts:
  /Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar

Writing bloop project in /Users/alexandre/projects/scala-cli/test/.scala/.bloop/project_f643cb0bc2-test.json
Listing BSP build targets
Compiling project_f643cb0bc2-test with Bloop
Compilation succeeded
Post-processing class files of pre-processed sources
Moving semantic DBs around
  Running
/Users/alexandre/Library/Caches/Coursier/jvm/adopt@1.11.0-7/Contents/Home/bin/java
-cp
/Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar:/Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/runner_3/0.0.5+43-g60eae701-SNAPSHOT/jars/runner_3.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/oss.sonatype.org/content/repositories/snapshots/org/virtuslab/pretty-stacktraces_3/0.0.0%2B27-b9d69198-SNAPSHOT/pretty-stacktraces_3-0.0.0%2B27-b9d69198-SNAPSHOT.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-tasty-inspector_3/3.0.0/scala3-tasty-inspector_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.0/scala3-compiler_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.0/scala3-interfaces-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.0/tasty-core_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar:/Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar
scala.cli.runner.Runner
test_sc

execve available
Hello
```

```text
$ scala-cli . -v -v -v
Attempting a connection to bloop server 127.0.0.1:8212 ...
No bloop daemon found on 127.0.0.1:8212
Starting bloop server
Fetching List(ch.epfl.scala:bloop-frontend_2.12:1.4.8-124-49a6348a)
Found 127 artifacts:
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-frontend_2.12/1.4.8-124-49a6348a/bloop-frontend_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.12.13/scala-library-2.12.13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/sockets/1.4.8-124-49a6348a/sockets-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-shared_2.12/1.4.8-124-49a6348a/bloop-shared_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-backend_2.12/1.4.8-124-49a6348a/bloop-backend_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bloop-config_2.12/1.4.8-124-49a6348a/bloop-config_2.12-1.4.8-124-49a6348a.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalaz/scalaz-core_2.12/7.2.20/scalaz-core_2.12-7.2.20.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix_2.12/2.3.3/monix_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/case-app_2.12/2.0.6/case-app_2.12-2.0.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/scala-debug-adapter_2.12/1.1.3/scala-debug-adapter_2.12-1.1.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.8.0/jna-5.8.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna-platform/5.8.0/jna-platform-5.8.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/bsp4s_2.12/2.0.0-M13/bsp4s_2.12-2.0.0-M13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc_2.12/1.3.0-M4%2B46-edbe573e/zinc_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/jpountz/lz4/lz4/1.3.0/lz4-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/soc/directories/10/directories-10.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/test-interface/1.0/test-interface-1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/test-agent/1.4.4/test-agent-1.4.4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/googlecode/java-diff-utils/diffutils/1.3.0/diffutils-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/pprint_2.12/0.5.3/pprint_2.12-0.5.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/com-microsoft-java-debug-core/0.21.0%2B1-7f1080f1/com-microsoft-java-debug-core-0.21.0%2B1-7f1080f1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/nailgun-server/ee3c4343/nailgun-server-ee3c4343.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalaz/scalaz-concurrent_2.12/7.2.20/scalaz-concurrent_2.12-7.2.20.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier_2.12/2.0.16/coursier_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier-cache_2.12/2.0.16/coursier-cache_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/librarymanagement-ivy_2.12/1.0.0/librarymanagement-ivy_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/sourcecode_2.12/0.1.4/sourcecode_2.12-0.1.4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/directory-watcher/0.8.0%2B6-f651bd93/directory-watcher-0.8.0%2B6-f651bd93.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/zeroturnaround/zt-zip/1.13/zt-zip-1.13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/brave/brave/5.6.1/brave-5.6.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/reporter2/zipkin-sender-urlconnection/2.7.15/zipkin-sender-urlconnection-2.7.15.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm/9.2/asm-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm-util/9.2/asm-util-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/plokhotnyuk/jsoniter-scala/jsoniter-scala-core_2.12/2.4.0/jsoniter-scala-core_2.12-2.4.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-types_2.12/2.3.3/monix-types_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-execution_2.12/2.3.3/monix-execution_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-eval_2.12/2.3.3/monix-eval_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/monix/monix-reactive_2.12/2.3.3/monix-reactive_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/case-app-annotations_2.12/2.0.6/case-app-annotations_2.12-2.0.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/case-app-util_2.12/2.0.6/case-app-util_2.12-2.0.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-core_2.12/0.9.3/circe-core_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-derivation_2.12/0.9.0-M4/circe-derivation_2.12-0.9.0-M4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/lsp4s_2.12/0.2.0/lsp4s_2.12-0.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-core_2.12/1.3.0-M4%2B46-edbe573e/zinc-core_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-persist_2.12/1.3.0-M4%2B46-edbe573e/zinc-persist_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-compile-core_2.12/1.3.0-M4%2B46-edbe573e/zinc-compile-core_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-classfile_2.12/1.3.0-M4%2B46-edbe573e/zinc-classfile_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fansi_2.12/0.2.5/fansi_2.12-0.2.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/commons/commons-lang3/3.6/commons-lang3-3.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/code/gson/gson/2.7/gson-2.7.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/reactivex/rxjava2/rxjava/2.1.1/rxjava-2.1.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/reactivestreams/reactive-streams/1.0.0/reactive-streams-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/commons-io/commons-io/2.5/commons-io-2.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.26/slf4j-api-1.7.26.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalaz/scalaz-effect_2.12/7.2.20/scalaz-effect_2.12-7.2.20.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier-core_2.12/2.0.16/coursier-core_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/github/alexarchambault/argonaut-shapeless_6.2_2.12/1.2.0/argonaut-shapeless_6.2_2.12-1.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/get-coursier/coursier-util_2.12/2.0.16/coursier-util_2.12-2.0.16.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/alexarchambault/windows-ansi/windows-ansi/0.0.3/windows-ansi-0.0.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/librarymanagement-core_2.12/1.0.0/librarymanagement-core_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/sjson-new-core_2.12/0.8.2/sjson-new-core_2.12-0.8.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/ivy/ivy/2.3.0-sbt-a3314352b638afbf0dca19f127e8263ed6f898bd/ivy-2.3.0-sbt-a3314352b638afbf0dca19f127e8263ed6f898bd.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/zipkin2/zipkin/2.12.1/zipkin-2.12.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/zipkin/reporter2/zipkin-reporter/2.7.15/zipkin-reporter-2.7.15.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm-tree/9.2/asm-tree-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/ow2/asm/asm-analysis/9.2/asm-analysis-9.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jctools/jctools-core/2.0.1/jctools-core-2.0.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/chuusai/shapeless_2.12/2.3.3/shapeless_2.12-2.3.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-numbers_2.12/0.9.3/circe-numbers_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-core_2.12/1.1.0/cats-core_2.12-1.1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scalameta/jsonrpc_2.12/0.2.0/jsonrpc_2.12-0.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/beachape/enumeratum_2.12/1.5.13/enumeratum_2.12-1.5.13.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/beachape/enumeratum-circe_2.12/1.5.17/enumeratum-circe_2.12-1.5.17.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-apiinfo_2.12/1.3.0-M4%2B46-edbe573e/zinc-apiinfo_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/zinc-classpath_2.12/1.3.0-M4%2B46-edbe573e/zinc-classpath_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/compiler-interface/1.3.0-M4%2B46-edbe573e/compiler-interface-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/thesamet/scalapb/scalapb-runtime_2.12/0.8.0-RC1/scalapb-runtime_2.12-0.8.0-RC1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/io_2.12/1.2.0/io_2.12-1.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-logging_2.12/1.2.2/util-logging_2.12-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-relation_2.12/1.2.2/util-relation_2.12-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/sbinary_2.12/0.5.0/sbinary_2.12-0.5.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/launcher-interface/1.0.0/launcher-interface-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-parser-combinators_2.12/1.0.5/scala-parser-combinators_2.12-1.0.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-control_2.12/1.2.2/util-control_2.12-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/github/alexarchambault/concurrent-reference-hash-map/1.0.0/concurrent-reference-hash-map-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-xml_2.12/1.3.0/scala-xml_2.12-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/argonaut/argonaut_2.12/6.2.5/argonaut_2.12-6.2.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-collection-compat_2.12/2.2.0/scala-collection-compat_2.12-2.2.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/fusesource/jansi/jansi/1.18/jansi-1.18.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-compiler/2.12.11/scala-compiler-2.12.11.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/jcraft/jsch/0.1.46/jsch-0.1.46.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-reflect/2.12.11/scala-reflect-2.12.11.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/gigahorse-okhttp_2.12/0.3.0/gigahorse-okhttp_2.12-0.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/squareup/okhttp3/okhttp-urlconnection/3.7.0/okhttp-urlconnection-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-position_2.12/1.0.0/util-position_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-cache_2.12/1.0.0/util-cache_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/macro-compat_2.12/1.1.1/macro-compat_2.12-1.1.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-macros_2.12/1.1.0/cats-macros_2.12-1.1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/cats-kernel_2.12/1.1.0/cats-kernel_2.12-1.1.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/typelevel/machinist_2.12/0.6.2/machinist_2.12-0.6.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/scribe_2.12/2.5.0/scribe_2.12-2.5.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-parser_2.12/0.9.3/circe-parser_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/beachape/enumeratum-macros_2.12/1.5.9/enumeratum-macros_2.12-1.5.9.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/ch/epfl/scala/compiler-bridge_2.12/1.3.0-M4%2B46-edbe573e/compiler-bridge_2.12-1.3.0-M4%2B46-edbe573e.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.2.2/util-interface-1.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/thesamet/scalapb/lenses_2.12/0.8.0-RC1/lenses_2.12-0.8.0-RC1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fastparse_2.12/1.0.0/fastparse_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/swoval/apple-file-events/1.3.2/apple-file-events-1.3.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/jline/jline/2.14.4/jline-2.14.4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/logging/log4j/log4j-api/2.8.1/log4j-api-2.8.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.8.1/log4j-core-2.8.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lmax/disruptor/3.3.6/disruptor-3.3.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/sjson-new-scalajson_2.12/0.8.2/sjson-new-scalajson_2.12-0.8.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/gigahorse-core_2.12/0.3.0/gigahorse-core_2.12-0.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/squareup/okhttp3/okhttp/3.7.0/okhttp-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/sjson-new-murmurhash_2.12/0.8.0/sjson-new-murmurhash_2.12-0.8.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/scribe-macros_2.12/2.5.0/scribe-macros_2.12-2.5.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/perfolation_2.12/1.0.2/perfolation_2.12-1.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/io/circe/circe-jawn_2.12/0.9.3/circe-jawn_2.12-0.9.3.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/lihaoyi/fastparse-utils_2.12/1.0.0/fastparse-utils_2.12-1.0.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/eed3si9n/shaded-scalajson_2.12/1.0.0-M4/shaded-scalajson_2.12-1.0.0-M4.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/spire-math/jawn-parser_2.12/0.11.1/jawn-parser_2.12-0.11.1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/ssl-config-core_2.12/0.2.2/ssl-config-core_2.12-0.2.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/squareup/okio/okio/1.12.0/okio-1.12.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/outr/perfolation-macros_2.12/1.0.2/perfolation-macros_2.12-1.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/typesafe/config/1.2.0/config-1.2.0.jar

Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Attempting a connection to bloop server 127.0.0.1:8212 ...
Unable to load nailgun-version.properties.
NGServer [UNKNOWN] started on address /127.0.0.1 port 8212.
Attempting a connection to bloop server 127.0.0.1:8212 ...
Bloop server started
Opening BSP connection with bloop
Bloop BSP connection waiting at local:/Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
nailgun debug: Sending arguments '--protocol local --socket /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511' to Nailgun server
nailgun debug: Sending environment variables to Nailgun server
nailgun debug: Sending working directory /Users/alexandre/projects/scala-cli/test/.scala to Nailgun server
nailgun debug: Sending command to bsp Nailgun server
nailgun debug: Finished sending command information to Nailgun server
nailgun debug: Starting thread to read stdin...
[W] Internal error in session
java.io.EOFException
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
	at java.base/java.io.DataInputStream.readInt(DataInputStream.java:397)
	at com.martiansoftware.nailgun.NGCommunicator.readCommandContext(NGCommunicator.java:140)
	at com.martiansoftware.nailgun.NGSession.run(NGSession.java:197)
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 not found, waiting 100 milliseconds
BSP connection at /Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511 opened
Connected to Bloop via BSP at local:/Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511
Connected to Bloop via BSP at local:/Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511
nailgun debug: Received action Print([B@1c79f3a7) from Nailgun server
The server is listening for incoming connections at local:///Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511...
nailgun debug: Received action Print([B@274c0297) from Nailgun server
Accepted incoming BSP client connection at local:///Users/alexandre/Library/Caches/ScalaCli/bsp-sockets/proc-80511
Sending buildInitialize BSP command to Bloop
nailgun debug: Received action Print([B@7af46130) from Nailgun server
request received: build/initialize
nailgun debug: Received action Print([B@29f9d46d) from Nailgun server
BSP initialization handshake complete.
Fetching List(org.scala-lang::scala3-compiler:3.0.2), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 13 artifacts:
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.2/scala3-compiler_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.2/scala3-interfaces-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.2/tasty-core_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

Fetching List(org.scala-lang::scala3-library::3.0.2, org.virtuslab.scala-cli::runner:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Fetching List(org.virtuslab.scala-cli:stubs:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 1 artifacts:
  /Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar

Writing bloop project in /Users/alexandre/projects/scala-cli/test/.scala/.bloop/project_940fb43dce.json
Listing BSP build targets
Compiling project_940fb43dce with Bloop
Received onBuildTaskStart from bloop: TaskStartParams [
  taskId = TaskId [
    id = "1"
    parents = null
  ]
  eventTime = 1634309020072
  message = "Compiling project_940fb43dce (1 Scala source)"
  dataKind = "compile-task"
  data = {"target":{"uri":"file:/Users/alexandre/projects/scala-cli/test/.scala/?id=project_940fb43dce"}}
]
Compiling project (Scala 3.0.2, JVM)
Received onBuildTaskFinish from bloop: TaskFinishParams [
  taskId = TaskId [
    id = "1"
    parents = null
  ]
  eventTime = 1634309023968
  message = "Compiled 'project_940fb43dce'"
  status = OK
  dataKind = "compile-report"
  data = {"target":{"uri":"file:/Users/alexandre/projects/scala-cli/test/.scala/?id=project_940fb43dce"},"originId":null,"errors":0,"warnings":0,"time":null,"isNoOp":false,"isLastCycle":true,"clientDir":"file:///Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/","analysisOut":"file:///Users/alexandre/projects/scala-cli/test/.scala/.bloop/project_940fb43dce/project_940fb43dce-analysis.bin"}
]
Compiled project (Scala 3.0.2, JVM)
Compilation succeeded
Post-processing class files of pre-processed sources
Overwriting .scala/project_940fb43dce/classes/main/test$.class
Overwriting .scala/project_940fb43dce/classes/main/test.class
Overwriting .scala/project_940fb43dce/classes/main/test_sc$.class
Overwriting .scala/project_940fb43dce/classes/main/test_sc.class
Moving semantic DBs around
Reading TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test.tasty
Parsed TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test.tasty
Overwriting .scala/project_940fb43dce/classes/main/test.tasty
Reading TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test_sc.tasty
Parsed TASTy file /Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main/test_sc.tasty
Overwriting .scala/project_940fb43dce/classes/main/test_sc.tasty
Fetching List(org.scala-lang::scala3-compiler:3.0.2), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 13 artifacts:
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.2/scala3-compiler_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.2/scala3-interfaces-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.2/tasty-core_3-3.0.2.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar
  /Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar

Fetching List(org.scala-lang::scala3-library::3.0.2, org.virtuslab.scala-cli::runner:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Fetching List(org.virtuslab.scala-cli:stubs:0.0.5+43-g60eae701-SNAPSHOT), adding List(https://oss.sonatype.org/content/repositories/snapshots, ivy:file:///Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7//[defaultPattern])
Found 1 artifacts:
  /Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar

Writing bloop project in /Users/alexandre/projects/scala-cli/test/.scala/.bloop/project_f643cb0bc2-test.json
Listing BSP build targets
Compiling project_f643cb0bc2-test with Bloop
Compilation succeeded
Post-processing class files of pre-processed sources
Moving semantic DBs around
  Running
/Users/alexandre/Library/Caches/Coursier/jvm/adopt@1.11.0-7/Contents/Home/bin/java
-cp
/Users/alexandre/projects/scala-cli/test/.scala/project_940fb43dce/classes/main:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.0.2/scala3-library_3-3.0.2.jar:/Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/runner_3/0.0.5+43-g60eae701-SNAPSHOT/jars/runner_3.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.6/scala-library-2.13.6.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/oss.sonatype.org/content/repositories/snapshots/org/virtuslab/pretty-stacktraces_3/0.0.0%2B27-b9d69198-SNAPSHOT/pretty-stacktraces_3-0.0.0%2B27-b9d69198-SNAPSHOT.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-tasty-inspector_3/3.0.0/scala3-tasty-inspector_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-compiler_3/3.0.0/scala3-compiler_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-interfaces/3.0.0/scala3-interfaces-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/tasty-core_3/3.0.0/tasty-core_3-3.0.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/modules/scala-asm/9.1.0-scala-1/scala-asm-9.1.0-scala-1.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/compiler-interface/1.3.5/compiler-interface-1.3.5.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-reader/3.19.0/jline-reader-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal/3.19.0/jline-terminal-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/jline/jline-terminal-jna/3.19.0/jline-terminal-jna-3.19.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/com/google/protobuf/protobuf-java/3.7.0/protobuf-java-3.7.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/org/scala-sbt/util-interface/1.3.0/util-interface-1.3.0.jar:/Users/alexandre/Library/Caches/Coursier/v1/https/repo1.maven.org/maven2/net/java/dev/jna/jna/5.3.1/jna-5.3.1.jar:/Users/alexandre/Library/Caches/ScalaCli/local-repo/v0.0.5-43-60eae7/org.virtuslab.scala-cli/stubs/0.0.5+43-g60eae701-SNAPSHOT/jars/stubs.jar
scala.cli.runner.Runner
test_sc

execve available
Hello
Client in /Users/alexandre/projects/scala-cli/test/.scala/.bloop disconnected with a 'SocketError' event. Cancelling tasks...
```
