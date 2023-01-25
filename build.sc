import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalajslib._
import $repo.`https://jitpack.io`
import $ivy.`de.sebbraun::mill-utils:main-SNAPSHOT`
import de.sebbraun.millutils._

object v {
  val scala = "3.2.1"
  val scalaJS = "1.12.0"
  val circe = "0.14.3"
  val utest = "0.8.1"
  object endpoints4s {
    val algebra = "1.9.0"
  }
}

object d {
  val circe = DepGroup("io.circe", v.circe, "circe-")
  val utest = DepGroup("com.lihaoyi", v.utest, "utest-")
  object endpoints4s {
    val algebra = DepGroup("org.endpoints4s", v.endpoints4s.algebra, "algebra-")
  }
}

object meta extends Module {
  def optString(s: String): Option[String] =
    if (s == null) None
    else {
      val st = s.trim()
      if (st.isEmpty()) None
      else Some(st)
    }

  def versionFromEnv = T.input { T.env.get("PUBLISH_VERSION") }
  def versionFromGitSha = T.input {
    optString(os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim)
}
  def versionFromGitTag = T.input {
    optString(
      os.proc("git", "tag", "-l", "-n0", "--points-at", "HEAD")
        .call()
        .out
        .trim
        .stripPrefix("v")
    )
  }
  def publishVersion = T {
    (versionFromEnv() orElse versionFromGitTag() orElse versionFromGitSha())
      .getOrElse("latest")
  }

  def pomSettings = PomSettings(
      description = "Auto-generation of Circe codecs",
    organization = groupID,
      url = "https://github.com/braunse/hecate",
      licenses = Seq(License.`MPL-2.0`),
      versionControl = VersionControl.github("braunse", "hecate"),
      developers = Seq(
        Developer(
          id = "braunse",
          name = "Sebastien Braun",
          url = "https://github.com/braunse"
        )
      )
  )

  val groupID = "de.sebbraun.hecate"
  val prefix = "hecate"
}

trait CommonSharedModule extends SharedPublishModule {
  def scalaVersion = v.scala
  def scalaJSVersion = v.scalaJS

  def testFramework = "utest.runner.Framework"

  def artifactNameSuffix: String
  def artifactName = s"${meta.prefix}-${artifactNameSuffix}"
  def publishVersion = meta.publishVersion()
  def pomSettings = meta.pomSettings

}

object core extends CommonSharedModule {
  def ivyDeps = Agg(
    d.circe.multi("core")
  )
  def testIvyDeps = Agg[MultiDep](
    d.utest.self
  )

  def artifactNameSuffix = "core"

  object backend extends BackendModule with BackendPublishModule {
    def moduleDeps = Seq()
    object tests extends BackendTests
  }

  object frontend extends FrontendModule with FrontendPublishModule {
    def moduleDeps = Seq()
    object tests extends FrontendTests
  }
}

object js extends ScalaJSModule with CommonModule {
  def millSourcePath = build.millSourcePath

  def scalaJSVersion = v.scalaJS

  def artifactName = "hecate-core"

  def ivyDeps = Agg(
    ivy"io.circe::circe-core::${v.circe}"
  )

  def sources = T.sources(
    millSourcePath / "src" / "shared",
    millSourcePath / "src" / "js"
  )

  object tests extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::${v.utest}"
    )

    def testFramework = "utest.runner.Framework"

    def sources = T.sources(
      millSourcePath / "src" / "shared",
      millSourcePath / "src" / "js"
    )
  }
}
