import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalajslib._

object v {
  val scala = "3.2.1"
  val scalaJS = "1.12.0"
  val circe = "0.14.3"
  val utest = "0.8.1"
}

object meta extends Module {
  def optString(s: String): Option[String] =
    if(s == null) None
    else {
      val st = s.trim()
      if(st.isEmpty()) None
      else Some(st)
    }

  def versionFromEnv = T.input { T.env.get("PUBLISH_VERSION") }
  def versionFromGitSha = T.input { optString(os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim) }
  def versionFromGitTag = T.input { optString(os.proc("git", "tag", "-l", "-n0", "--points-at", "HEAD").call().out.trim.stripPrefix("v")) }
  def publishVersion = T { (versionFromEnv() orElse versionFromGitTag() orElse versionFromGitSha()).getOrElse("latest") }

  val groupID = "de.sebbraun.hecate"
  val prefix = "hecate-"
}

trait CommonModule extends ScalaModule with PublishModule {
  def scalaVersion = v.scala

  def publishVersion = meta.publishVersion()

  def pomSettings = PomSettings(
      description = "Auto-generation of Circe codecs",
      organization = "de.sebbraun.hecate",
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
}

object jvm extends ScalaModule with CommonModule {
  def millSourcePath = build.millSourcePath

  def artifactName = "hecate-core"

  def ivyDeps = Agg(
    ivy"io.circe::circe-core:${v.circe}"
  )

  def sources = T.sources(
    millSourcePath / "src" / "shared",
    millSourcePath / "src" / "jvm"
  )

  object tests extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest:${v.utest}"
    )

    def testFramework = "utest.runner.Framework"

    def sources = T.sources(
      millSourcePath / "src" / "shared",
      millSourcePath / "src" / "jvm"
    )
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
