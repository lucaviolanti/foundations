package answers.dataprocessing

import answers.dataprocessing.FileAnswers._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.io.File

class FileAnswersTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {
  val scalaSeedProject = new File("PATH_TO_PROJECT/scala-seed-project")

  ignore("diskUsage") {
    assert(diskUsageImperative(scalaSeedProject) == 1986)
    assert(diskUsage(scalaSeedProject) == 1986)
  }

  ignore("largestFileSize") {
    assert(largestFileSize(scalaSeedProject) == 447)
  }

  ignore("filterFiles") {
    assert(
      filterFiles(scalaSeedProject, _.getName.endsWith(".scala")).map(_.getName).sorted ==
        List("Dependencies.scala", "Hello.scala", "HelloSpec.scala")
    )
  }

}
