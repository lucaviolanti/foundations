package answers.action.fp.console

import answers.action.fp.IO

import java.time.Instant

trait Clock {
  def now: IO[Instant]
}

object Clock {
  val system: Clock = new Clock {
    val now: IO[Instant] =
      IO(Instant.now())
  }

  def constant(instant: Instant): Clock = new Clock {
    val now: IO[Instant] = IO(instant)
  }
}
