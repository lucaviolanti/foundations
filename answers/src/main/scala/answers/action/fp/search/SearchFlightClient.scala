package answers.action.fp.search

import answers.action.fp.IO

import java.time.LocalDate

trait SearchFlightClient {
  def search(from: Airport, to: Airport, date: LocalDate): IO[List[Flight]]
}

object SearchFlightClient {

  // test client which returns the same list of flights for all requests
  def constant(flights: IO[List[Flight]]): SearchFlightClient =
    new SearchFlightClient {
      def search(from: Airport, to: Airport, date: LocalDate): IO[List[Flight]] =
        flights
    }

}
