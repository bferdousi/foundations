package exercises.action.fp.search

import exercises.action.DateGenerator._
import exercises.action.fp.IO
import exercises.action.fp.search.Airport._
import exercises.action.fp.search.SearchFlightGenerator._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant, LocalDate}
import scala.concurrent.ExecutionContext

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchFlightServiceTest
class SearchFlightServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("fromTwoClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.fromTwoClients(client1, client2)(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromTwoClients example unique") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("2", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")
    val flight5 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4, flight5)))

    val service = SearchFlightService.fromTwoClients(client1, client2)(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight4)))
  }

  test("fromTwoClients handle error gracefully") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight1)))
    val client2 = SearchFlightClient.constant(IO.fail(new Exception("Boom")))

    val service = SearchFlightService.fromTwoClients(client1, client2)(ExecutionContext.global)
    val result  = service.search(parisOrly, londonGatwick, today).attempt.unsafeRun()

    assert(result.isSuccess)
    assert(result.get == SearchResult(List(flight1)))
  }

  test("fromTwoClients property based test") {
    forAll(
      SearchFlightGenerator.clientGen,
      SearchFlightGenerator.clientGen,
      SearchFlightGenerator.airportGen,
      SearchFlightGenerator.airportGen,
      dateGen
    ) { (client1, client2, fromAirport, toAirport, date) =>
      val service = SearchFlightService.fromTwoClients(client1, client2)(ExecutionContext.global)
      val result  = service.search(fromAirport, toAirport, date).attempt.unsafeRun()

      assert(result.isSuccess)
    }
  }
  test("from multiple clients") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")
    val flight5 = Flight("5", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight5)))
    val client3 = SearchFlightClient.constant(IO(List(flight4)))

    val service = SearchFlightService.fromClients(List(client1, client2, client3))
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight4, flight5, flight1, flight2, flight3)))
  }
  test("from multiple clients property based") {
    forAll(Gen.listOf(clientGen), dateGen, airportGen, airportGen) {
      (clients: List[SearchFlightClient], date, from, to) => {
        val res = SearchFlightService.fromClients(clients).search(from, to, date).attempt.unsafeRun()
        assert(res.isSuccess)
      }
    }
  }

}
