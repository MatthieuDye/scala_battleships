import battleships.Cell
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class BattleshipsTest extends FunSuite with DiagrammedAssertions {
  test("Hello should start with H") {
    assert("Hello".startsWith("H"))
  }

  test("Testing if a cell already exists") {
    assert(IndexedSeq((8, 1), (8, 2), (8, 3), (8, 4)).exists(cellWith => Set((8, 1), (6, 1), (4, 1), (7, 1), (5, 1)).contains(cellWith)))
  }

  test("Testiong if a full set of cells is already in another one") {
    assert( Set( (8, 1), (8, 3) ) .forall( cell => IndexedSeq( (1,2), (8,1), (2,8), (8,3) ).contains(cell)) )
  }

  test("Test contains") {
    assert(Iterator((8, 1), (8, 3)).contains((8, 1)))
  }

}