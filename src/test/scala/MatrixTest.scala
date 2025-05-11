package ca.mosnyan

import org.scalatest.flatspec.AnyFlatSpec

class MatrixTest extends AnyFlatSpec {

  trait TwoByTwoMatrix {
    val mat: Matrix[Int] = Matrix(List(List(1, 2), List(3, 4)))
  }

  "The matrix" must "have two rows" in new TwoByTwoMatrix {
    assert(mat.nRows == 2)
  }

  "The matrix" must "have two columns" in new TwoByTwoMatrix {
    assert(mat.nCols == 2)
  }

  "A matrix with uneven rows" must "not be created" in {
    assertThrows[MatrixFormatException] {
      Matrix(List(List(1, 2, 3), List(4, 5)))
    }
  }

  "A matrix with no data" must "not be created" in {
    assertThrows[MatrixFormatException] {
      Matrix[Int](List(List(), List()))
    }
  }

  "The element 1, 1" must "be 4" in new TwoByTwoMatrix {
    assert(mat(1, 1).get == 4)
  }

  "There" must "not be any element -1, 0" in new TwoByTwoMatrix {
    assert(mat(-1, 0).isEmpty)
  }

  "Nor" must "there be an element 0, 3" in new TwoByTwoMatrix {
    assert(mat(0, 3).isEmpty)
  }

  "The row 1, 2" must "be returned" in new TwoByTwoMatrix {
    assert(mat.row(0).get == List(1, 2))
  }

  "The column 1, 3" must "be returned" in new TwoByTwoMatrix {
    assert(mat.col(0).get == List(1, 3))
  }

  "The row 4" must "return None" in new TwoByTwoMatrix {
    assert(mat.row(4).isEmpty)
  }

  "The column -1" must "return None" in new TwoByTwoMatrix {
    assert(mat.col(-1).isEmpty)
  }

  "Iterating through rows" must "result in [1, 2][3, 4]" in new TwoByTwoMatrix {
    val test = for {
      row <- mat.rows
    } yield row
    assert(test.toList == List(List(1, 2), List(3, 4)))
  }

  "Iterating through columns" must "result in (1, 3)(2, 4)" in new TwoByTwoMatrix {
    val test = for {
      col <- mat.cols
    } yield col
    assert(test.toList == List(List(1, 3), List(2, 4)))
  }

  "The 2x2 matrix" must "be returned as string" in new TwoByTwoMatrix {
    assert(mat.toString == "1 2\n3 4")
  }

  "The identity method" must "succeed" in {
    assert(Matrix.getIdentity(2).get.toString == "1 0\n0 1")
  }

  "The dot product of 1, 2 and 3, 4" must "be 11" in {
    assert(Matrix.dotProduct(List(1, 2), List(3, 4)).get == 11)
  }

  "The dot product" must "not work with two vectors of different sizes" in {
    assert(Matrix.dotProduct(List(1, 2, 3), List(4, 5)).isEmpty)
  }

  "Adding the matrix twice" must "be (2, 4, 6, 8)" in new TwoByTwoMatrix {
    assert((mat + mat).get.toString == "2 4\n6 8")
  }

  "Matrix addition" must "not work if dimensions aren't compatible" in {
    val mat1 = Matrix.getIdentity(2).get
    val mat2 = Matrix.getIdentity(3).get
    assert((mat1 + mat2).isEmpty)
  }

  "Subtracting the matrix twice" must "be (0, 0, 0, 0)" in new TwoByTwoMatrix {
    assert((mat - mat).get.toString == "0 0\n0 0")
  }

  "Matrix subtraction" must "not work if dimensions aren't compatible" in {
    val mat1 = Matrix.getIdentity(2).get
    val mat2 = Matrix.getIdentity(3).get
    assert((mat1 - mat2).isEmpty)
  }

  "These matrices" must "be equal" in new TwoByTwoMatrix {
    val newMat: Matrix[Int] = Matrix(List(List(1, 2), List(3, 4)))
    assert(mat == newMat)
  }

  "The matrix multiplication" must "work" in new TwoByTwoMatrix {
    assert((mat * mat).get == Matrix(List(List(7, 10), List(15, 22))))
  }

  "The matrix multiplication" must "not work if dimensions aren't compatible" in {
    val mat1 = Matrix.getIdentity(2).get
    val mat2 = Matrix.getIdentity(3).get
    assert((mat1 * mat2).isEmpty)
  }

}
