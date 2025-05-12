package ca.mosnyan

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps
import scala.math.Fractional.Implicits.infixFractionalOps
import java.util.NoSuchElementException
import scala.math
import scala.math.{Pi, pow}

/**
 * Yet Another Linear Algebra Practice Package: Scala Edition
 */

class Matrix[T : Numeric] private (private val data: List[List[T]]) {
  if (data.isEmpty || data.forall(row => row.isEmpty)) {
    throw MatrixFormatException("Empty matrix !")
  }
  else if (!data.forall(row => row.size == data.head.size)) {
    throw MatrixFormatException("A matrix must have rows of equal size !")
  }
  val nRows: Int = data.size
  val nCols: Int = data.head.size

  /**
   * Apply method to grab an element from the matrix.
   * @param row The row index of the element to get.
   * @param col The column index of the element to get.
   * @return Some(T) if successful, None if one of the indices is out of bounds.
   */
  def apply(row: Int, col: Int): Option[T] = {
    if (row < 0 || row >= nRows) None
    else if (col < 0 || col >= nCols) None
    else Some(data(row)(col))
  }

  /**
   * Gets the specified row of the matrix.
   * @param row The row index of the row to get.
   * @return Some(List[T]) if successful, None if the index is out of bounds.
   */
  def row(row: Int): Option[List[T]] = {
    if (row < 0 || row >= nRows) None
    else Some(data(row))
  }

  /**
   * Gets the specified column of the matrix.
   * @param col The column index of the column to get.
   * @return Some(List[T]) if successful, None if the index is out of bounds.
   */
  def col(col: Int): Option[List[T]]  = {
    if (col < 0 || col >= nCols) None
    else Some(data.map(row => row(col)))
  }

  /**
   * Supplies an iterator to go through the rows.
   * @return An iterator that iterates over the rows.
   */
  def rows: Iterator[List[T]] = {
    data.iterator
  }

  /**
   * Supplies an iterator to go through the columns.
   * @return An iterator that iterates over the columns.
   */
  def cols: Iterator[List[T]] = {
    new Iterator[List[T]] {
      private var idx: Int = 0

      override def hasNext: Boolean = {
        idx < nCols
      }

      override def next(): List[T] = {
        val res = Matrix.this.col(idx) match {
          case Some(c) => c
          case None => throw NoSuchElementException("Column iterator out of bounds !")
        }
        idx = idx + 1
        res
      }
    }
  }

  /**
   * Transposes the matrix.
   * @return The transposed matrix. Cannot fail.
   */
  def transpose(): Matrix[T] = {
    Matrix(nCols, nRows, (row, col) => this(col, row).get)
  }

  /**
   * Indicates whether the matrix is square or not.
   * @return True if square, false otherwise.
   */
  def isSquare: Boolean = {
    nRows == nCols
  }

  /**
   * Calculates the trace of the matrix. The trace is the sum of the elements in its diagonal.
   * @return Some(T) for the trace, None if the matrix is not square.
   */
  def trace: Option[T] = {
    @tailrec
    def sumDiagonal(idx: Int, acc: T): T = {
      if (idx == nRows) acc
      else sumDiagonal(idx + 1, acc + this(idx, idx).get)
    }
    if (!isSquare) None
    else Some(sumDiagonal(1, this(0, 0).get))
  }

  /**
   * Calculates the determinant of the matrix.
   * @return Some(T) for the determinant, None if it can't be computed (matrix isn't square).
   */
  def determinant: Option[T] = {
   if (!isSquare) None
   else if (nRows == 1) this(0, 0)
   else if (nRows == 2) Some(this(0, 0).get * this(1, 1).get - this(0, 1).get * this(1, 0).get)
   else Some(cols.zipWithIndex.map((col, idx) => getSign(0, idx) * col.head * this.submatrix(0, idx).determinant.get).sum)
  }

  /**
   * Gets the minor of the specified element. The minor is the determinant of the submatrix composed of the base
   * matrix, minus the row and column where the element sits.
   * @param row The row of the element.
   * @param col The column of the element.
   * @return Some(T) if the minor can be calculated, None otherwise (matrix isn't square).
   */
  def minor(row: Int, col: Int): Option[T] = {
    if (!isSquare) None
    else if (row < 0 || row >= nRows) None
    else if (col < 0 || col >= nCols) None
    else Some(submatrix(row, col).determinant.get)
  }

  /**
   * Gets the cofactor of the specified element. The cofactor is the signed minor.
   * @param row The row of the element.
   * @param col The column of the element.
   * @return Some(T) if the cofactor can be calculated, None otherwise (matrix isn't square).
   */
  def cofactor(row: Int, col: Int): Option[T] = {
    if (!isSquare) None
    else if (row < 0 || row >= nRows) None
    else if (col < 0 || col >= nCols) None
    else Some(getSign(row, col) * submatrix(row, col).determinant.get)
  }

  /**
   * Returns the cofactor matrix of the matrix. The cofactor matrix is a matrix where each element is replaced
   * by its cofactor.
   * @return Some(Matrix[T]) if the cofactor matrix exists, None otherwise (matrix isn't square).
   */
  def cofactorMatrix(): Option[Matrix[T]] = {
    if (!isSquare) None
    else Some(Matrix(nRows, nCols, (row, col) => cofactor(row, col).get))
  }

  /**
   * Returns the adjugate matrix. The adjugate matrix is the transposed cofactor matrix.
   * @return Some(Matrix[T]) if the adjugate matrix exists, None otherwise.
   */
  def adjugateMatrix(): Option[Matrix[T]] = {
    cofactorMatrix() match {
      case Some(m) => Some(m.transpose())
      case None => None
    }
  }

  /**
   * Checks if a matrix is invertible.
   * @return True if it is, false otherwise.
   */
  def isInvertible: Boolean = {
    determinant match {
      case Some(d) => if (d != 0) true else false
      case None => false
    }
  }

  /**
   * Inverts the matrix. This returns a matrix of Doubles no matter the type of the base matrix, because the
   * inverse may be fractional.
   * @return The inverted matrix.
   */
  def invert(): Option[Matrix[Double]] = {
    if (!isInvertible) None
    else Some(Matrix(nRows, nCols, (row, col) =>
      (1/determinant.get.toDouble) * adjugateMatrix().get(row, col).get.toDouble))
  }

  /**
   * Operator overload to add two matrices.
   * @param other The other matrix.
   * @return Some(Matrix[T]) if successful, None if the matrices aren't of the same dimensions.
   */
  def +(other: Matrix[T]): Option[Matrix[T]] = {
    if (nRows != other.nRows) None
    else if (nCols != other.nCols) None
    else Some(Matrix(nRows, nCols, (row, col) => this(row, col).get + other(row, col).get))
  }

  /**
   * Operator overload to subtract two matrices.
   * @param other The other matrix.
   * @return Some(Matrix[T]) if successful, None if the matrices aren't of the same dimensions.
   */
  def -(other: Matrix[T]): Option[Matrix[T]] = {
    if (nRows != other.nRows) None
    else if (nCols != other.nCols) None
    else Some(Matrix(nRows, nCols, (row, col) => this(row, col).get - other(row, col).get))
  }

  /**
   * Operator overload to multiply two matrices.
   * @param other The other matrix.
   * @return Some(Matrix[T]) if successful, None if column dimension of LHS is not equal to row dimension of RHS.
   */
  def *(other: Matrix[T]): Option[Matrix[T]] = {
    if (nCols != other.nRows) None
    else Some(Matrix(nRows, other.nCols, (row, col) => {
      Matrix.dotProduct(this.row(row).get, other.col(col).get).get
    }))
  }

  /**
   * Operator overload to multiply a matrix by a scalar.
   * @param k The scalar.
   * @return A matrix with its elements multiplied by the scalar. Cannot fail.
   */
  def *(k: T): Matrix[T] = {
    Matrix(nRows, nCols, (row, col) => this(row, col).get * k)
  }

  /**
   * Operator shadowing to test for matrix equality.
   * @param other The other matrix.
   * @return True if the data value is equal, false if not.
   */
  def ==(other: Matrix[T]): Boolean = {
    this.data == other.data
  }

  /**
   * Strips a matrix of a specified row and a specified column.
   * @param row The row to strip.
   * @param col The column to strip.
   * @return A matrix amputated of the defined row and column.
   */
  private def submatrix(row: Int, col: Int): Matrix[T] = {
    val newData = this.data.zipWithIndex
      .filter((_, idx) => idx != row) // removes row
      .map((row, _) => row.zipWithIndex.filter((_, idx) => idx != col) // removes column
        .map((ele, idx) => ele)) // removes indices
    Matrix(newData)
  }

  /**
   * Gets the sign associated with the element to calculate the cofactor.
   * @param row The row of the element.
   * @param col The column of the element.
   * @return The associated sign (+1 if row + col is even, -1 otherwise).
   */
  private def getSign(row: Int, col: Int): T = {
    if ((row + col) % 2 == 0) implicitly[Numeric[T]].one
    else -implicitly[Numeric[T]].one
  }

  /**
   * Overloaded toString method to display a matrix.
   * @return A String formatted to resemble the matrix.
   */
  override def toString: String = {
    @tailrec
    def matrixBuilder(row: Int, str: String): String = {
      if (row == nRows) str.dropRight(1)
      else matrixBuilder(row + 1, str + data(row).mkString(" ") + '\n')
    }
    matrixBuilder(0, "")
  }
}

object Matrix {

  /**
   * Apply method to create a matrix from raw data.
   * @param data The data that the matrix will contain.
   * @return A matrix with the data.
   * @throws MatrixFormatException If the data is malformed (has different row lengths).
   */
  def apply[T : Numeric](data: List[List[T]]): Matrix[T] = {
    new Matrix(data)
  }

  /**
   * Apply method to create a matrix from parameters.
   * @param nRows The number of rows of the matrix.
   * @param nCols The number of columns of the matrix.
   * @param initializer An initializing function to set the value of each element, of the form (row, col) => value
   * @return A matrix with the data.
   * @throws MatrixFormatException If the data is malformed (has different row lengths).
   */
  def apply[T : Numeric](nRows: Int, nCols: Int, initializer: (Int, Int) => T): Matrix[T] = {
    new Matrix(List.tabulate(nRows, nCols)(initializer))
  }

  /**
   * Supplies an identity matrix (square matrix with 1s on the diagonal, 0 otherwise).
   * @param dim The matrix dimension to get.
   * @return Some(Matrix[T]) if dim > 0, None otherwise.
   */
  def getIdentity(dim: Int): Option[Matrix[Int]] = {
    if (dim == 0) None
    else Some(Matrix(dim, dim, (row, col) => if (row == col) 1 else 0))
  }

  /**
   * Computes the dot product of two vectors.
   * @param v1 The first vector.
   * @param v2 The second vector.
   * @return Some(T) if v1 and v2 are the same length, None otherwise.
   */
  def dotProduct[T : Numeric](v1: List[T], v2: List[T]): Option[T] = {
    if (v1.size != v2.size) None
    else Some(v1.zip(v2).map(pair => pair._1 * pair._2).sum)
  }
}

/**
 * Value class enabling scalar multiplication to be commutative.
 * @param k The scalar.
 * @tparam Numeric Any type that supports multiplication.
 */
implicit class CommutativeScalarMultiplication[Numeric](val k: Numeric) extends AnyVal {
  def *(mat: Matrix[Numeric]): Matrix[Numeric] = {
    mat * k
  }
}

/**
 * Exception class denoting a malformed matrix during object construction.
 * @param message The exception message.
 */
case class MatrixFormatException(message: String = "") extends Exception(message)