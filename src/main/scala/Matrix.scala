package ca.mosnyan

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.infixNumericOps
import java.util.NoSuchElementException

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
   * Operator shadowing to test for matrix equality.
   * @param other The other matrix.
   * @return True if the data value is equal, false if not.
   */
  def ==(other: Matrix[T]): Boolean = {
    this.data == other.data
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

case class MatrixFormatException(message: String = "") extends Exception(message)