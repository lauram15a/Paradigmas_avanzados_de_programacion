import scala.annotation.tailrec

object Funciones_lista
{
  /*
  * IMPRIMIR_LISTA
  * Impresión de una lista en forma de matriz a partir de esta y sus dimensiones
  * (No se puede realizar con ParVector ya que se imprimir en desorden)
  */
  @tailrec
  def imprimir_lista(width: Int, length: Int, matrix: List[Int]): Unit =
  {

    /*
    * IMPRIMIR_LISTA_AUX
    * Función auxiliar que imprime para una de las filas
    */
    @tailrec
    def imprimir_lista_aux(width: Int, matrix: List[Int]): List[Int] =
    {
      if (width == 0)
      {
        matrix
      }
      else
      {
        print(matrix.head + " ")
        imprimir_lista_aux(width - 1, matrix.tail)
      }
    }
    if (length != 0)
    {
      val matrix_ = imprimir_lista_aux(width, matrix)
      println()
      imprimir_lista(width, length-1, matrix_)
    }
  }

  /*
  * GET
  * Obtiene el valor de una posición concreta de una lista en forma de matriz a partir de la posición, la matriz y sus dimensiones
  * (No se cambia a ParVector y se accede ya que la conversión tarda más tiempo de ejecución que acceder directamente)
  */
  def get(x: Int, y: Int, width: Int, length: Int, matrix: List[Int]): Int =
  {
    assert(x<length && y<width)
    //matrix.par(x*width+y).toList
    matrix(x*width+y)
  }

  /*
  * SET
  * Fija un valor en una posición concreta de una lista en forma de matriz a partir del valor, la posición, la matriz y sus dimensiones
  * (No se cambia a ParVector y se modifica ya que la conversión tarda más tiempo de ejecución que modificar directamente)
  */
  def set(valor:Int, x: Int, y: Int, width: Int, length: Int, matrix: List[Int]): List[Int] =
  {
    assert(x<length && y<width)
    //matrix.par.updated(x*width+y, valor).toList
    matrix.updated(x*width+y, valor)
  }

  /*
  * ROW
  * Obtiene la fila especificada a partir de la requerida, la matriz y sus dimensiones
  * (No se puede realizar con ParVector ya que se pueden guardar en desorden)
  */
  def row(x: Int, width: Int, length: Int, matrix: List[Int]): List[Int] =
  {

    /*
    * ROW_AUX
    * Función auxiliar que que itera por cada uno de los elementos de la lista guardando los necesarios
    */
    def row_aux(x: Int, a: Int, b: Int, matrix: List[Int]): List[Int] =
    {
      if (x >= a && x <= b)
      {
        matrix.head :: row_aux(x+1, a, b, matrix.tail)
      }
      else
      {
        if (x < a)
        {
          row_aux(x+1, a, b, matrix.tail)
        }
        else
        {
          Nil
        }
      }
    }

    assert(x<length)
    row_aux(0, x*width, (x+1)*width-1, matrix)
  }

  /*
  * COLUMN
  * Obtiene la columna especificada a partir de la requerida, la matriz y sus dimensiones
  * (No se puede realizar con ParVector ya que se pueden guardar en desorden)
  */
  def column(y: Int, width: Int, length: Int, matrix: List[Int]): List[Int] =
  {

    /*
    * COLUMN_AUX
    * Función auxiliar que que itera por cada uno de los elementos de la lista guardando los necesarios
    */
    def column_aux(x: Int, y: Int, width: Int, length: Int, matrix: List[Int]): List[Int] =
    {
      if (x%width == y)
      {
        matrix.head :: column_aux(x+1, y, width, length, matrix.tail)
      }
      else
      {
        if ((width*(length-1)+y)+1 == x)
        {
          Nil
        }
        else
        {
          column_aux(x+1, y, width, length, matrix.tail)
        }
      }
    }

    assert(y<width)
    column_aux(0, y, width, length, matrix)
  }
}
