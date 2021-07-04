// 11. Escribir la función imprimir l donde l es una lista de enteros del 0 al 9, con longitud 16 (4*4).

import scala.math.sqrt

object Ej11_Imprimir_Lista {
  def main (args: Array[String])
  { 
     val num_elems : Int = 16;
     val num_elems_por_fila : Int = sqrt(num_elems).toInt;
     val cont : Int = 0;  // contador
     val lista = crear_lista(num_elems, List[Int]());
     println(lista)
     imprimir(num_elems_por_fila, cont, lista);
  }
  
  //imprimimos filas de 4 nums cada una
  def imprimir(n: Int, cont: Int, lista: List[Int]) : List[Int] =
  {
    // ya no hay más elementos
    if (lista.isEmpty)
    {
      lista;
    }
    else 
    {
      //salto de fila
      if (cont == (n - 1))
      {
        println(lista.head)
        imprimir(n, 0, lista.tail)
      }
      // misma fila
      else
      {
        print(lista.head)
        print(",")
        imprimir(n, cont + 1, lista.tail)
      }
    }
  }
  
  //Creamos la lista con nums aleatorios
  def crear_lista(num_elems : Int, lista : List[Int]): List[Int] =
  {
    if (num_elems == 0)
    {
      lista.reverse
    }
    else
    {
      crear_lista(num_elems - 1, generar_num_random() :: lista);
    }
  }
  
  //genera los nums aleatorios
  def generar_num_random() : Int =
  {
    val inicio = 0
    val fin   = 9
    val rnd = new scala.util.Random
    inicio + rnd.nextInt( (fin - inicio) + 1 )  
  }
}