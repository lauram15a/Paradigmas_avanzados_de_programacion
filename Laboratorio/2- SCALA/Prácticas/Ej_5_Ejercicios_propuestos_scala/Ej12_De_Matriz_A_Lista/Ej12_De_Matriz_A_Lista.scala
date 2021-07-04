// 12. Supongamos que una matriz de enteros de 8 filas (numeradas de arriba abajo)
//     x 8 columnas (numeradas de izquierda a derecha) la representamos como una lista m de 64 elementos.
//     Realizar las funciones de imprimir matriz, leer elemento, leer fila, leer columna y realizar traspuesta

import scala.math.sqrt

object Ej12_De_Matriz_A_Lista {
  def main (args: Array[String])
  { 
     val num_elems : Int = 64;
     val num_elems_por_fila : Int = sqrt(num_elems).toInt; // o por columna
     val cont : Int = 0;  // contador
     val cont_fila : Int = 0; // el num de la fil en la que estamos
     val pos_elem : Int = 5; //la posicion del elem que queremos
     val pos_fila : Int = 5; //la posicion de la fila que queremos
     val pos_columna : Int = 5; //la posicion de la columna que queremos
     
     val lista = crear_lista(num_elems, List[Int]());
     println(lista)
     imprimir(num_elems_por_fila, cont, lista);
     println(leer_elemento(pos_elem, cont, lista));
     println(leer_fila(pos_fila * num_elems_por_fila, cont, num_elems_por_fila, lista, List[Int]()));
     println(leer_columna(pos_columna, cont, cont_fila, num_elems_por_fila, lista, List[Int]()));
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
  
  //leemos elemento de una posicion dada
  def leer_elemento(pos: Int, cont: Int, lista: List[Int]) : Int =
  {
    // es el elemento
    if (pos == cont)
    {
      lista.head;
    }
    else 
    {
      leer_elemento(pos, cont + 1, lista.tail)
    }
  }
  
  //leemos fila de 8 nums cada una
  def leer_fila(pos_fila: Int, cont: Int, num_elems_por_fila: Int, lista: List[Int], lista_nueva: List[Int]) : List[Int] =
  {
    // se han guardado ya los elems de la fila en la lista nueva
    if (cont == (pos_fila + num_elems_por_fila))
    {
      lista_nueva.reverse;
    }
    else 
    {
      // se guardan los elems en la lista nueva
      if ((pos_fila <= cont) && (cont < (pos_fila + num_elems_por_fila)))
      {
        leer_fila(pos_fila, cont + 1, num_elems_por_fila, lista.tail, lista.head :: lista_nueva)
      }
      // no son los elems que hay que guardar
      else
      {
        leer_fila(pos_fila, cont + 1, num_elems_por_fila, lista.tail, lista_nueva)
      }
    }
  }
  
  //leemos columna de 8 nums cada una
  def leer_columna(pos_columna: Int, cont: Int, cont_fila: Int, num_elems_por_columna: Int, lista: List[Int], lista_nueva: List[Int]) : List[Int] =
  {
    // se han guardado ya los elems de la fila en la lista nueva
    if (lista.isEmpty)
    {
      lista_nueva.reverse;
    }
    else 
    {
      // se guardan los elems en la lista nueva
      if (cont == (cont_fila * num_elems_por_columna + pos_columna))
      {
        leer_columna(pos_columna, cont + 1, cont_fila + 1, num_elems_por_columna, lista.tail, lista.head :: lista_nueva)
      }
      // no son los elems que hay que guardar
      else
      {
        leer_columna(pos_columna, cont + 1, cont_fila, num_elems_por_columna, lista.tail, lista_nueva)
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