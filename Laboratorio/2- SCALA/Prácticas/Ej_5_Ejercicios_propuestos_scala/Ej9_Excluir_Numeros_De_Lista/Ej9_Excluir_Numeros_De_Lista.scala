// 9. Realiza dos funciones toma n l y deja n l, que devuelvan una nueva lista con los n primeros 
//    o sin los n primeros elementos de la lista l, respectivamente.

object Ej9_Excluir_Numeros_De_Lista {
  def main (args: Array[String])
  { 
     val lista = List[Int](1, 2, 3, 4, 5, 6, 7, 8); 
     val lista_nueva = List[Int](); 
     val n : Int = 3;
     val cont : Int = 0;  // contador
     println(toma(lista, lista_nueva));
     println(deja(lista, lista_nueva, n, cont));
  }
  
  //Lista con los n primeros nums
  def toma (lista: List[Int], lista_nueva: List[Int]) : List[Int] =
  {
    if (lista.isEmpty)
    {
      lista_nueva.reverse;
    }
    else 
    {
      toma(lista.tail, lista.head :: lista_nueva);  
    }
  }
  
  // Lista sin los n primeros nums
  def deja(lista: List[Int], lista_nueva: List[Int], n: Int, cont: Int) : List[Int] =
  {
    if (lista.isEmpty)
    {
      lista_nueva.reverse;
    }
    else 
    {
      //no coge los primeros elems
      if (n > cont)
      {
        deja(lista.tail, lista_nueva, n, cont + 1);
      }
      // coge los siguientes nums
      else
      {
        deja(lista.tail, lista.head :: lista_nueva, n, cont + 1);
      }
    }
  }
}