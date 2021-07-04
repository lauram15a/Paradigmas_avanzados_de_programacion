// 1. Realiza una función iota que genere una lista con todos los enteros comprendidos entre 1 y n. 
//    Para n=0 devuelve la lista vacía.

object Ej1_Listas {
  def main (args: Array[String])
  { 
     val lista = List[Int](); 
     val n : Int = 5;
     println(aniadirValores(lista,n));
  }
  
  def aniadirValores (lista: List[Int], n: Int) : List[Int] =
  {
    if (n == 0)
    {
      lista;
    }
    else 
    {
      aniadirValores(n :: lista, n - 1);  //meto n en la lista
    }
  }
}