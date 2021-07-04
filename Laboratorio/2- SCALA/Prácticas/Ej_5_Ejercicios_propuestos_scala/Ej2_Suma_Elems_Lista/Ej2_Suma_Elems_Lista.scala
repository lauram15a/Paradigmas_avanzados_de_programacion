// 2. Dada una lista de enteros, realiza una función que devuelva la suma de los elementos que tiene una lista

object Ej2_Suma_Elems_Lista {
  def main (args: Array[String])
  { 
     val lista = List[Int](1, 2, 3, 4, 5, 6); 
     val n : Int = 0;
     println(sumaElems(lista, n));
  }
  
  def sumaElems (lista: List[Int], n: Int) : Int =
  {
    if (lista.isEmpty)
    {
      n;
    }
    else 
    {
      sumaElems(lista.tail, n + lista.head);  // la nueva lista será la lista sin el primer elemento y n será n + el primer elemento de lista
    }
  }
}