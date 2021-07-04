// 8. Escribir una función que, dado un número, devuelva cuántas cifras tiene.
//    Considerar que el número 0 tiene 1 cifra.

object Ej8_Contar_Num_Cifras_De_Numero {
  def main (args: Array[String])
  { 
     val n : Int = 12345;
     val cont : Int = 0;   //contador
     println(contarCifras(n, cont));
  }
  
  def contarCifras (n: Int, cont: Int) : Int =
  {
    if (n <= 0)
    {
      cont;
    }
    else 
    {
      contarCifras(n / 10, cont + 1);
    }
  } 
}