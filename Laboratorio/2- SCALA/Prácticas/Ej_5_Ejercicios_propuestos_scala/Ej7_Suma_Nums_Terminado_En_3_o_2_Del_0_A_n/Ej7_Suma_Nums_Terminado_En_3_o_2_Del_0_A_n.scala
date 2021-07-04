// 7. Escribir una función que sume los números que terminen en 2 o en 3 del 0 al N, 
//    utilizando la función sumf1n definida anteriormente.

object Ej7_Suma_Nums_Terminado_En_3_o_2_Del_0_A_n {
  def main (args: Array[String])
  { 
     val n1 : Int = 0;
     val n2 : Int = 5;
     val suma : Int = 0;
     println(sumf1n(n1, n2, suma));
  }
  
  def sumf1n (n1: Int, n2: Int, suma: Int) : Int =
  {
    if (n1 == n2)
    {
      suma;
    }
    else 
    {
      //si el num acaba en 2 o en 3
      if ((n1 % 10 == 2) || (n1 % 10 == 3))
      {
        sumf1n(n1 + 1, n2, suma + n1);  //n1 = al num siguiente a n1 y suma es suma + n1 
      }
      //resto de terminaciones
      else
      {
        sumf1n(n1 + 1, n2, suma);      //n1 = al num siguiente a n1 
      }
    }
  } 
}