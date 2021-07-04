// 5. Realizar una función que sume los cuadrados de todos los enteros entre dos números a y b

//scala.math.pow(n1, 2)

import scala.math.pow

object Ej5_Suma_Cuadrados_Entre_a_Y_b {
  def main (args: Array[String])
  { 
     val n1 : Int = 1;
     val n2 : Int = 5;
     val suma : Int = 0;
     println(sumaCuadrados(n1, n2, suma));
  }
  
  def sumaCuadrados (n1: Int, n2: Int, suma: Int) : Int =
  {
    if (n1 == n2)
    {
      suma;
    }
    else 
    {
      sumaCuadrados(n1 + 1, n2, suma + pow(n1,2).toInt)  //n1 = al num siguiente a n1 y suma es suma + el cuadrado de n1 
    }
  }  
}