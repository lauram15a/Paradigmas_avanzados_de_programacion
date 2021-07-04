import Funciones_tablero._
import scala.io.StdIn.readLine
import scala.collection.parallel.immutable.ParVector

object IA 
{

    /*
    * AYUDA
    * Le indica al jugador que fichas intercambiar
    * El formato de la tupla de salida es: (x0, y0, x1, y1)
    */
    def ayuda(profundidad: Int, width: Int, length: Int, tablero: List[Int]): (Int, Int, Int, Int) =
    {

        /*
        * AYUDA_AUX
        * Genera de forma recursiva los movimientos posibles en la profundidad introducida
        */
        def ayuda_aux(profundidad: Int, genera_movimientos: List[Int] => ParVector[(Int, Int, Int, Int, List[Int], Int)], movimientos: ParVector[(Int, Int, Int, Int, List[Int], Int)]): ParVector[(Int, Int, Int, Int, List[Int], Int)] =
        {

            /*
            * SIGUIENTE_PROFUNDIDAD
            * Genera un ParVector con las tuplas obtenidas a partir de un tupla incial
            */
            def siguiente_profundidad(genera_movimientos: List[Int] => ParVector[(Int, Int, Int, Int, List[Int], Int)], movimiento: (Int, Int, Int, Int, List[Int], Int)): ParVector[(Int, Int, Int, Int, List[Int], Int)] =
            {
                val movimiento_siguiente_profundidad = genera_movimientos(movimiento._5)
                if(movimiento_siguiente_profundidad.isEmpty)
                {
                    // Guarda la tupla inicial para no perder esta opción si puede llegar a ser el mejor movimiento al no tener tuplas hijas
                    ParVector(movimiento)
                }
                else
                {
                    // Guarda en la tupla el movimiento de la tupola padre mientras que se guarda el nuevo tablero
                    movimiento_siguiente_profundidad.map((x) => (movimiento._1, movimiento._2, movimiento._3, movimiento._4, x._5, x._6))
                }
            }

            if(profundidad == 0)
            {
                movimientos
            }
            else
            {
                // Busqueda en la siguiente profundidad y unión de todos los ParVector que estos producen
                val movimientos_siguiente_profundidad = movimientos.map((x) => siguiente_profundidad(genera_movimientos, x)).flatten

                ayuda_aux(profundidad-1, genera_movimientos, movimientos_siguiente_profundidad)
            }
        }

        // Función movimientos_posibles currificada con los valores de dimensión del tablero
        val movimientos_posibles_currying = movimientos_posibles(width)(length)_

        // Busqueda de los movimientos posibles de la primera profundidad
        val movimientos = movimientos_posibles_currying(tablero)

        // Busca en profundidad el resto de tuplas tras la busqueda en la primera profundidad
        val movimientos_profundidad = ayuda_aux(profundidad-1, movimientos_posibles_currying, movimientos)
        
        // Se utiliza fold y no reduce para que ocurra ningún error en el caso de que solo haya un movimiento
        // El valor inicial elegido es uno que siempre será peor que el resto (-1)
        val recomendacion = movimientos_profundidad.fold((-1,-1,-1,-1,List(),-1))((x, y) => if(x._6>y._6){x}else{y})
        (recomendacion._1, recomendacion._2, recomendacion._3, recomendacion._4)
    }

    /*
    * MOVIMIENTOS_POSIBLES
    * Genera un ParVector de tuplas que contienen el movimiento actual de la ficha junto con su tablero asociado
    * El formato de las tuplas es: (x0, y0, x1, y1, tablero, número de diamantes eliminados)
    */
    def movimientos_posibles(width: Int)(length: Int)(tablero: List[Int]): ParVector[(Int, Int, Int, Int, List[Int], Int)] =
    {

        /*
        * MOVIMIENTOS_POSIBLES_AUX
        * Itera por cada posición del tablero para generar un nodo para cada tablero al que se puede llegar
        */
        def movimientos_posibles_aux(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int], Int)]): ParVector[(Int, Int, Int, Int, List[Int], Int)] =
        {

            /*
            * MOVIMIENTO_DERECHA
            * Genera un nodo si el posible mover el dimante a la derecha
            */
            def movimiento_derecha(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int], Int)]): ParVector[(Int, Int, Int, Int, List[Int], Int)] =
            {
                // Se realiza con una doble condición para tratar de evitar el computo de nuevos tableros
                if(y+1 != width)
                {
                    val nuevo_tablero = eliminar_fichas_ia(width, length, mover_fichas(x, y, x, y+1, width, length, tablero))
                    val contador = nuevo_tablero.count(_ == -1)
                    if(contador != tablero.count(_ == -1))
                    {
                        nodos :+ (x, y, x, y+1, nuevo_tablero, contador)
                    }
                    else
                    {
                        nodos
                    }
                } 
                else 
                {
                    nodos
                }
            }

            /*
            * MOVIMIENTO_ABAJO
            * Genera un nodo si el posible mover el dimante abajo
            */
            def movimiento_abajo(x: Int, y: Int, width: Int, length: Int, tablero: List[Int], nodos: ParVector[(Int, Int, Int, Int, List[Int], Int)]): ParVector[(Int, Int, Int, Int, List[Int], Int)] =
            {
                // Se realiza con una doble condición para tratar de evitar el computo de nuevos tableros
                if(x+1 != length)
                {
                    val nuevo_tablero = eliminar_fichas_ia(width, length, mover_fichas(x, y, x+1, y, width, length, tablero))
                    val contador = nuevo_tablero.count(_ == -1)
                    if(contador != tablero.count(_ == -1))
                    {
                        nodos :+ (x, y, x+1, y, nuevo_tablero, contador)
                    }
                    else
                    {
                        nodos
                    }
                } 
                else 
                {
                    nodos
                }
            }
            
            /*
            * ELIMINAR_FICHAS_IA
            * Borra las fichas alineadas y llena las posiciones de diamantes -1 comprobando que tras caer los diamantes no se puedan eliminar aún más
            */
            def eliminar_fichas_ia(width: Int, length: Int, tablero: List[Int]): List[Int] =
            {
                val nuevo_tablero = comprobar_fichas_alineadas(width, length, tablero, true)
                if(tablero == nuevo_tablero) 
                {
                    tablero
                }
                else 
                {
                    eliminar_fichas_ia(width, length, nuevo_tablero)
                }
            }
            
            val vector1 = movimiento_derecha(x, y, width, length, tablero, nodos)
            val vector2 = movimiento_abajo(x, y, width, length, tablero, vector1)

            // no hay mas que comprobar
            if(x+1 == length && y+1 == width)
            {
                vector2
            } 
            else 
            {
                movimientos_posibles_aux(if(y+1==width){x+1}else x, if(y+1==width){0}else y+1, width, length, tablero, vector2)
            }
        }

        movimientos_posibles_aux(0,0,width,length,tablero, new ParVector())
    }

  /*
  * PEDIR_PROFUNDIDAD
  * Pide el dato por pantalla y comprueba que sea un entero positivo
  */
  def pedir_profundidad () : Int =
  {
    println("Profundidad: ")
    try
    {
      //Pedir dato por pantalla
      val profundidad: Int = readLine().toInt

      // si no es positivo
      if (profundidad <= 0)
      {
        println("\nTiene que ser un numero mayor de 0\n")
        pedir_profundidad()
      }
      else
      {
        profundidad
      }
    }
    catch
    {
      case _: NumberFormatException =>
        println("\nTiene que ser un numero entero\n")
        pedir_profundidad ()
    }
  }
}
