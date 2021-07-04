import Funciones_tablero._
import IA._

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main
{

  def main(args: Array[String]): Unit =
  {
    val width = 7
    val length = 9

    val tablero1 = crear_tablero(width, length)                                   // Creamos el tablero que esta vacio
    //imprimir_tablero(width, length, tablero)

    val tablero2 = nuevos_diamantes(width, length, tablero1, false)               // Rellenamos el tablero con los diamantes

    val tablero3 = comprobar_fichas_alineadas (width, length, tablero2, false)    // Comprobamos si hay fichas que se pueden eliminar

    // Si ha habido eliminacion de fichas se imprime el tablero inicial, si no no porque apareceria duplicado
    if (tablero2 != tablero3)
    {
      imprimir_tablero(width, length, tablero2)
    }

    jugar (width, length, tablero3)
  }

  /*
  * JUGAR
  * El desarrollo del juego
  */
  @tailrec
  def jugar(width: Int, length: Int, tablero: List[Int]): Unit =
  {

    /*
    * CONTINUAR_JUEGO
    * El juego continua (dando a ENTER) o se detiene poniendo "exit"
    */
    @tailrec
    def continuar_juego(tablero: List[Int]): Boolean =
    {
      println("\n--> Pulsa ENTER para seguir jugando, recibe ayuda con HELP o escribe EXIT para abandonar el juego")
      val jugar: String = readLine()

      // Enter
      if(jugar == "")
      {
        true
      }
      else
      {
        // Exit
        if (jugar.toUpperCase() == "EXIT")
        {
          false
        }
        // Otra cosa
        else
        {
          if ((jugar.toUpperCase() == "AYUDA") || (jugar.toUpperCase() == "HELP"))
          {
            imprimir_tablero(width, length, tablero)
            val profundiad = pedir_profundidad ()
            val intercambio_fichas = ayuda(profundiad, width, length, tablero)
            // No hay ningún movimiento disponible
            if(intercambio_fichas == (-1, -1, -1, -1))
            {
              println("No hay movimientos disponibles")
              false
            // Movimiento recomendado
            } else {
              print("\nfila: "+intercambio_fichas._1+", columna: "+intercambio_fichas._2+" --> fila: "+intercambio_fichas._3+", columna: "+intercambio_fichas._4+"\n")
              true
            }
          }
          else
          {
            println("\n--> No entiendo tu instruccion")
            continuar_juego (tablero)
          }
        }
      }
    }

    imprimir_tablero(width, length, tablero)

    println("-- Introduce la posicion de la ficha que quieres mover --")

    // Pedimos los datos sobre las posiciones de las practicas
    val fila_inicial = pedir_num_fila (length)
    val columna_inicial = pedir_num_columna (width)

    println("\n-- Introduce la posicion a la que quieres mover la ficha --")

    val fila_final = pedir_num_fila (length)
    val columna_final = pedir_num_columna (width)
    println("")

    // Comprobamos que las fichas son adyacentes
    if (comprobar_movimiento_permitido (fila_inicial, columna_inicial, fila_final, columna_final))
    {
      // Movemos las fichas
      val tablero1 = mover_fichas(fila_inicial, columna_inicial, fila_final, columna_final, width, length, tablero)
      println("Mover fichas")
      imprimir_tablero(width, length, tablero1)

      // Comprobamos si hay fichas que se pueden eliminar
      val tablero2 = comprobar_fichas_alineadas (width, length, tablero1, false)

      // Comprobamos que se han eliminado fichas --> si no, se vuelve al tablero antes del movimiento de las fichas
      val tablero3 = comprobar_hay_cambios (fila_inicial, columna_inicial, fila_final, columna_final, width, length, tablero1, tablero2)

      // Continuar el juego o exit
      if (continuar_juego(tablero3))
      {
        // Seguimos el juego
        jugar (width, length, tablero3)
      }
      else
      {
        println("\n ADIOS, VUELVE PRONTO!!")
      }
    }
    else
    {
      println("\n--> Las fichas tienen que ser adyacentes:   ---xo---\n")
      jugar (width, length, tablero)
    }
  }
}