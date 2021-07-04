import scala.annotation.tailrec
import Funciones_lista._

import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.parallel.immutable.ParVector

object Funciones_tablero
{

  /*
  * CREAR_TABLERO
  * Creación de un tablero de las dimensiones introducidas
  */
  def crear_tablero(width: Int, length: Int): List[Int] =
  {
    List.fill(width*length)(0)
  }

  /*
  * IMPRIMIR_TABLERO
  * Imprime con diferentes colores el tablero introducido en forma de lista
  * (No se puede realizar con ParVector ya que se puede imprimir en desorden)
  */
  def imprimir_tablero(width: Int, length: Int, tablero: List[Int]): Unit =
  {

    /*
    * PUNTOS
    * Imprime una línea horizontal con el número de puntos introducido
    */
    @tailrec
    def puntos(n: Int): Unit =
    {
      if(n == 0)
      {
        println()
      }
      else
      {
        print("·")
        puntos(n-1)
      }
    }

    /*
    * IMPRIMIR_FILAS_TABLERO
    * Imprime las diferentes filas del tablero
    */
    @tailrec
    def imprimir_filas_tablero(num_fila:Int, width: Int, length: Int, tablero: List[Int]): Unit =
    {

      /*
      * IMPRIMIR_FILA_TABLERO
      * Imprime cada una de las filas del tablero
      */
      @tailrec
      def imprimir_fila_tablero(width: Int, tablero: List[Int]): List[Int] =
      {
        if (width == 0)
        {
          print(Console.RESET + "·")
          tablero
        }
        else
        {
          print(Console.RESET + "·")
          (tablero.head: Int) match
          {
            case 0 => print(" ")
            case 1 => print(Console.BLACK + "1")
            case 2 => print(Console.RED + "2")
            case 3 => print(Console.GREEN + "3")
            case 4 => print(Console.YELLOW + "4")
            case 5 => print(Console.BLUE + "5")
            case 6 => print(Console.MAGENTA + "6")
            case 7 => print(Console.CYAN + "7")
            case 8 => print(Console.WHITE + "8")
            case -1 => println("Imprimiendo un valor no definido\n")
            case _ => println("Error\n")
          }
          imprimir_fila_tablero(width - 1, tablero.tail)
        }
      }

      if (length != 0)
      {
        print(num_fila)
        print("| ")
        val tablero_restante = imprimir_fila_tablero(width, tablero)
        println()
        imprimir_filas_tablero(num_fila + 1, width, length - 1, tablero_restante)
      }
    }

    println("")
    print("   |")
    imprimir_posiciones_columnas(0, width)
    print("   ")
    puntos(width * 2 + 1)
    imprimir_filas_tablero(0, width, length, tablero)
    print("   ")
    puntos(width * 2 + 1)
    println("")
  }

  /*
  * IMPRIMIR_POSICIONES_COLUMNAS
  * Imprime encima del tablero una fila de numeros para que se vean mejor el numero de cada columna
  */
  @tailrec
  def imprimir_posiciones_columnas(num: Int, width: Int): Unit =
  {
    if (num == width)
    {
      println("")
    }
    else
    {
      print(num)
      print("|")
      imprimir_posiciones_columnas(num + 1, width)
    }
  }

  /*
  * QUEDAR_POR_CAER
  * Hace caer los diamantes del tablero introducido y caen nuevas fichas
  */
  def nuevos_diamantes(width: Int, length: Int, tablero: List[Int]): List[Int] =
  {

    /*
    * QUEDAR_POR_CAER
    * Hace caer cada diamante una posición hacia abajo hasta que todos los diamantes se encuentran sobre otro diamante
    * (No se puede realizar con ParVector ya que sino unas piezas se podrían mover sobre otras o hacer desaparecer alguna)
    */
    @tailrec
    def caer_diamantes(x: Int, y: Int, width: Int, length: Int, tablero: List[Int]): List[Int] =
    {

      /*
      * QUEDAR_POR_CAER
      * Comprueba si falta por caer algun diamante en todo el tablero
      */
      @tailrec
      def quedar_por_caer(columnas_restantes: Int, width: Int, length: Int, tablero: List[Int], columnas: ParVector[List[Int]]): Boolean =
      {

        /*
        * QUEDAR_POR_CAER_COLUMNA
        * Comprueba si falta por caer algun diamante en una columna del tablero
        */
        @tailrec
        def quedar_por_caer_columna(encima: Int)(columna: List[Int]): Boolean =
        {
          if (columna.isEmpty)
          {
            false
          }
          else
          {
            if (columna.head != 0)
            {
              quedar_por_caer_columna(columna.head)(columna.tail)
            }
            else
            {
              if (encima == 0)
              {
                quedar_por_caer_columna(columna.head)(columna.tail)
              }
              else
              {
                true
              }
            }
          }
        }

        if(columnas_restantes == 0)
        {
          // Currying
          columnas.map(quedar_por_caer_columna(0)_).reduce(_ || _)
        }
        else
        {
          quedar_por_caer(columnas_restantes-1, width, length, tablero, columnas :+ column(columnas_restantes-1, width, length, tablero))
        }
      }

      if(length == x)
      {
        if(quedar_por_caer(width, width, length, tablero, new ParVector()))
        {
          caer_diamantes(0, 0, width, length, tablero)
        }
        else
        {
          tablero
        }
      }
      else
      {
        val diamante = get(x,y,width,length,tablero)
        if(x+1 != length && diamante != 0 && get(x+1,y,width,length,tablero) == 0)
        {
          val tablero_ = set(diamante,x+1,y,width,length,set(0,x,y,width,length,tablero))
          if(y+1 == width)
          {
            caer_diamantes(x+1,0,width,length,tablero_)
          }
          else
          {
            caer_diamantes(x,y+1,width,length,tablero_)
          }
        }
        else
        {
          if(y+1 == width)
          {
            caer_diamantes(x+1,0,width,length,tablero)
          }
          else
          {
            caer_diamantes(x,y+1,width,length,tablero)
          }
        }
      }
    }

    /*
    * COLOCAR_DIAMANTES
    * Se colocan fichas con un valor entre el 1 y el 8 en las posiciones sin ninguna de estas
    */
    def colocar_diamantes(tablero: List[Int]): List[Int] =
    {
      tablero.par.map(x => if(x == 0){new Random().nextInt(8)+1}else x).toList
    }
    
    val tablero_ = caer_diamantes(0, 0, width, length, tablero)
    colocar_diamantes(tablero_)
  }

  /*
  * MOVER_FICHAS
  * Intercambia la ficha de la posición indicada por la ficha de la posición en la que queremos poner la ficha principal
  */
  def mover_fichas (fila_inicial: Int, columna_inicial: Int, fila_final: Int, columna_final: Int, width: Int, length: Int, tablero: List[Int]): List[Int] =
  {
    val ficha_ppal = get(fila_inicial, columna_inicial, width, length, tablero)              // La ficha que se quiere mover, la principal
    val ficha_secun = get(fila_final, columna_final, width, length, tablero)                 // La ficha por la que se ha movido la principal, es decir, la secundaria

    val tablero_ = set(ficha_ppal, fila_final, columna_final, width, length, tablero)        // Cambiamos la ficha ppal
    set(ficha_secun, fila_inicial, columna_inicial, width, length, tablero_)                 // Cambiamos la ficha secundaria
  }

  /*
  * COMPROBAR_FICHAS_ALINEADAS
  * Comprueba fichas alineadas
  * Devuelve el tablero
  * (Es funcionamiento de la función es secuencial por lo que no se puede aplicar ParVector de ninguna manera)
  */
  def comprobar_fichas_alineadas (width: Int, length: Int, tablero: List[Int]) : List[Int] =
  {

    /*
    * COMPROBAR_FICHAS_ALINEADAS_FILA
    * Comprueba fila a fila si hay fichas alineadas hasta que encuentra que en una si se da el caso
    * Devuelve el tablero
    */
    @tailrec
    def comprobar_fichas_alineadas_fila(num_fila: Int, width: Int, length: Int, tablero: List[Int]) : List[Int] =
    {

      /*
      * ELIMINAR_FICHAS_FILA
      * Las fichas que se eliminan se ponen con valor 0
      */
      @tailrec
      def eliminar_fichas_fila(valor: Int, cont: Int, num_fila: Int, num_columna: Int, width: Int, length: Int, tablero: List[Int]) : List[Int] =
      {
        // Ya se ha revisado toda la fila y/o ya se han eliminado las fichas
        if (((num_columna + 1) > width) || (cont == Int.MaxValue))
        {
          tablero
        }
        else
        {
          // Se cambian los elementos sucesivos
          if (valor == get(num_fila, num_columna, width, length, tablero))
          {
            val tablero_ = set( 0, num_fila, num_columna, width, length, tablero)
            eliminar_fichas_fila(valor, cont + 1, num_fila, num_columna + 1, width, length, tablero_)
          }
          // El elem no se elimina
          else 
          {
            (cont: Int) match
            {
              // cont = 0 --> No se ha modificado previamente el tablero en ningun momento
              case 0 => eliminar_fichas_fila(valor, cont, num_fila, num_columna + 1, width, length, tablero)
                // cont == 1 --> el tablero ha sido modificado previamente --> No tienen el minimo necesario para eliminarse --> se les vuelve a asignar su valor actual
              case 1 => eliminar_fichas_fila(valor, 0, num_fila, num_columna + 1, width, length, set(valor, num_fila, num_columna - 1, width, length, tablero)) // Se vuelve a poner el cont a 0
                // cont == 2 -->  el tablero ha sido modificado previamente --> No tienen el minimo necesario para eliminarse --> se les vuelve a asignar su valor actual
              case 2 => eliminar_fichas_fila(valor, 0, num_fila, num_columna + 1, width, length, set(valor, num_fila, num_columna - 2, width, length, set(valor, num_fila, num_columna - 1, width, length, tablero))) // Se vuelve a poner el cont a 0
              // cont >= 3 --> Ya se han eliminado las fichas
              case _ => eliminar_fichas_fila(valor, Int.MaxValue, num_fila, num_columna + 1, width, length, tablero) // Asignamos al contador el número entero mayor posible para que nunca coincida --> necesario para la condicion de parada
            }
          }
        }
      }

      // Ya no hay mas filas que comprobar
      if (num_fila ==  length)
      {
        tablero
      }
      // Sigue habiendo filas que comprobar
      else
      {
        val fila = row(num_fila, width, length, tablero)
        val valor = comprobar_fichas_alineadas_aux(fila, tablero)

        (valor: Int) match
        {
          // valor = 0 --> no hay fichas alineadas
          case 0 => comprobar_fichas_alineadas_fila (num_fila + 1, width, length, tablero)
          // valor !=0 --> Si hay fichas alineadas 
          case _ => comprobar_fichas_alineadas (width, length, nuevos_diamantes(width, length, eliminar_fichas_fila(valor, 0, num_fila, 0, width, length, tablero)))
        }
      }
    }

    /*
    * COMPROBAR_FICHAS_ALINEADAS_COLUMNA
    * Comprueba columna a columna si hay fichas alineadas hasta que encuentra que en una si se da el caso
    * Devuelve el tablero
    */
    @tailrec
    def comprobar_fichas_alineadas_columna(num_columna: Int, width: Int, length: Int, tablero: List[Int]) : List[Int] =
    {

      /*
      * ELIMINAR_FICHAS_COLUMNA
      * Elimina las fichas de la columna
      */
      @tailrec
      def eliminar_fichas_columna(valor: Int, cont: Int, num_fila: Int, num_columna: Int, width: Int, length: Int, tablero: List[Int]) : List[Int] =
      {
        // se ha recorrido toda la columna y/o ya se han eliminado las fichas
        if (((num_fila + 1) > length) || (cont == Int.MaxValue))
        {
          tablero
        }
        else
        {
          // Se cambian los elementos sucesivos
          if (valor == get(num_fila, num_columna, width, length, tablero))
          {
            val tablero_ = set( 0, num_fila, num_columna, width, length, tablero)
            eliminar_fichas_columna(valor, cont + 1, num_fila + 1, num_columna, width, length, tablero_)
          }
          // El elem no se elimina
          else
          {
            (cont: Int) match
            {
              // cont == 0 --> No se ha modificado previamente el tablero en ningun momento
              case 0 => eliminar_fichas_columna(valor, cont, num_fila + 1, num_columna, width, length, tablero)
              // cont == 1 --> el tablero ha sido modificado previamente --> no tienen el minimo necesario para eliminarse
              case 1 => eliminar_fichas_columna(valor, 0, num_fila + 1, num_columna, width, length, set( valor, num_fila - 1, num_columna, width, length, tablero))   // Se vuelve a poner el cont a 0
              // cont == 2 --> el tablero ha sido modificado previamente --> no tienen el minimo necesario para eliminarse
              case 2 => eliminar_fichas_columna(valor, 0, num_fila + 1, num_columna, width, length, set( valor, num_fila - 2, num_columna, width, length, set( valor, num_fila - 1, num_columna, width, length, tablero)))   // Se vuelve a poner el cont a 0
              // cont >= 3 --> Ya se han eliminado las fichas
              case _ => eliminar_fichas_columna(valor, Int.MaxValue, num_fila + 1, num_columna, width, length, tablero)   // Asignamos al contador el númeo entero mayor posible para que nunca coincida --> necesario para la condicion de parada
            }
          }
        }
      }

      // Ya no hay mas columnas que comprobar
      if (num_columna == width)
      {
        tablero
      }
      // Sigue habiendo filas que comprobar
      else
      {
        val columna = column(num_columna, width, length, tablero)
        val valor = comprobar_fichas_alineadas_aux(columna, tablero)

        (valor: Int) match
        {
            // valor == 0 --> No hay fichas alineadas
            case 0 => comprobar_fichas_alineadas_columna (num_columna + 1, width, length, tablero)
            // valor != 0 --> Si hay fichas alineadas 
            case _ => comprobar_fichas_alineadas (width, length, nuevos_diamantes(width, length, eliminar_fichas_columna(valor, 0, 0, num_columna, width, length, tablero)))
        }
      }
    }

    /*
    * COMPROBAR_FICHAS_ALINEADAS_AUX
    * Comprueba si hay 3 o más fichas alineadas
    * eedd = estructura de datos (fila y/o columna)
    * Devuelve el valor que se ha repetido
    */
    @tailrec
    def comprobar_fichas_alineadas_aux(eedd: List[Int], tablero: List[Int]) : Int =
    {

      /*
      * CONTAR
      * Cuenta cuantas veces esta repetido un valor sucesivamente
      * eedd = estructura de datos (fila y/o columna)
      */
      @tailrec
      def contar(valor: Int, eedd: List[Int], contador: Int) : Int =
      {
        // Si la fila esta vacia o el valor no es igual al sucesor
        if (eedd.isEmpty || (valor != eedd.head))
        {
          contador
        }
        // Si el valor es igual al elemento sucesor
        else
        {
          contar (valor, eedd.tail, contador + 1)   // Se suma 1 al contador y se pasa el resto de la fila
        }
      }

      // La eedd esta vacia
      if (eedd.isEmpty)
      {
        0
      }
      // La eeddd no esta vacia
      else
      {
        val valor = eedd.head
        val contador = 0                                          // Cuenta las veces que está repetido un valor

        // Si el valor es no esta 3 veces seguidas o más
        if (contar (valor, eedd, contador) < 3)
        {
          comprobar_fichas_alineadas_aux(eedd.tail, tablero)     // Comprueba con el siguiente elemento
        }
        // Si el valor esta repetido 3 o más
        else
        {
          valor
        }
      }
    }

    // Comprobamos mirando la fila
    val tablero1 = comprobar_fichas_alineadas_fila (0, width, length, tablero)

    // Comprobamos mirando la columna
    comprobar_fichas_alineadas_columna (0, width, length, tablero1)
  }

  /*
  * COMPROBAR_HAY_CAMBIOS
  * Comprobar si el tablero final y el inicial son iguales
  */
  def comprobar_hay_cambios (fila_inicial: Int, columna_inicial: Int, fila_final: Int, columna_final: Int, width: Int, length: Int, tablero1: List[Int], tablero2: List[Int]) : List[Int] =
  {
    // Si no se ha hecho ningun cambio --> devuelve el tablero inicial, sin el cambio previo de posicion entre fichas
    if (tablero1 == tablero2)
    {
      println("No se ha conseguido eliminar fichas --> las fichas vuelven a sus posiciones originales")
      mover_fichas (fila_inicial, columna_inicial, fila_final, columna_final, width, length, tablero1)
    }
    // Si se han producido cambios
    else
    {
      tablero2
    }
  }

  /*
  * PEDIR_NUM_FILA
  * Pide el dato por pantalla y cpmrueba que esté dentro del rango
  */
  def pedir_num_fila (length: Int) : Int =
  {
    println("Fila (0-8): ")
    try
    {
      //Pedir dato por pantalla
      val num_fila: Int = readLine().toInt

      // si no esta dentro del rango, se vuelve a pedir el valor
      if ((num_fila < 0) || (num_fila > (length - 1)))
      {
        println("\nEl valor debe estar dentro del rango [0, 8]\n")
        pedir_num_fila(length)
      }
      else
      {
        num_fila
      }
    }
    catch
    {
      case _: NumberFormatException =>
        println("Tiene que ser un numero entero")
        pedir_num_fila (length)
    }
  }

  /*
  * PEDIR_NUM_COLUMNA
  * Pide el dato por pantalla y cpmrueba que esté dentro del rango
  */
  def pedir_num_columna (width: Int) : Int =
  {
    println("Columna (0-6): ")
    try
    {
      //Pedir dato por pantalla
      val num_columna: Int = readLine().toInt

      // si no esta dentro del rango, se vuelve a pedir el valor
      if ((num_columna < 0) || (num_columna > (width - 1)))
      {
        println("\nEl valor debe estar dentro del rango [0, 6]\n")
        pedir_num_columna(width)
      }
      else
      {
        num_columna
      }
    }
    catch
    {
      case _: NumberFormatException =>
        println("Tiene que ser un numero entero")
        pedir_num_columna (width)
    }
  }

  /*
  * COMPROBAR_MOVIMIENTO_PERMITIDO
  * Comprueba que las fichas a mover sean adyacentes entre ellas
  */
  def comprobar_movimiento_permitido (fila_inicial: Int, columna_inicial: Int, fila_final: Int, columna_final: Int): Boolean =
  {
    // Movimiento en la misma fila
    if (fila_inicial == fila_final)
    {
      // ---xox---    o = columna_inicial  ||  x = columna_final
      if (((columna_inicial + 1) == columna_final) || ((columna_inicial - 1) == columna_final))
      {
        true
      }
      // --x-o--x-    o = columna_inicial  ||  x = columna_final
      else
      {
        false
      }
    }
    // No movimiento en la misma fila
    else
    {
      // Movimiento en la misma columna
      if (columna_inicial == columna_final)
      {
        // ---xox---    o = fila_inicial  ||  x = fila_final
        if (((fila_inicial + 1) == fila_final) || ((fila_inicial - 1) == fila_final))
        {
          true
        }
        // --x-o--x-    o = fila_inicial  ||  x = fila_final
        else
        {
          false
        }
      }
      // No movimiento en la misma columna
      else
      {
        false
      }
    }
  }
}