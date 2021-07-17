package ar.edu.utn.frba.tadp.grupo12

import scala.annotation.tailrec

object Casino {
  type Apuestas = List[Apuesta]
  type Probabilidad_Monto = (Double,Double)
  type Puntaje = Double
  //No estoy seguro que que voy a hacer con esto, podría devolver Apuestas.
//  val combinar: Apuestas => List[Apuestas] = apuestas =>  {
//    println(s"apuestas.lenght ${apuestas.length}")
//    apuestas.toSet[Apuesta].subsets.toList
//  }

  def f_max(apuestas:Apuestas, g:List[Apuesta]=>Double):Apuestas = combinar(apuestas).maxBy(apuestas => g(apuestas))
  val prob_ganar: Apuestas => Double = apuestas => apuestas.map(apuesta => probabilidad(apuesta)._1).product
  def f(apuestas:Apuestas, g:List[Apuestas]=>Apuestas):Apuestas = g(combinar(apuestas))
  def combinar[T](seq: Seq[T]) : List[List[T]] = {
    (1 to seq.length).flatMap(i => seq.combinations(i).flatMap(_.permutations)).toList.map(_.toList).distinct
  }

  def probabilidad_de_conjunto(apuestas: Apuestas):Double = apuestas.map(apuesta => probabilidad(apuesta)._1).product

  @tailrec
  def puedo_jugarla(apuestas: List[Apuesta], monto: Double):Boolean={
    if(apuestas.isEmpty) true
    else {
      if(monto >= apuestas.head.monto) {
        val monto_2 = monto - apuestas.head.monto + (apuestas.head.monto * probabilidad(apuestas.head)._2)
        puedo_jugarla(apuestas.tail, monto_2)
      } else
        false
    }
  }


  def planificar(jugador:Jugador, posibles_apuestas:Apuestas): Apuestas ={
    val combinadas = combinar(posibles_apuestas).filter(apuestas => puedo_jugarla(apuestas, jugador.monto))

    combinadas.maxBy(lista_apuesta=>{
      val arbol = ArbolApuestas.generar_arbol_de_apuestas(lista_apuesta, (1, jugador.monto))
      jugador.perfil(arbol.dame_tus_hojas.toList, jugador.monto)
    })
  }

  type Probabilidad_Multiplicador = (Double,Double)
  val probabilidad: Apuesta => Probabilidad_Multiplicador = apuesta =>{
    apuesta.tipo.tipoApuesta match{
      case Cara | Cruz => (DistribucionEquiprobable.probabilidad(List(Cara,Cruz)),2)
      case Rojo | Negro | Par | Impar => (18.0/37.0, 2) //porque el 0 no es par/impar ni negro/rojo
      case PrimerDocena | SegundaDocena | TercerDocena => (12.0/37.0, 3)
      case Numero(_) => (1.0/37.0, 36)
      case MonedaCargada(loquequiero, total) => (DistribucionPonderada.probabilidad(loquequiero, total), 2)
    }
  }

  //me da un random [0,hasta), sin incluir la posibilidad que de el valor hasta.
  def dame_un_random(menor_a: Int): Int ={
    scala.util.Random.nextInt(menor_a)
  }

  // Auxiliar para método evaluar, ayuda a obtener el resultado de la apuesta
  def sale(tipoApuesta: TipoApuesta): Boolean = {
    tipoApuesta match {
      case Cara =>  dame_un_random(2) == 0
      case Cruz =>  dame_un_random(2) == 1
      case MonedaCargada(loquequiero, total) => List.range(1, loquequiero).contains(dame_un_random(total))
      case Rojo => List[Int](1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36).contains(dame_un_random(37))
      case Negro => List[Int](2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35).contains(dame_un_random(37))
      case Par  => dame_un_random(37) % 2 == 0
      case Impar => dame_un_random(37) % 2 == 1
      case PrimerDocena => List.range(1,13).contains(dame_un_random(37))
      case SegundaDocena => List.range(13,25).contains(dame_un_random(37))
      case TercerDocena => List.range(25,37).contains(dame_un_random(37))
      case Numero(numero) => numero == dame_un_random(37)
    }
  }

  // Dada una apuesta y un jugador evalúa si este gana la apuesta y paga el monto.
  def evaluar(apuesta:Apuesta, resultado: State): State={
    if (sale(apuesta.tipo.tipoApuesta)) {
      resultado.gana(apuesta.monto * probabilidad(apuesta)._2)
    } else{
      resultado
    }
  }

  def jugar(apuestas: Apuestas, jugador: Jugador): State ={
    apuestas.foldLeft(State(jugador)){
      (resultado_previo, apuesta) =>
        resultado_previo match{
          case Betting(jugador) => if(jugador.puedePagar(apuesta)) resultado_previo.hacerApuesta(apuesta) else {
            //println(s"[${jugador.nombre}] no puede pagar ${apuesta.monto} porque solo tiene ${jugador.monto}")
            StandBy(jugador)
          }
          case StandBy(jugador) => jugar(apuestas.tail,jugador)
        }
    }
  }
}