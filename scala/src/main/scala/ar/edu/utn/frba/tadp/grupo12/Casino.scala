package ar.edu.utn.frba.tadp.grupo12

import scala.annotation.tailrec

object Casino {
  type Apuestas = List[Apuesta]
  type Probabilidad_Monto = (Double,Double)
  type Puntaje = Double

  def f_max(apuestas:Apuestas, g:List[Apuesta]=>Double):Apuestas = combinar(apuestas).maxBy(apuestas => g(apuestas))
  val prob_ganar: Apuestas => Double = apuestas => apuestas.map(apuesta => probabilidad(apuesta)._1).product
  def f(apuestas:Apuestas, g:List[Apuestas]=>Apuestas):Apuestas = g(combinar(apuestas))
  def combinar[T](seq: Seq[T]) : List[List[T]] = {
    (1 to seq.length).flatMap(i => seq.combinations(i).flatMap(_.permutations)).toList.map(_.toList).distinct
  }

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
      //Piedra gana solo cuando sale tijera con un 40% de probabilidad
      case Piedra => (DistribucionPonderada.probabilidad(40, 100), 2)
      //Papel gana solo cuando sale piedra con un 35% de probabilidad
      case Papel => (DistribucionPonderada.probabilidad(35, 100), 2)
      //Tijera gana solo cuando sale papel con un 25% de probabilidad
      case Tijera => (DistribucionPonderada.probabilidad(25, 100), 2)
    }
  }

  //me da un random [0,hasta), sin incluir la posibilidad que de el valor hasta.
  def dame_un_random(menor_a: Int): Int ={
    scala.util.Random.nextInt(menor_a)
  }

  // Auxiliar para método evaluar, ayuda a obtener el resultado de la apuesta
  // Si es null, significa que empató. Si es true, ganó, y si es false, perdió.
  def sale(tipoApuesta: TipoApuesta): Option[Boolean] = {
    tipoApuesta match {
      case Cara =>  Option(dame_un_random(2) == 0)
      case Cruz =>  Option(dame_un_random(2) == 1)
      case MonedaCargada(loquequiero, total) => Option(List.range(1, loquequiero).contains(dame_un_random(total)))
      case Rojo => Option(List[Int](1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36).contains(dame_un_random(37)))
      case Negro => Option(List[Int](2,4,6,8,10,11,13,15,17,20,22,24,26,28,29,31,33,35).contains(dame_un_random(37)))
      case Par  => Option(dame_un_random(37) % 2 == 0)
      case Impar => Option(dame_un_random(37) % 2 == 1)
      case PrimerDocena => Option(List.range(1,13).contains(dame_un_random(37)))
      case SegundaDocena => Option(List.range(13,25).contains(dame_un_random(37)))
      case TercerDocena => Option(List.range(25,37).contains(dame_un_random(37)))
      case Numero(numero) => Option(numero == dame_un_random(37))
      case (piedraPapelOTijera: PiedraPapelOTijera) => piedraPapelOTijera.leGana(sale_piedraPapelOTijera())
    }
  }

  def sale_piedraPapelOTijera(): PiedraPapelOTijera ={
    var queSale = dame_un_random(100)
    var piedra = List.range(1, 35)
    var papel = List(1, 25)
    //La tijera tiene la probabilidad restante

    if(piedra.contains(queSale)){
      Piedra
    } else if(papel.contains(queSale)){
      Papel
    } else {
      Tijera
    }
  }



  // Dada una apuesta y un jugador evalúa si este gana la apuesta y paga el monto.
  def evaluar(apuesta:Apuesta, resultado: State): State={
    val salio = sale(apuesta.tipo.tipoApuesta)

    if (salio.isEmpty) {
      resultado.gana(apuesta.monto)
    } else if(salio.get){
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