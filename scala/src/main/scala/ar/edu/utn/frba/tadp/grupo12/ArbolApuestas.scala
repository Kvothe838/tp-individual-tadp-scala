package ar.edu.utn.frba.tadp.grupo12

import ar.edu.utn.frba.tadp.grupo12.Casino.{Apuestas, probabilidad}

import scala.annotation.tailrec

trait ArbolApuestas[+A]{
  def contar_hojas: Int ={
    @tailrec //optimizacion para el loop
    def loop(a: List[ArbolApuestas[A]], z: Int): Int = a match {
      case (hoja: HojaApuesta[A]) :: cola => loop(cola, z + 1)
      case (rama: RamaApuestas[A]) :: cola => loop(rama.izq :: rama.der :: cola, z)
      case _ :: cola            => loop(cola, z)
      case _                  => z
    }
    loop(List(this),  0)
  }
  def altura: Int = {
    def loop(arbol: ArbolApuestas[A]): Int = arbol match {
      case hoja: HojaApuesta[A] => 1
      case rama: RamaApuestas[A] => Seq(loop(rama.izq), loop(rama.der)).max + 1
      case _          => 0
    }
    loop(this) - 1
  }

  def dame_tus_hojas: Seq[A] ={
    @tailrec
    def loop(a: List[ArbolApuestas[A]], z:Seq[A]): Seq[A] ={
      a match {
        case (h:HojaApuesta[A]) :: cola => loop(cola, z.appended(h.value))
        case (rama: RamaApuestas[A]) :: cola => loop(rama.izq :: rama.der :: cola, z)
        case _ :: cola            => loop(cola, z)
        case _                  => z
      }
    }
    loop(List(this),Seq())
  }
}
case class HojaApuesta[A](value: A) extends ArbolApuestas[A]
case class RamaApuestas[A](izq: ArbolApuestas[A], der: ArbolApuestas[A]) extends ArbolApuestas[A]

object ArbolApuestas {
  def tupla_resultado_positivo(apuesta:Apuesta, estado:(Double,Double)):(Double,Double) ={
    if(estado._2 >= apuesta.monto){
      //      println(s"para resultado positivo ${apuesta.tipo}:${apuesta.monto} estado:${estado} => estado=>${(probabilidad(apuesta)._1*estado._1,estado._2+probabilidad(apuesta)._2*apuesta.monto-apuesta.monto)}")
      Tuple2(probabilidad(apuesta)._1*estado._1,estado._2+probabilidad(apuesta)._2*apuesta.monto-apuesta.monto)
    }else{
      Tuple2(probabilidad(apuesta)._1*estado._1,estado._2)
    }
  }
  def tupla_resultado_negativo(apuesta:Apuesta, estado:(Double,Double)):(Double,Double) ={
    if(estado._2 >= apuesta.monto){
      //      println(s"para resultado negativo ${apuesta.tipo}:${apuesta.monto} estado:${estado} => estado=>${(probabilidad(apuesta)._1*estado._1,estado._2-apuesta.monto)}")
      Tuple2((1-probabilidad(apuesta)._1)*estado._1,estado._2-apuesta.monto)
    }else{
      Tuple2((1-probabilidad(apuesta)._1)*estado._1,estado._2)
    }
  }
  def generar_arbol_de_apuestas(apuestas: Apuestas,dato:(Double,Double)): ArbolApuestas[(Double,Double)] =
    if(apuestas.isEmpty) HojaApuesta(dato)
    else {
      if(apuestas.head.monto > dato._2){
        HojaApuesta(dato)
      } else {
        RamaApuestas(generar_arbol_de_apuestas(apuestas.tail,tupla_resultado_positivo(apuestas.head,dato)), generar_arbol_de_apuestas(apuestas.tail,tupla_resultado_negativo(apuestas.head,dato)))
      }
    }
}