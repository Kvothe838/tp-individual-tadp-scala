package ar.edu.utn.frba.tadp.grupo12

import ar.edu.utn.frba.tadp.grupo12.Types.Criterio
import ar.edu.utn.frba.tadp.grupo12.Types.Probabilidad_Monto

case class Jugador(nombre:String, perfil: Criterio, monto:Double){
  def puedePagar(apuesta: Apuesta): Boolean = {
    monto >= apuesta.monto
  }
  def gana(cantidad: Double): Jugador ={
    copy(monto = monto+cantidad)
  }
  def pagar(apuesta: Apuesta): Jugador = {
    copy(monto = (monto - apuesta.monto).max(0))
  }
}

case object Types {
  type Criterio = (List[(Double,Double)], Double) => Double
  type Probabilidad_Monto = (Double,Double)
}

case object Perfil{
  def TipoCauto: Criterio = (hojas:List[Probabilidad_Monto], monto: Double)=>hojas.filter(hoja => hoja._2 >= monto).map(hoja=>hoja._1).sum
  def TipoRacional: Criterio = (hojas:List[Probabilidad_Monto], _)=>hojas.map(hoja=>hoja._1 * hoja._2).sum
  def TipoArriesgado: Criterio = (hojas:List[Probabilidad_Monto], _)=>hojas.map(hoja=>hoja._2).max
}

