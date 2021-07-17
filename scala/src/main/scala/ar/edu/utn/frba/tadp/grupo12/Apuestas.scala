package ar.edu.utn.frba.tadp.grupo12

class Apuesta(val tipo: Tipo, val monto: Double)
object Apuesta {
  implicit def ordering[A <: Apuesta]: Ordering[A] = Ordering.by(_.toString)
}
trait TipoApuesta
case class MonedaCargada(loquequiero: Int, total: Int) extends TipoApuesta
case object Cara extends TipoApuesta
case object Cruz extends TipoApuesta
case object PrimerDocena extends TipoApuesta
case object SegundaDocena extends TipoApuesta
case object TercerDocena extends TipoApuesta
case object Rojo extends TipoApuesta
case object Negro extends TipoApuesta
case object Par extends TipoApuesta
case object Impar extends TipoApuesta
case class Numero(valor: Int) extends TipoApuesta
case class Tipo(tipoApuesta: TipoApuesta,tipoDistribucion:TipoDistribucion)
abstract class TipoDistribucion

case object DistribucionEquiprobable extends TipoDistribucion {
  val probabilidad: List[TipoApuesta] => Double = lista =>{
    1.0/lista.length
  }
}

case object DistribucionPonderada extends TipoDistribucion {
  def probabilidad(loquequiero: Int, total: Int):Double ={
    loquequiero.toDouble/total.toDouble
  }
}

