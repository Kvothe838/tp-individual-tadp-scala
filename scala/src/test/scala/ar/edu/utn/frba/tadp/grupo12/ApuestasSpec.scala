package ar.edu.utn.frba.tadp.grupo12

import ar.edu.utn.frba.tadp.grupo12.Types.Criterio
import org.scalatest.freespec.AnyFreeSpec

class ApuestasSpec extends AnyFreeSpec{
  "Apostar" - {
    "Jugador apuesta Cara y juega" in {
      val bob_esponja = Jugador("Bob Esponja", Perfil.TipoCauto, 100)
      val apuestas = List[Apuesta](new Apuesta(Tipo(Cara, DistribucionEquiprobable),50),
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),150))
      val resultado =Casino.jugar(apuestas,bob_esponja)
      val arbol = ArbolApuestas.generar_arbol_de_apuestas(apuestas,(1,100))
      val resultados_posibles = arbol.dame_tus_hojas.map(a=>a._2)
      assert(resultados_posibles.contains(resultado.jugador.monto))
      assert(resultado.jugador.monto == 50||resultado.jugador.monto == 150||resultado.jugador.monto == 300||resultado.jugador.monto == 0)
    }

    "Probabilidad de ganar en una coleccion" in {
      val apuestas = List[Apuesta](new Apuesta(Tipo(Cara,DistribucionEquiprobable),50),new Apuesta(Tipo(Cruz,DistribucionEquiprobable),50))
      val proba = Casino.prob_ganar(apuestas)
      assert(proba == 0.25)
    }

    "Combinacion" in {
      val apuestas = List[Apuesta](new Apuesta(Tipo(Cara,DistribucionEquiprobable),50),new Apuesta(Tipo(Cruz,DistribucionEquiprobable),150))
      val comb = Casino.combinar(apuestas)
      val combinatoria = Casino.combinar(apuestas)
      val esperado =List(
        List((Cara,50.0)),
        List((Cruz,150.0)),
        List((Cara,50.0), (Cruz,150.0)),
        List((Cruz,150.0), (Cara,50.0)))
      /*for {
        apuestas <- comb
      }yield println(apuestas.map(a => (a.tipo.tipoApuesta,a.monto)))
      println(s"combinatoria.length:${combinatoria.length} deberia generar un arbol binario de ${scala.math.pow(2,combinatoria.flatten.length)} hojas")*/
      val arbol = ArbolApuestas.generar_arbol_de_apuestas(apuestas,(1,100))
      /*println(s"Un arbol de altura ${arbol.altura} con ${arbol.contar_hojas}")
      println("hojas")
      for {
        hoja <- arbol.dame_tus_hojas
      }yield println(s"hoja:$hoja")
      println(s"probabilidad de no perder: ${arbol.dame_tus_hojas.filter(h=>h._2 >= 50.0).map(h=>h._1).sum}")*/
      assert(comb.length == esperado.length)
    }

    "Juegos Sucesivos" in {
      val bob_marley = Jugador("Bob Marley", Perfil.TipoCauto,15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10), // Cara 10
        new Apuesta(Tipo(Numero(0),DistribucionEquiprobable),15)) // 0 15
      val chance_de_ganar_550 = Casino.prob_ganar(apuestas)
      assert(chance_de_ganar_550 == 0.5*1/37)
      val monto_final = Casino.jugar(apuestas,bob_marley).jugador.monto
      println(s"monto final: $monto_final")
      assert(monto_final == 5 || monto_final == 550 || monto_final == 10)
    }

    "Jugador Cauto" in {
      val cauto = Jugador("Juan Cauto", Perfil.TipoCauto,15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10), // 0.5, las chances de no perder son 0.5
        new Apuesta(Tipo(Numero(0),DistribucionEquiprobable),15)) // 1/37

        val planificacion = Casino.planificar(cauto,apuestas)
        //println(s" planificacion ${planificacion.map(_.tipo.tipoApuesta)} cantidad ${planificacion.length}")
        assert(planificacion.length == 1)
        assert(planificacion.head.tipo.tipoApuesta == Cara)
    }
    "Jugador Arriesgado" in {
      val arriesgado = Jugador("Juan Arriesgado", Perfil.TipoArriesgado, 15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10), // Cara 10
        new Apuesta(Tipo(Numero(0),DistribucionEquiprobable),15)) // 0 15
        val planificacion = Casino.planificar(arriesgado,apuestas)

        assert(planificacion.length == 2)
        assert(planificacion == apuestas.reverse || planificacion == apuestas)
    }

    "Jugador racional" in {
      val racional = Jugador("Juan Racional", Perfil.TipoRacional,15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10), // Cara 10
        new Apuesta(Tipo(Numero(0),DistribucionEquiprobable),15)) // 0 15

      val planificacion = Casino.planificar(racional,apuestas)
      //println(s" planificacion ${planificacion.map(_.tipo.tipoApuesta)} cantidad ${planificacion.length}")

      assert(planificacion.length == 1)
      assert(planificacion.head.tipo.tipoApuesta == Cara)
    }

    "Jugador adicto" in {
      val criterio : Criterio = (hojas, _) => hojas.length
      val adicto = Jugador("Juan Adicto", criterio, 15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10), // Cara 10
        new Apuesta(Tipo(Numero(0),DistribucionEquiprobable),15), // 0 15
        new Apuesta(Tipo(Par,DistribucionEquiprobable),15), // 0 15
        new Apuesta(Tipo(Numero(2),DistribucionEquiprobable),16)) // 0 15

      val planificacion = Casino.planificar(adicto,apuestas)
      assert(planificacion.length == apuestas.length)
    }

    "Moneda cargada" in {
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(MonedaCargada(4, 7), DistribucionPonderada),10))

        val bob_esponja = Jugador("Bob Esponja", Perfil.TipoArriesgado, 100)
        val resultado =Casino.jugar(apuestas,bob_esponja)

        assert(resultado.jugador.monto == 110 || resultado.jugador.monto == 90)
    }

    "Jugador apuesta Piedra y juega" in {
      val bob_esponja = Jugador("Bob Esponja", Perfil.TipoCauto, 100)
      val apuestas = List[Apuesta](new Apuesta(Tipo(Piedra, DistribucionPonderada),50))
      val resultado = Casino.jugar(apuestas,bob_esponja)
      assert(resultado.jugador.monto == 50||resultado.jugador.monto == 100||resultado.jugador.monto == 200)
    }

    "Jugador apuesta Papel y juega" in {
      val bob_esponja = Jugador("Bob Esponja", Perfil.TipoCauto, 100)
      val apuestas = List[Apuesta](new Apuesta(Tipo(Papel, DistribucionPonderada),50))
      val resultado = Casino.jugar(apuestas,bob_esponja)
      assert(resultado.jugador.monto == 50||resultado.jugador.monto == 100||resultado.jugador.monto == 200)
    }

    "Jugador apuesta Tijera y juega" in {
      val bob_esponja = Jugador("Bob Esponja", Perfil.TipoCauto, 100)
      val apuestas = List[Apuesta](new Apuesta(Tipo(Tijera, DistribucionPonderada),50))
      val resultado = Casino.jugar(apuestas,bob_esponja)
      assert(resultado.jugador.monto == 50||resultado.jugador.monto == 100||resultado.jugador.monto == 200)
    }

    "Jugador adicto piedra, papel, tijera" in {
      val criterio : Criterio = (hojas, _) => hojas.length
      val adicto = Jugador("Juan Adicto", criterio, 15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Piedra,DistribucionPonderada),10), // Cara 10
        new Apuesta(Tipo(Papel,DistribucionPonderada),15), // 0 15
        new Apuesta(Tipo(Tijera,DistribucionPonderada),15)) // 0 15

      val planificacion = Casino.planificar(adicto,apuestas)
      assert(planificacion.length == apuestas.length)
    }

    "Jugador Cauto apuesta cara y piedra" in {
      val cauto = Jugador("Juan Cauto", Perfil.TipoCauto,15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10),
        new Apuesta(Tipo(Piedra,DistribucionPonderada),15))

      val planificacion = Casino.planificar(cauto,apuestas)
      assert(planificacion.length == 1)
      assert(planificacion.head.tipo.tipoApuesta == Cara)
    }

    "Jugador Arriesgado apuesta cara y papel" in {
      val arriesgado = Jugador("Juan Arriesgado", Perfil.TipoArriesgado, 15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cara,DistribucionEquiprobable),10), // Cara 10
        new Apuesta(Tipo(Papel,DistribucionPonderada),15)) // 0 15
      val planificacion = Casino.planificar(arriesgado,apuestas)

      assert(planificacion.length == 2)
      assert(planificacion == apuestas.reverse || planificacion == apuestas)
    }

    "Jugador racional apuesta Cruz y Tijera" in {
      val racional = Jugador("Juan Racional", Perfil.TipoRacional,15)
      val apuestas = List[Apuesta](
        new Apuesta(Tipo(Cruz,DistribucionEquiprobable),10), // Cara 10
        new Apuesta(Tipo(Tijera,DistribucionPonderada),15)) // 0 15

      val planificacion = Casino.planificar(racional,apuestas)

      assert(planificacion.length == 1)
      assert(planificacion.head.tipo.tipoApuesta == Cruz)
    }

  }
}
