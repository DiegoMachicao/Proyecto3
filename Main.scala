object main {

  // Convierte un Resultado en texto (lo que más adelante hará la interfaz).
  def mostrar[A](resultado: Resultado[A], paraExito: A => String): String = resultado match
    case Exito(valor)  => paraExito(valor)
    case Fallo(motivo) => s"ERROR: $motivo"

  def main(args: Array[String]): Unit = {
    val cod = Videojuego(1, "Call Of Duty", "FPS")

    val equipo1 = Equipo(1, "Triple t", List(Participante(1, "Tokkf", 1, "Diego", "a@gmail.com")))
    val equipo2 = Equipo(2, "Enanos Verdes", List(Participante(2, "La Trucha", 2, "Matias", "b@gmail.com")))
    val equipo3 = Equipo(3, "Gatitos feudales", List(Participante(3, "Leon", 3, "Paolo", "c@gmail.com")))
    val equipo4 = Equipo(4, "Los Cuatro", List(Participante(4, "Marco", 4, "Marco", "d@gmail.com")))
    val equipoGrande = Equipo(5, "Dúo", List(
      Participante(5, "Ana", 5, "Ana", "e@gmail.com"),
      Participante(6, "Beto", 6, "Beto", "f@gmail.com")))

    val torneo = TorneoSistemaDePuntos(10, 2, 1, "Copa Warzone", cod, Individual, Nil, TorneoPendiente)

    val inicial: Resultado[EstadoSistema] = Exito(Sistema.estadoInicial)
    val equipos = List(equipo1, equipo2, equipo3, equipo4, equipoGrande)

    // Registro de equipos con foldLeft: se detiene en el primer error.
    val conEquipos = equipos.foldLeft(inicial)((acc, equipo) =>
      acc.encadenar(e => Sistema.registrarEquipo(e, equipo)))

    val preparado = conEquipos
      .encadenar(e => Sistema.registrarVideojuego(e, cod))
      .encadenar(e => Sistema.agregarTorneo(e, torneo))

    // Inscripción de los 4 equipos individuales
    val inscrito = List(1, 2, 3, 4).foldLeft(preparado)((acc, idEquipo) =>
      acc.encadenar(e => Sistema.inscribirEquipo(e, 1, idEquipo)))

    println("--- Inscribir equipo de 2 integrantes en torneo Individual (debe fallar) ---")
    println(mostrar(inscrito.encadenar(e => Sistema.inscribirEquipo(e, 1, 5)), _ => "inscrito"))

    println("--- Iniciar el torneo ---")
    val enCurso = inscrito.encadenar(e => Sistema.cambiarEstadoTorneo(e, 1, TorneoEnCurso))
    println(mostrar(enCurso, e => Sistema.resumenes(e).toString))

    println("--- Iniciar de nuevo (transición inválida) ---")
    println(mostrar(enCurso.encadenar(e => Sistema.cambiarEstadoTorneo(e, 1, TorneoEnCurso)), _ => "ok"))

    val ronda1 = List(
      DesempenoEquipo(equipo1, 2, 5), DesempenoEquipo(equipo2, 1, 3),
      DesempenoEquipo(equipo3, 4, 0), DesempenoEquipo(equipo4, 3, 1))
    val ronda2 = List(
      DesempenoEquipo(equipo1, 1, 4), DesempenoEquipo(equipo2, 2, 2),
      DesempenoEquipo(equipo3, 3, 3), DesempenoEquipo(equipo4, 4, 0))

    println("--- Tabla por sistema de puntos (2 rondas) ---")
    println(mostrar(
      enCurso.encadenar(e => Sistema.calcularTablaPuntos(e, 1, List(ronda1, ronda2), "31/10/2026")),
      tabla => tabla.imprimirPodio()))

    println("--- Ronda inválida (posición repetida) ---")
    val rondaMala = List(
      DesempenoEquipo(equipo1, 1, 0), DesempenoEquipo(equipo2, 1, 0),
      DesempenoEquipo(equipo3, 3, 0), DesempenoEquipo(equipo4, 4, 0))
    println(mostrar(
      enCurso.encadenar(e => Sistema.calcularTablaPuntos(e, 1, List(rondaMala), "hoy")),
      tabla => tabla.imprimirPodio()))

    println("--- Actualizar puntaje de una PartidaPuntos ---")
    val partida = PartidaPuntos(0, 0, 1, equipo1, equipo2, EnCurso)
    println(mostrar(partida.actualizarPuntaje(15, 12), p => p.resumen()))
    println(mostrar(partida.actualizarPuntaje(10, 10), p => p.resumen()))
    println(mostrar(partida.actualizarPuntaje(-1, 3), p => p.resumen()))
  }
}
