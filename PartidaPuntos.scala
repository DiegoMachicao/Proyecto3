case class PartidaPuntos(
  val puntaje1: Int,
  val puntaje2: Int,
  override val idPartida: Int,
  override val equipo1: Equipo,
  override val equipo2: Equipo,
  override val estado: EstadoPartida
) extends Partida(idPartida, equipo1, equipo2, estado) {

  def resumen(): String = {
    estado match
      case FinalizadoGanoEquipo1 => s"${estado.descripcion()}: ${equipo1.nombreEquipo} con $puntaje1 puntos"
      case FinalizadoGanoEquipo2 => s"${estado.descripcion()}: ${equipo2.nombreEquipo} con $puntaje2 puntos"
      case FinalizadoEmpate => s"${estado.descripcion()}"
      case _ => "La partida aún no ha finalizado"
  }

  // Estado que corresponde a un par de puntajes.
  private def estadoSegunPuntajes(p1: Int, p2: Int): EstadoPartida =
    if p1 > p2 then FinalizadoGanoEquipo1
    else if p2 > p1 then FinalizadoGanoEquipo2
    else FinalizadoEmpate

  // NUEVO: registra los puntajes y deja la partida finalizada con su ganador.
  def actualizarPuntaje(nuevoPuntaje1: Int, nuevoPuntaje2: Int): Resultado[PartidaPuntos] =
    if nuevoPuntaje1 < 0 || nuevoPuntaje2 < 0 then Fallo("Los puntajes no pueden ser negativos")
    else Exito(copy(
      puntaje1 = nuevoPuntaje1,
      puntaje2 = nuevoPuntaje2,
      estado = estadoSegunPuntajes(nuevoPuntaje1, nuevoPuntaje2)
    ))

  // CORREGIDO: ahora sí usa el estado recibido.
  def actualizarEstadoPartida(estadoNuevo: EstadoPartida): Partida = copy(estado = estadoNuevo)
}
