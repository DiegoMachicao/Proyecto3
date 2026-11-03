case class PartidaEliminacion(
    override val idPartida: Int,
    override val equipo1: Equipo,
    override val equipo2: Equipo,
    override val estado: EstadoPartida
) extends Partida(idPartida, equipo1, equipo2, estado) {
    def resumen(): String = {
        estado match
            case FinalizadoGanoEquipo1 => s"${estado.descripcion()}: ${equipo1.nombreEquipo}"
            case FinalizadoGanoEquipo2 => s"${estado.descripcion()}: ${equipo2.nombreEquipo}"
            case FinalizadoEmpate => s"${estado.descripcion()}"
            case _ => "La partida aún no ha finalizado"
    }

    def actualizarEstadoPartida(estadoNuevo: EstadoPartida): Partida = copy(estado = estadoNuevo)

    // def definirGanador(equipoGanador: Equipo): Equipo 
}