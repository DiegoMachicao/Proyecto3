abstract class Partida(
    val idPartida: Int,
    val equipo1: Equipo,
    val equipo2: Equipo,
    val estado: EstadoPartida
) {
    def resumen(): String

    def actualizarEstadoPartida(estadoNuevo: EstadoPartida): Partida

    def ganador: Option[Equipo] = 
        estado match
            case FinalizadoGanoEquipo1 => Some(equipo1)
            case FinalizadoGanoEquipo2 => Some(equipo2)
            case _ => None

    def perdedor: Option[Equipo] = 
        estado match
            case FinalizadoGanoEquipo1 => Some(equipo2)
            case FinalizadoGanoEquipo2 => Some(equipo1)
            case _ => None

    def finalizada: Boolean = 
        estado match
            case FinalizadoGanoEquipo1 | FinalizadoGanoEquipo2 | FinalizadoEmpate => true
            case _ => false
}