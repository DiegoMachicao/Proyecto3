abstract class Torneo(
  val idTorneo: Int,
  val nombreTorneo: String,
  val videojuego: Videojuego,
  val modalidad: ModalidadTorneo,
  val equipos: List[Equipo],
  val estado: EstadoTorneo
) {
  def descripción(): String
  def actualizarEstadoTorneo(estadoNuevo: EstadoTorneo): Torneo
  def conEquipos(nuevosEquipos: List[Equipo]): Torneo   // NUEVO
}
