case class TorneoDobleEliminacion(
  val cuadroGanadores: TorneoEliminacionDirecta,
  val cuadroPerdedores: TorneoEliminacionDirecta,
  override val idTorneo: Int,
  override val nombreTorneo: String,
  override val videojuego: Videojuego,
  override val modalidad: ModalidadTorneo,
  override val equipos: List[Equipo],
  override val estado: EstadoTorneo
) extends Torneo(idTorneo, nombreTorneo, videojuego, modalidad, equipos, estado) {
  def descripción(): String = s"Torneo de eliminación doble\nID: $idTorneo\nNombre: $nombreTorneo\nJuego: ${videojuego.titulo}\nModalidad: ${modalidad.descripcion()}\nEstado: ${estado.descripcion()}"
  def actualizarEstadoTorneo(estadoNuevo: EstadoTorneo): Torneo = copy(estado = estadoNuevo)
  def conEquipos(nuevosEquipos: List[Equipo]): Torneo = copy(equipos = nuevosEquipos)
}
