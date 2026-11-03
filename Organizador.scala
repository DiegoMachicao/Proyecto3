// Un organizador es el único rol que puede crear torneos y administrarlos.
// Guarda solo los ids de los torneos que creó: los torneos cambian con cada
// operación (inscripciones, estado), así que se consultan siempre en el EstadoSistema.
case class Organizador(
  val idsTorneos: List[Int],
  override val id: Int,
  override val nombre: String,
  override val correo: String
) extends Usuario(id, nombre, correo) {

  def esDuenoDe(idTorneo: Int): Boolean =
    idsTorneos.filter(i => i == idTorneo).nonEmpty

  // Devuelve un organizador nuevo que además es dueño del torneo indicado.
  def conTorneo(idTorneo: Int): Organizador =
    copy(idsTorneos = idsTorneos :+ idTorneo)

  // Torneos del estado que fueron creados por este organizador.
  def misTorneos(estado: EstadoSistema): List[Torneo] =
    estado.torneos.filter(t => esDuenoDe(t.idTorneo))

  // Crea el torneo (valida videojuego, modalidad e id repetido) y lo registra como suyo.
  def crearTorneo(estado: EstadoSistema, torneo: Torneo): Resultado[EstadoSistema] =
    Sistema.agregarTorneo(estado, torneo).mapear(nuevo =>
      nuevo.copy(usuarios = registrarComoDueno(nuevo.usuarios, torneo.idTorneo)))

  // Actualiza a este organizador dentro de la lista de usuarios (el resto queda igual).
  private def registrarComoDueno(usuarios: List[Usuario], idTorneo: Int): List[Usuario] =
    usuarios.map {
      case o: Organizador if o.id == id => o.conTorneo(idTorneo)
      case otro => otro
    }

  // Solo el organizador que creó el torneo puede cambiarle el estado.
  private def exigirDueno(idTorneo: Int): Resultado[Int] =
    if esDuenoDe(idTorneo) then Exito[Int](idTorneo)
    else Fallo[Int](s"No puedes administrar el torneo $idTorneo: lo creó otro organizador")

  def iniciarTorneo(estado: EstadoSistema, idTorneo: Int): Resultado[EstadoSistema] =
    exigirDueno(idTorneo).encadenar(_ => Sistema.cambiarEstadoTorneo(estado, idTorneo, TorneoEnCurso))

  def finalizarTorneo(estado: EstadoSistema, idTorneo: Int): Resultado[EstadoSistema] =
    exigirDueno(idTorneo).encadenar(_ => Sistema.cambiarEstadoTorneo(estado, idTorneo, TorneoFinalizado))
}
