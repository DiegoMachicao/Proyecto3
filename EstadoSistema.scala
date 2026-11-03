// Todo lo que el sistema "sabe" en un momento dado. Es inmutable:
// cada operación devuelve un EstadoSistema nuevo.
case class EstadoSistema(
  val videojuegos: List[Videojuego],
  val equipos: List[Equipo],
  val torneos: List[Torneo],
  val usuarios: List[Usuario] = Nil   // organizadores y participantes registrados
)

// Datos listos para mostrar en una tabla o lista de la interfaz.
case class ResumenTorneo(
  val id: Int,
  val nombre: String,
  val tipo: String,
  val videojuego: String,
  val modalidad: String,
  val estado: String,
  val cantidadEquipos: Int
)
