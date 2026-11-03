// Cómo le fue a un equipo en una ronda de un torneo por sistema de puntos.
// posicion: 1 = primer lugar. bajas: eliminaciones logradas en la ronda.
case class DesempenoEquipo(
  val equipo: Equipo,
  val posicion: Int,
  val bajas: Int
)
