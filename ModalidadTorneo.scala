sealed trait ModalidadTorneo{
  def descripcion(): String
  def requisito(): String
  def admiteEquipo(equipo: Equipo): Boolean
}

case object Individual extends ModalidadTorneo{
  def descripcion(): String = "Individual"
  def requisito(): String = "cada equipo debe tener exactamente 1 integrante"
  def admiteEquipo(equipo: Equipo): Boolean = equipo.integrantes.length == 1
}

case object PorEquipos extends ModalidadTorneo{
  def descripcion(): String = "Por equipos"
  def requisito(): String = "cada equipo debe tener al menos 2 integrantes"
  def admiteEquipo(equipo: Equipo): Boolean = equipo.integrantes.length >= 2
}
