case class ResultadoEquipo(
    val equipo: Equipo,
    val puntosTotales: Int
) {
    def descripcionResultado(): String = {
        s"Equipo: ${equipo.nombreEquipo}  Puntaje: $puntosTotales"
    }
}