case class TorneoSistemaDePuntos(
  val pesoClasificacion: Int,   // antes: pesoClasificaion
  val puntajePorBaja: Int,
  override val idTorneo: Int,
  override val nombreTorneo: String,
  override val videojuego: Videojuego,
  override val modalidad: ModalidadTorneo,
  override val equipos: List[Equipo],
  override val estado: EstadoTorneo
) extends Torneo(idTorneo, nombreTorneo, videojuego, modalidad, equipos, estado) {

  def descripción(): String = s"Torneo por sistema de puntos\nID: $idTorneo\nNombre: $nombreTorneo\nJuego: ${videojuego.titulo}\nModalidad: ${modalidad.descripcion()}\nEstado: ${estado.descripcion()}"
  def actualizarEstadoTorneo(estadoNuevo: EstadoTorneo): Torneo = copy(estado = estadoNuevo)
  def conEquipos(nuevosEquipos: List[Equipo]): Torneo = copy(equipos = nuevosEquipos)

  // Puntos de un equipo en una ronda:
  //   (equipos - posicion + 1) * pesoClasificacion  +  bajas * puntajePorBaja
  def puntosPorDesempeno(desempeno: DesempenoEquipo): Int =
    (equipos.length - desempeno.posicion + 1) * pesoClasificacion + desempeno.bajas * puntajePorBaja

  // Una ronda es válida si cada equipo del torneo aparece una sola vez,
  // las posiciones van de 1 a n sin repetirse y las bajas no son negativas.
  def validarRonda(ronda: List[DesempenoEquipo]): Resultado[List[DesempenoEquipo]] = {
    val equiposNoUnicos = equipos.filter(e =>
      ronda.filter(d => d.equipo.idEquipo == e.idEquipo).length != 1)
    val posicionesInvalidas = ronda.filter(d => d.posicion < 1 || d.posicion > equipos.length)
    val posicionesRepetidas = ronda.filter(d =>
      ronda.filter(otro => otro.posicion == d.posicion).length > 1)
    val bajasInvalidas = ronda.filter(d => d.bajas < 0)

    if ronda.length != equipos.length then
      Fallo("La ronda debe incluir un desempeño por cada equipo del torneo")
    else if !equiposNoUnicos.isEmpty then
      Fallo("Cada equipo del torneo debe aparecer exactamente una vez en la ronda")
    else if !posicionesInvalidas.isEmpty then
      Fallo(s"Las posiciones deben estar entre 1 y ${equipos.length}")
    else if !posicionesRepetidas.isEmpty then
      Fallo("Dos equipos no pueden tener la misma posición en una ronda")
    else if !bajasInvalidas.isEmpty then
      Fallo("Las bajas no pueden ser negativas")
    else Exito(ronda)
  }

  // Suma los puntos de todas las rondas y devuelve un ResultadoEquipo por equipo.
  def calcularResultados(rondas: List[List[DesempenoEquipo]]): Resultado[List[ResultadoEquipo]] = {
    val desempenos = rondas.flatMap(ronda => ronda)
    val resultados = equipos.map(equipo =>
      ResultadoEquipo(
        equipo,
        desempenos
          .filter(d => d.equipo.idEquipo == equipo.idEquipo)
          .map(d => puntosPorDesempeno(d))
          .foldLeft(0)((acumulado, puntos) => acumulado + puntos)
      ))

    rondas.map(ronda => validarRonda(ronda)).filter(r => !r.esExito) match
      case Fallo(motivo) :: _ => Fallo(motivo)
      case _                  => Exito(resultados)
  }
}
