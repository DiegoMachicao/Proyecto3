object Validaciones {

  // Verifica que todos los equipos cumplan la modalidad del torneo.
  def validarEquipos(modalidad: ModalidadTorneo, equipos: List[Equipo]): Resultado[List[Equipo]] = {
    val invalidos = equipos.filter(equipo => !modalidad.admiteEquipo(equipo))
    if invalidos.isEmpty then Exito(equipos)
    else {
      val nombres = invalidos.map(equipo => equipo.nombreEquipo).reduce((a, b) => s"$a, $b")
      Fallo(s"En modalidad ${modalidad.descripcion()} ${modalidad.requisito()}. No cumplen: $nombres")
    }
  }

  def validarTorneo(torneo: Torneo): Resultado[Torneo] =
    validarEquipos(torneo.modalidad, torneo.equipos).mapear(_ => torneo)
}
