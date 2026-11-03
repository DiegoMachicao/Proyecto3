// Fachada del sistema: la interfaz (consola o gráfica) solo debería llamar a estas funciones.
// Todas son puras: reciben un EstadoSistema y devuelven un Resultado o una lista nueva.
object Sistema {

  val estadoInicial: EstadoSistema = EstadoSistema(Nil, Nil, Nil)

  // ---------- Utilidades genéricas ----------

  def siguienteId[A](elementos: List[A], obtenerId: A => Int): Int =
    elementos.foldLeft(0)((maximo, e) => if obtenerId(e) > maximo then obtenerId(e) else maximo) + 1

  private def agregarSiNoExiste[A](lista: List[A], nuevo: A, obtenerId: A => Int, entidad: String): Resultado[List[A]] =
    if lista.filter(e => obtenerId(e) == obtenerId(nuevo)).isEmpty then Exito(lista :+ nuevo)
    else Fallo(s"Ya existe un registro de $entidad con id ${obtenerId(nuevo)}")

  // ---------- Registro ----------

  def registrarVideojuego(estado: EstadoSistema, videojuego: Videojuego): Resultado[EstadoSistema] =
    agregarSiNoExiste(estado.videojuegos, videojuego, (v: Videojuego) => v.idJuego, "videojuego")
      .mapear(lista => estado.copy(videojuegos = lista))

  def registrarEquipo(estado: EstadoSistema, equipo: Equipo): Resultado[EstadoSistema] =
    agregarSiNoExiste(estado.equipos, equipo, (e: Equipo) => e.idEquipo, "equipo")
      .mapear(lista => estado.copy(equipos = lista))

  // Operación interna (sin control de rol). La interfaz usa `crearTorneo`, que exige un organizador.
  // Valida la modalidad, que el videojuego esté registrado y que el id no se repita.
  def agregarTorneo(estado: EstadoSistema, torneo: Torneo): Resultado[EstadoSistema] =
    if estado.videojuegos.filter(v => v.idJuego == torneo.videojuego.idJuego).isEmpty then
      Fallo(s"El videojuego ${torneo.videojuego.titulo} no está registrado")
    else
      Validaciones.validarTorneo(torneo)
        .encadenar(valido => agregarSiNoExiste(estado.torneos, valido, (t: Torneo) => t.idTorneo, "torneo"))
        .mapear(lista => estado.copy(torneos = lista))

  // ---------- Búsquedas ----------

  def buscarTorneo(estado: EstadoSistema, idTorneo: Int): Resultado[Torneo] =
    estado.torneos.filter(t => t.idTorneo == idTorneo) match
      case torneo :: _ => Exito(torneo)
      case Nil         => Fallo(s"No existe el torneo con id $idTorneo")

  def buscarEquipo(estado: EstadoSistema, idEquipo: Int): Resultado[Equipo] =
    estado.equipos.filter(e => e.idEquipo == idEquipo) match
      case equipo :: _ => Exito(equipo)
      case Nil         => Fallo(s"No existe el equipo con id $idEquipo")

  private def reemplazarTorneo(estado: EstadoSistema, nuevo: Torneo): EstadoSistema =
    estado.copy(torneos = estado.torneos.map(t => if t.idTorneo == nuevo.idTorneo then nuevo else t))

  // ---------- Operaciones sobre torneos ----------

  // Integrantes del equipo que ya participan en el torneo con otro equipo.
  private def integrantesRepetidos(torneo: Torneo, equipo: Equipo): List[Participante] =
    equipo.integrantes.filter(p =>
      torneo.equipos.flatMap(e => e.integrantes).filter(q => q.idParticipante == p.idParticipante).nonEmpty)

  private def validarInscripcion(torneo: Torneo, equipo: Equipo): Resultado[Torneo] =
    torneo.estado match
      case TorneoPendiente =>
        if torneo.equipos.filter(e => e.idEquipo == equipo.idEquipo).nonEmpty then
          Fallo[Torneo](s"El equipo ${equipo.nombreEquipo} ya está inscrito en el torneo")
        else integrantesRepetidos(torneo, equipo) match
          case Nil => Validaciones.validarEquipos(torneo.modalidad, List(equipo)).mapear(_ => torneo)
          case repetidos =>
            val nombres = repetidos.map(p => p.nickname).mkString(", ")
            Fallo[Torneo](s"Ya participan en el torneo: $nombres")
      case _ => Fallo[Torneo]("Solo se pueden inscribir equipos en torneos pendientes")

  def inscribirEquipo(estado: EstadoSistema, idTorneo: Int, idEquipo: Int): Resultado[EstadoSistema] =
    buscarEquipo(estado, idEquipo).encadenar(equipo =>
      buscarTorneo(estado, idTorneo)
        .encadenar(torneo => validarInscripcion(torneo, equipo))
        .mapear(torneo => reemplazarTorneo(estado, torneo.conEquipos(torneo.equipos :+ equipo))))

  private def transicionValida(actual: EstadoTorneo, nuevo: EstadoTorneo): Boolean = (actual, nuevo) match
    case (TorneoPendiente, TorneoEnCurso)  => true
    case (TorneoEnCurso, TorneoFinalizado) => true
    case _                     => false

  def cambiarEstadoTorneo(estado: EstadoSistema, idTorneo: Int, nuevoEstado: EstadoTorneo): Resultado[EstadoSistema] =
    buscarTorneo(estado, idTorneo).encadenar(torneo =>
      if !transicionValida(torneo.estado, nuevoEstado) then
        Fallo(s"No se puede pasar de '${torneo.estado.descripcion()}' a '${nuevoEstado.descripcion()}'")
      else if nuevoEstado == TorneoEnCurso && torneo.equipos.length < 2 then
        Fallo("Se necesitan al menos 2 equipos para iniciar el torneo")
      else
        Exito(reemplazarTorneo(estado, torneo.actualizarEstadoTorneo(nuevoEstado))))

  // Solo para torneos por sistema de puntos.
  private def tablaDe(torneo: Torneo, rondas: List[List[DesempenoEquipo]], fecha: String): Resultado[TablaPosiciones] =
    torneo match
      case t: TorneoSistemaDePuntos =>
        t.calcularResultados(rondas).mapear(resultados => TablaPosiciones(t, resultados, fecha))
      case _ => Fallo("Este torneo no usa sistema de puntos")

  def calcularTablaPuntos(estado: EstadoSistema, idTorneo: Int, rondas: List[List[DesempenoEquipo]], fecha: String): Resultado[TablaPosiciones] =
    buscarTorneo(estado, idTorneo).encadenar(torneo => tablaDe(torneo, rondas, fecha))

  // ---------- Usuarios ----------

  def registrarUsuario(estado: EstadoSistema, usuario: Usuario): Resultado[EstadoSistema] =
    if estado.usuarios.filter(u => u.correo.toLowerCase == usuario.correo.toLowerCase).nonEmpty then
      Fallo[EstadoSistema](s"Ya existe un usuario con el correo ${usuario.correo}")
    else
      agregarSiNoExiste(estado.usuarios, usuario, (u: Usuario) => u.id, "usuario")
        .mapear(lista => estado.copy(usuarios = lista))

  def registrarOrganizador(estado: EstadoSistema, nombre: String, correo: String): Resultado[EstadoSistema] =
    registrarUsuario(estado, Organizador(Nil, siguienteId(estado.usuarios, (u: Usuario) => u.id), nombre, correo))

  // El id de usuario y el de participante son el mismo número.
  def registrarParticipante(estado: EstadoSistema, nickname: String, nombre: String, correo: String): Resultado[EstadoSistema] =
    val id = siguienteId(estado.usuarios, (u: Usuario) => u.id)
    registrarUsuario(estado, Participante(id, nickname, id, nombre, correo))

  def buscarUsuario(estado: EstadoSistema, idUsuario: Int): Resultado[Usuario] =
    estado.usuarios.filter(u => u.id == idUsuario) match
      case usuario :: _ => Exito(usuario)
      case Nil => Fallo(s"No existe el usuario con id $idUsuario")

  // Inicio de sesión: se identifica al usuario por su correo.
  def iniciarSesion(estado: EstadoSistema, correo: String): Resultado[Usuario] =
    estado.usuarios.filter(u => u.correo.toLowerCase == correo.trim.toLowerCase) match
      case usuario :: _ => Exito(usuario)
      case Nil => Fallo(s"No hay ningún usuario registrado con el correo ${correo.trim}")

  def buscarParticipante(estado: EstadoSistema, idParticipante: Int): Resultado[Participante] =
    estado.usuarios.collect { case p: Participante => p }.filter(p => p.idParticipante == idParticipante) match
      case participante :: _ => Exito(participante)
      case Nil => Fallo(s"No existe el participante con id $idParticipante")

  def participantes(estado: EstadoSistema): List[Participante] =
    estado.usuarios.collect { case p: Participante => p }

  // ---------- Permisos por rol ----------
  // Cada operación recibe el id del usuario que la pide (no el objeto), así siempre se
  // usa su versión más reciente del estado. Si el rol no corresponde, devuelve Fallo.

  private def exigirOrganizador(usuario: Usuario, accion: String): Resultado[Organizador] = usuario match
    case o: Organizador => Exito[Organizador](o)
    case _ => Fallo[Organizador](s"Solo un organizador puede $accion")

  private def exigirParticipante(usuario: Usuario, accion: String): Resultado[Participante] = usuario match
    case p: Participante => Exito[Participante](p)
    case _ => Fallo[Participante](s"Solo un participante puede $accion")

  // Función de orden superior: la acción permitida la decide quien llama.
  def comoOrganizador(estado: EstadoSistema, idUsuario: Int, accion: String)
                     (operacion: Organizador => Resultado[EstadoSistema]): Resultado[EstadoSistema] =
    buscarUsuario(estado, idUsuario)
      .encadenar(usuario => exigirOrganizador(usuario, accion))
      .encadenar(organizador => operacion(organizador))

  def comoParticipante(estado: EstadoSistema, idUsuario: Int, accion: String)
                      (operacion: Participante => Resultado[EstadoSistema]): Resultado[EstadoSistema] =
    buscarUsuario(estado, idUsuario)
      .encadenar(usuario => exigirParticipante(usuario, accion))
      .encadenar(participante => operacion(participante))

  // ----- Solo organizadores -----

  def registrarVideojuegoComo(estado: EstadoSistema, idUsuario: Int, videojuego: Videojuego): Resultado[EstadoSistema] =
    comoOrganizador(estado, idUsuario, "registrar videojuegos")(o => registrarVideojuego(estado, videojuego))

  def crearTorneo(estado: EstadoSistema, idUsuario: Int, torneo: Torneo): Resultado[EstadoSistema] =
    comoOrganizador(estado, idUsuario, "crear torneos")(o => o.crearTorneo(estado, torneo))

  def iniciarTorneo(estado: EstadoSistema, idUsuario: Int, idTorneo: Int): Resultado[EstadoSistema] =
    comoOrganizador(estado, idUsuario, "iniciar torneos")(o => o.iniciarTorneo(estado, idTorneo))

  def finalizarTorneo(estado: EstadoSistema, idUsuario: Int, idTorneo: Int): Resultado[EstadoSistema] =
    comoOrganizador(estado, idUsuario, "finalizar torneos")(o => o.finalizarTorneo(estado, idTorneo))

  // ----- Solo participantes -----

  def unirseSolo(estado: EstadoSistema, idUsuario: Int, idTorneo: Int): Resultado[EstadoSistema] =
    comoParticipante(estado, idUsuario, "unirse a torneos")(p => p.unirseSolo(estado, idTorneo))

  def unirseConEquipo(estado: EstadoSistema, idUsuario: Int, idTorneo: Int, idEquipo: Int): Resultado[EstadoSistema] =
    comoParticipante(estado, idUsuario, "unirse a torneos")(p => p.unirseConEquipo(estado, idTorneo, idEquipo))

  def unirseConEquipoNuevo(estado: EstadoSistema, idUsuario: Int, idTorneo: Int,
                           nombreEquipo: String, idsCompaneros: List[Int]): Resultado[EstadoSistema] =
    comoParticipante(estado, idUsuario, "unirse a torneos")(p =>
      p.unirseConEquipoNuevo(estado, idTorneo, nombreEquipo, idsCompaneros))

  // ---------- Consultas para la interfaz ----------

  // Función de orden superior: el criterio lo decide quien llama.
  def filtrarTorneos(estado: EstadoSistema, criterio: Torneo => Boolean): List[Torneo] =
    estado.torneos.filter(criterio)

  def torneosActivos(estado: EstadoSistema): List[Torneo] =
    filtrarTorneos(estado, t => t.estado != TorneoFinalizado)

  def torneosPorVideojuego(estado: EstadoSistema, idJuego: Int): List[Torneo] =
    filtrarTorneos(estado, t => t.videojuego.idJuego == idJuego)

  def tipoDeTorneo(torneo: Torneo): String = torneo match
    case _: TorneoEliminacionDirecta => "Eliminación directa"
    case _: TorneoDobleEliminacion   => "Doble eliminación"
    case _: TorneoSistemaDePuntos    => "Sistema de puntos"
    case _                           => "Otro"

  def resumir(torneo: Torneo): ResumenTorneo =
    ResumenTorneo(
      torneo.idTorneo, torneo.nombreTorneo, tipoDeTorneo(torneo),
      torneo.videojuego.titulo, torneo.modalidad.descripcion(),
      torneo.estado.descripcion(), torneo.equipos.length)

  def resumenes(estado: EstadoSistema): List[ResumenTorneo] =
    estado.torneos.map(torneo => resumir(torneo))

  def torneosDeOrganizador(estado: EstadoSistema, idUsuario: Int): List[Torneo] =
    buscarUsuario(estado, idUsuario) match
      case Exito(o: Organizador) => o.misTorneos(estado)
      case _ => Nil

  def esDuenoDeTorneo(estado: EstadoSistema, idUsuario: Int, idTorneo: Int): Boolean =
    torneosDeOrganizador(estado, idUsuario).filter(t => t.idTorneo == idTorneo).nonEmpty

  def torneosDeParticipante(estado: EstadoSistema, idUsuario: Int): List[Torneo] =
    buscarUsuario(estado, idUsuario) match
      case Exito(p: Participante) => p.torneosInscritos(estado)
      case _ => Nil

  // Nombre del organizador que creó el torneo ("—" si no se registró ninguno).
  def nombreOrganizador(estado: EstadoSistema, idTorneo: Int): String =
    estado.usuarios.collect { case o: Organizador => o }.filter(o => o.esDuenoDe(idTorneo)) match
      case organizador :: _ => organizador.nombre
      case Nil => "—"
}