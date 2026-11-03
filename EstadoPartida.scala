sealed trait EstadoPartida{
    def descripcion(): String
}

case object Pendiente extends EstadoPartida{
    def descripcion(): String = "Pendiente"
}

case object EnCurso extends EstadoPartida{
    def descripcion(): String = "En curso"
}

case object FinalizadoGanoEquipo1 extends EstadoPartida{
    def descripcion(): String = "Ganó el equipo 1"
}

case object FinalizadoGanoEquipo2 extends EstadoPartida{
    def descripcion(): String = "Ganó el equipo 2"
}

case object FinalizadoEmpate extends EstadoPartida{
    def descripcion(): String = "La partida terminó en empate"
}