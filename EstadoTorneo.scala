sealed trait EstadoTorneo{
    def descripcion(): String
}

case object TorneoPendiente extends EstadoTorneo{
    def descripcion(): String = "Pendiente"
}

case object TorneoEnCurso extends EstadoTorneo{
    def descripcion(): String = "En curso"
}

case object TorneoFinalizado extends EstadoTorneo{
    def descripcion(): String = "El torneo ya finalizó"
}