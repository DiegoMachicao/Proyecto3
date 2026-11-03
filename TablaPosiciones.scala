case class TablaPosiciones(
    val torneo: Torneo,
    val equipos: List[ResultadoEquipo],
    val fechaFinalizacion: String
) {
    def definirPodio(acc: Int = 0, podio: List[ResultadoEquipo] = Nil, listaEquipos: List[ResultadoEquipo] = equipos): List[ResultadoEquipo] = {
        if acc > 2 then podio.reverse
        else {
            val mayor: ResultadoEquipo = listaEquipos.reduce((a, b) => if a.puntosTotales > b.puntosTotales then a else b)
            definirPodio(acc + 1, mayor :: podio, listaEquipos.filter(x => !x.equals(mayor)))
        }
    }

    def imprimirPodio(podio: List[ResultadoEquipo] = definirPodio()): String = {
        podio match
            case Nil => ""
            case cabeza :: cola => s"${cabeza.descripcionResultado()}\n${imprimirPodio(cola)}"        
    }
}