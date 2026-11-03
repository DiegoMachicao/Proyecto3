// Resultado de una operación que puede salir bien (Exito) o mal (Fallo).
// Es un tipo genérico: sirve para devolver cualquier tipo A.
sealed trait Resultado[A] {

  def esExito: Boolean = this match
    case Exito(_)  => true
    case Fallo(_)  => false

  // Transforma el valor si la operación salió bien; si falló, conserva el error.
  def mapear[B](f: A => B): Resultado[B] = this match
    case Exito(valor)  => Exito(f(valor))
    case Fallo(motivo) => Fallo(motivo)

  // Encadena otra operación que también puede fallar.
  def encadenar[B](f: A => Resultado[B]): Resultado[B] = this match
    case Exito(valor)  => f(valor)
    case Fallo(motivo) => Fallo(motivo)
}

case class Exito[A](valor: A) extends Resultado[A]
case class Fallo[A](motivo: String) extends Resultado[A]
