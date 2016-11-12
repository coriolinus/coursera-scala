package idealized.scala.Naturals

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true
  def predecessor: Nat = throw new NoSuchElementException()
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new NoSuchElementException()
}
class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def + (that: Nat): Nat = if (that.isZero) this else this.successor + (that.predecessor)
  def - (that: Nat): Nat = if (that.isZero) this else this.predecessor - (that.predecessor)
}
