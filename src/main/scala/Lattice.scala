package llmaam.aam

trait Lattice[T]:
  def bot: T
  def top: T
  extension (x: T)
    def ⊑(y: T): Boolean
    def ⊔(y: T): T
    def ⊓(y: T): T

given ProductLattice[A: Lattice, B: Lattice]: Lattice[(A, B)] with
  val la: Lattice[A] = summon[Lattice[A]]
  val lb: Lattice[B] = summon[Lattice[B]]
  def bot: (A, B) = (la.bot, lb.bot)
  def top: (A, B) = (la.top, lb.top)
  extension (x: (A, B))
    def ⊑(y: (A, B)): Boolean = x._1 ⊑ y._1 && x._2 ⊑ y._2
    def ⊔(y: (A, B)): (A, B) = (x._1 ⊔ y._1, x._2 ⊔ y._2)
    def ⊓(y: (A, B)): (A, B) = (x._1 ⊓ y._1, x._2 ⊓ y._2)

given MapLattice[K, V: Lattice]: Lattice[Map[K, V]] with
  val lv: Lattice[V] = summon[Lattice[V]]
  def bot: Map[K, V] = Map[K, V]()
  def top: Map[K, V] = throw new RuntimeException("No representation of top map")
  extension (m1: Map[K, V])
    def ⊑(m2: Map[K, V]): Boolean =
      m1.forall { case (k, v) => v ⊑ m2.getOrElse(k, lv.bot) }
    def ⊔(m2: Map[K, V]): Map[K, V] =
      m1.foldLeft(m2) { case (m, (k, v)) =>
        val oldValue = m.getOrElse(k, lv.bot)
        //if (oldValue != Set()) println(s"Precision loss at ${k}:\n  merging ${v}\n      and ${oldValue}")
        m + (k -> v ⊔ oldValue)
      }
    def ⊓(m2: Map[K, V]): Map[K, V] =
      m1.keySet.intersect(m2.keySet).foldLeft(Map[K,V]()) {
        case (m, k) => m + (k -> m1(k) ⊓ m2(k))
      }

given SetLattice[T]: Lattice[Set[T]] with
  def bot: Set[T] = Set[T]()
  def top: Set[T] = throw new RuntimeException("No representation of top set")
  extension (s1: Set[T])
    def ⊑(s2: Set[T]): Boolean = s1.subsetOf(s2)
    def ⊔(s2: Set[T]): Set[T] = s1 ++ s2
    def ⊓(s2: Set[T]): Set[T] = s1.intersect(s2)
