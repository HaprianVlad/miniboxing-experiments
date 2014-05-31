package Algebra

import spire.algebra.AdditiveGroup
import spire.algebra.MultiplicativeGroup
import spire.math.Integral

trait Korp[A] extends Integral[A] with Iterable[A] with AdditiveGroup[A] with MultiplicativeGroup[A]
