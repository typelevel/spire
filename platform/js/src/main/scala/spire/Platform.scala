package spire

import scala.collection._

object Platform {
  type TrieMap[K, V] = mutable.Map[K, V]

  @inline
  def TrieMap[K, V](): TrieMap[K, V] = mutable.Map()
}
