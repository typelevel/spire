package spire

import  scala.collection._

object Platform {
  type TrieMap[K, V] = concurrent.TrieMap[K, V]

  @inline
  def TrieMap[K, V](): TrieMap[K, V] = new concurrent.TrieMap()
}
