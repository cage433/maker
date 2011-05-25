package starling.utils

import java.util.concurrent._
import math.abs


object CollectionUtils {
  /** Converts to Iterables to a Map by zipping. Elements will be dropped
   * 	if not of equal size
   */
	def toMap[A, B](i1 : Iterable[A], i2 : Iterable[B]) : Map[A, B] = {
	  toMap(i1.toList, i2.toList)
	}

  /** Converts to Lists to a Map by zipping. Elements will be dropped
   * 	if not of equal size
   */
  def toMap[A, B](list1 : List[A], list2 : List[B]) : Map[A, B] = {
	  Map.empty[A, B] ++ list1.zip(list2).map{case (a, b) => a -> b}
	}
 
  /** Constructs a Map with a key set and a function to determine the values
   */
	def toMap[A, B](keySet : Iterable[A], f : A => B) : Map[A, B] = {
	  Map.empty[A, B] ++ keySet.map{case a => a -> f(a)}
	}


  /**
   * Splits a list into a list of lists of size splitSize. The last
   * entry will be whatever was remaining.
   */
  def split[T](l : List[T], splitSize : Int) : List[List[T]] = {
    def spl(l : List[T]) : List[List[T]] = {
      if(l.size == 0) {
        Nil
      } else if(l.size < splitSize) {
        List(l)
      } else {
        l.slice(0, splitSize) :: spl(l.slice(splitSize, l.size + 1))
      }
    }
    spl(l)
  }

  /**
   *  Scala's List.intersect method is an O(m * n) algorithm, this is O(m + n)
   * but will only work for things with sensible hash codes
   */
	def intersectLists[A](l1 : List[A], l2 : List[A]) = {
	  val s1 = Set.empty[A] ++ l1
	  val s2 = Set.empty[A] ++ l2
	  (s1 & s2).toList
  }

  /** Returns the set union of an arbitrary collection of collections
   */
  def setUnion[A](l : Iterable[Iterable[A]]) : Set[A] = {
	  (Set.empty[A] /: l)(_ ++ _)
	}

  def setDifference(t1 : TraversableOnce[_], t2 : TraversableOnce[_]) : Set[_] = {
    t1.toSet.filterNot(t2.toSet.contains)
  }

  def sumDoubleSeq(seq : Seq[Double]) : Double = {
    (0.0 /: seq)(_+_)
  }

  /** Returns the average of a sequnce of doubles. Will fail if sequence is empty
   */
  def averageDoubleSeq(seq : Seq[Double]) : Double = {
    assert(seq.size > 0, "Can't calculate average of an empty sequence")
    sumDoubleSeq(seq) / seq.size
  }

  /** Returns the average of a sequnce of doubles. Returns 0 if the sequence is empty
   */
  def averageDoubleSeqOrZeroWhenEmpty(seq : Seq[Double]) : Double = {
    seq.size match{
      case 0 => 0.0
      case _ => averageDoubleSeq(seq)
    }
  }

  def sortOptionCollection(elts : Iterable[Option[String]]) : List[Option[String]] = {
    val (somes, nones) = elts.partition(_.isDefined)
    val sortedSomes = somes.map(_.get).toList.sortWith(_<_).map(Some(_))
    nones.toList ::: sortedSomes
  }

  /**
   * Maps the given function over a collection, finding the first which returns non-None. If there is
   * no such element, the method returns None.
   */
  def findDefined[A,B](f : A => Option[B], i : List[A]) : Option[B] = i match {
    case x :: xs => f(x).orElse(findDefined(f, xs))
    case Nil => None
  }

  /**
   *  Given a set of keys and corresponding lists of values, explode into maps of all possible key/value pairs
   */
  def allKeyValueCombinations[V](listsOfValues : List[List[V]]) : List[List[V]] = {

    if (listsOfValues.isEmpty)
      return List[List[V]]()

    def recurse(listsOfValues : List[List[V]], accumulator : List[List[V]]) : List[List[V]] = {
      listsOfValues match {
        case Nil => accumulator
        case (values : List[_]) :: rest =>{
          val newAccumulator : List[List[V]] = values.flatMap{
            v => accumulator.map{
              acc => v :: acc
            }
          }
          recurse(rest, newAccumulator)
        }
      }
    }
    recurse(listsOfValues, List(List[V]()))
  }


  /**
   * returns the index of the first element of x[] gtr than x0
   * if it exists, otherwise returns index of last element
   */
  def firstGtr(x: Seq[Double], x0: Double): Int = {
    x.findIndexOf(_ > x0) match {
      case -1 => x.toList.size - 1
      case i => i
    }
  }

  /**
   * A common pattern for lazy placement into a concurrent map
   * Returns the value either from the map or from calling f. Also returns
   * boolean, true if the value was already in the map. False otherwise.
   */
  def putIfAbsent[K,V](map: ConcurrentMap[K,FutureTask[V]], k: K, f: => V): (V, Boolean) = {
    val task = new FutureTask[V](new Callable[V]() {
      def call = {
        f
      }
    })
    var hit = true
    var actualTask = map.putIfAbsent(k, task)
    if (actualTask == null) { //null indicates there was no previous entry for key
      hit = false
      actualTask = task
      task.run()
    }
    try {
      (actualTask.get(), hit)
    } catch {
      case e:ExecutionException => throw e.getCause
    }
  }

  /**
   * Finds nearest element in the list x[] to x0. x doesn't need to be sorted
   */
  def findNearest(x: List[Double], x0: Double): Int = {
    x.map(e =>abs(x0 - e)).zipWithIndex.sortWith(_._1 < _._1).head._2
  }

  case class JavaEnumerationIterator[A](itr: java.util.Enumeration[A]) extends Iterator[A] {
    def hasNext = itr.hasMoreElements

    def next() = itr.nextElement
  }

  implicit def implicitJavaEnumerationToScalaIterable[A](enumeration: java.util.Enumeration[A]): Iterator[A] =
    JavaEnumerationIterator(enumeration)


  def filterOnType[T](collection : scala.collection.Traversable[_])(implicit m : Manifest[T]) = {
    collection.filter(m.erasure.isInstance(_)).map(_.asInstanceOf[T])
  }

  def levenshtein(s: Array[Any], t: Array[Any]): Int = {
    val m = s.size
    val n = t.size
    val d = Array.ofDim[Int](m + 1, n + 1)
    (0 to m).foreach(i => d(i)(0) = i)
    (0 to n).foreach(j => d(0)(j) = j)
    (1 to n).foreach {
      j => {
        (1 to m).foreach {
          i => {
            val a = s(i - 1)
            val b = t(j - 1)
            if (a == b) {
              d(i)(j) = d(i - 1)(j - 1)
            } else {
              d(i)(j) = min(
                d(i - 1)(j) + 1, // del
                d(i)(j - 1) + 1, // ins
                d(i - 1)(j - 1) + 1 // subs
                )
            }
          }
        }
      }
    }
    d(m)(n)
  }

  def min(a: Int, b: Int, c: Int): Int = {
    math.min(a, math.min(b, c))
  }
}
