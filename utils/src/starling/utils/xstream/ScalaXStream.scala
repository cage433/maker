package starling.utils.xstream

import scala.collection.immutable.{ HashMap, HashSet, Map, Set, TreeMap, TreeSet }
import scala.collection.mutable.{ ArrayBuffer, ListBuffer }

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.converters.collections.AbstractCollectionConverter
import com.thoughtworks.xstream.mapper.Mapper
import com.thoughtworks.xstream.io.{ HierarchicalStreamReader, HierarchicalStreamWriter }
import java.lang.{Class, String}
import com.thoughtworks.xstream.converters._
import starling.utils.Log

object ScalaXStream {
  def configure(xstream : XStream) = {
    ListConverter.configure(xstream)
    TupleConverter.configure(xstream)
    TreeSetConverter.configure(xstream)
    TreeMapConverter.configure(xstream)
//    HashSetConverter.configure(xstream) // I've added HashSets to the DefaultSetConverter. I'll leave them here for a bit though in case we run into some problems.
    HashMapConverter.configure(xstream)
    DefaultSetConverter.configure(xstream)
    DefaultMapConverter.configure(xstream)
    ScalaObjectConverter.configure(xstream)
    xstream
  }

  val DefaultSetPosition = -10
  val ObjectPosition = -20
}

import ScalaXStream._

object ConverterUtils {
  def withChild[T](reader : HierarchicalStreamReader)(op : => T) = {
    reader.moveDown
    val result = op
    reader.moveUp
    result
  }
}

object ScalaObjectConverter extends Log {
  def configure(xstream : XStream) {
    xstream.registerConverter(new Converter() {
      def canConvert(theType:Class[_]) = theType.getName.endsWith("$")
      def marshal(obj:Object, writer:HierarchicalStreamWriter, context:MarshallingContext) {
        //nothing to store, we just need the class name
      }
      def unmarshal(reader:HierarchicalStreamReader , context:UnmarshallingContext ) = {
        val klass = context.getRequiredType
        val field = klass.getField("MODULE$")
        val r = field.get(null)
        log.debug("scalaobject:: " + r)
        r
      }
    }, ObjectPosition) // I'm putting this in at a lower priority than others so that it doesn't pick up the EmptySet$ and EmptyMap$ that can come up occasionally.
  }
}

object ListConverter {
  def configure(xstream : XStream, alias : String = "list") = {
    xstream.aliasType(alias, classOf[List[_]])
    xstream.registerConverter(new ListConverter(xstream.getMapper))
    xstream
  }
}

class ListConverter(mapper : Mapper) extends AbstractCollectionConverter(mapper) with Log {
  import ConverterUtils._

  override def canConvert(clazz : Class[_]) = classOf[List[_]].isAssignableFrom(clazz)

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    val list = value.asInstanceOf[List[_]]
    list.foreach(writeItem(_, context, writer))
  }

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val buffer = new ListBuffer[Any]
    while (reader.hasMoreChildren)
      withChild(reader)(buffer += readItem(reader, context, buffer))
    val r = buffer.toList
    log.debug("list:: " + r)
    r
  }
}

abstract class ProductConverter(mapper : Mapper) extends AbstractCollectionConverter(mapper) {
  override def canConvert(clazz : Class[_]) = classOf[Product].isAssignableFrom(clazz)

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    val product = value.asInstanceOf[Product]
    product.productIterator.foreach(writeItem(_, context, writer))
  }
}

object TupleConverter {
  def configure(xstream : XStream, alias : String = "tuple") = {
    tuples.foreach(xstream.aliasType(alias, _))
    xstream.registerConverter(new TupleConverter(xstream.getMapper))
    xstream
  }

  private val tuples : Set[Class[_]]= Set(
    classOf[Tuple1[_]],
    classOf[Tuple2[_, _]],
    classOf[Tuple3[_, _, _]],
    classOf[Tuple4[_, _, _, _]],
    classOf[Tuple5[_, _, _, _, _]],
    classOf[Tuple6[_, _, _, _, _, _]],
    classOf[Tuple7[_, _, _, _, _, _, _]],
    classOf[Tuple8[_, _, _, _, _, _, _, _]],
    classOf[Tuple9[_, _, _, _, _, _, _, _, _]],
    classOf[Tuple10[_, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]],
    classOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
  )
}

class TupleConverter(mapper : Mapper) extends ProductConverter(mapper) with Log {
  import TupleConverter._
  import ConverterUtils._

  override def canConvert(clazz : Class[_]) = tuples(clazz) || classOf[Tuple1[_]].isAssignableFrom(clazz) || classOf[Tuple2[_, _]].isAssignableFrom(clazz)

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val buffer = new ArrayBuffer[AnyRef]
    while (reader.hasMoreChildren)
      withChild(reader)(buffer += readItem(reader, context, buffer))
    val arity = buffer.size
    if (arity < 1 || arity > 22)
      throw new ConversionException("Unsupported Tuple arity: "+arity)
    val tupleClazz = Class.forName("scala.Tuple"+arity)
    val tupleCtor = tupleClazz.getDeclaredConstructors()(0)
    val r = tupleCtor.newInstance(buffer: _*).asInstanceOf[Product]
    log.debug("tuple:: " + r)
    r
  }
}

trait TreeCollectionConverter extends Log {
  import ConverterUtils._

  val mapper: Mapper

  def marshalOrdering(ordering : Ordering[_], writer : HierarchicalStreamWriter, context : MarshallingContext) {
    writer.startNode("ordering")
    writer.addAttribute("class", mapper.serializedClass(ordering.getClass))
    context.convertAnother(ordering)
    writer.endNode
  }

  def unmarshallOrdering(reader : HierarchicalStreamReader, context : UnmarshallingContext) : Ordering[Any] = {
    val r = withChild(reader) {
      val orderingClazz = reader.getAttribute("class")
      context.convertAnother(null, mapper.realClass(orderingClazz)).asInstanceOf[Ordering[Any]]
    }
    log.debug("tree:: " + r)
    r
  }
}

abstract class SetConverter(mapper : Mapper) extends AbstractCollectionConverter(mapper) with Log {
  import ConverterUtils._

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    val set = value.asInstanceOf[Set[_]]
    set.foreach(writeItem(_, context, writer))
  }

  def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext, ctor : (Seq[Any]) => AnyRef) = {
    val buffer = new ListBuffer[Any]
    while (reader.hasMoreChildren)
      withChild(reader)(buffer += readItem(reader, context, buffer))
    val r = ctor(buffer)
    log.debug("set:: " + r)
    r
  }
}

abstract class MapConverter(override val mapper : Mapper) extends AbstractCollectionConverter(mapper) with Log {
  import ConverterUtils._

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    val map = value.asInstanceOf[Map[_, _]]
    for ((key, value) <- map) {
      writer.startNode("entry")
      writeItem(key, context, writer)
      writeItem(value, context, writer)
      writer.endNode
    }
  }

  def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext, ctor : (Seq[(Any, Any)] => AnyRef)) = {
    val buffer = new ListBuffer[(Any, Any)]
    while (reader.hasMoreChildren)
      withChild(reader) {
        val key = withChild(reader)(readItem(reader, context, buffer))
        val value = withChild(reader)(readItem(reader, context, buffer))
        buffer += ((key, value))
      }
    val r = ctor(buffer)
    log.debug("map:: " + r)
    r
  }
}

object TreeSetConverter {
  def configure(xstream : XStream, alias : String = "set") = {
    xstream.alias(alias, classOf[TreeSet[_]])
    xstream.registerConverter(new TreeSetConverter(xstream.getMapper))
    xstream
  }
}

class TreeSetConverter(override val mapper : Mapper) extends SetConverter(mapper) with TreeCollectionConverter {
  import ConverterUtils._

  override def canConvert(clazz : Class[_]) = clazz == classOf[TreeSet[_]]

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    val set = value.asInstanceOf[TreeSet[_]]
    marshalOrdering(set.ordering, writer, context)
    super.marshal(value, writer, context)
  }

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val ordering = unmarshallOrdering(reader, context)
    val r = unmarshal(reader, context, x => TreeSet(x : _*)(ordering))
    log.debug("treeset:: " + r)
    r
  }
}

object TreeMapConverter {
  def configure(xstream : XStream, alias : String = "map") = {
    xstream.alias(alias, classOf[TreeMap[_, _]])
    xstream.registerConverter(new TreeMapConverter(xstream.getMapper))
    xstream
  }
}

class TreeMapConverter(mapper : Mapper) extends MapConverter(mapper) with TreeCollectionConverter {
  import ConverterUtils._

  override def canConvert(clazz : Class[_]) = clazz == classOf[TreeMap[_, _]]

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    val map = value.asInstanceOf[TreeMap[_, _]]
    marshalOrdering(map.ordering, writer, context)
    super.marshal(value, writer, context)
  }

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val ordering = unmarshallOrdering(reader, context)
    val r = unmarshal(reader, context, x => TreeMap(x : _*)(ordering))
    log.debug("treemap:: " + r)
    r
  }
}

object DefaultSetConverter {
  val defaultSets : Set[Class[_]] = Set(
    Set.empty.getClass,
    classOf[Set.EmptySet[_]],
    classOf[Set.Set1[_]],
    classOf[Set.Set2[_]],
    classOf[Set.Set3[_]],
    classOf[Set.Set4[_]],
    classOf[HashSet[_]],
    classOf[HashSet.HashSet1[_]],
    classOf[HashSet.HashTrieSet[_]]
  )

  def configure(xstream : XStream, alias : String = "default-set") = {
    defaultSets.foreach(xstream.alias(alias, _))
    xstream.registerConverter(new DefaultSetConverter(xstream.getMapper), DefaultSetPosition)
    xstream
  }
}

class DefaultSetConverter(override val mapper : Mapper) extends SetConverter(mapper) {
  import DefaultSetConverter._

  override def canConvert(clazz : Class[_]) = classOf[Set[_]].isAssignableFrom(clazz)//defaultSets(clazz)

  override def marshal(value:AnyRef, writer:HierarchicalStreamWriter, context:MarshallingContext) = {
    if (!defaultSets(value.getClass)) {
      throw new Exception("Don't know how to serialize: " + value.getClass + " : " + value)
    }
    super.marshal(value, writer, context)
  }

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val r = unmarshal(reader, context, x => Set(x : _*))
    log.debug("defaultset:: " + r)
    r
  }
}

object DefaultMapConverter {
  val defaultMaps : Set[Class[_]] = Set(
    classOf[Map.EmptyMap[_, _]],
    classOf[Map.Map1[_, _]],
    classOf[Map.Map2[_, _]],
    classOf[Map.Map3[_, _]],
    classOf[Map.Map4[_, _]]
  )

  def configure(xstream : XStream, alias : String = "default-map") = {
    defaultMaps.foreach(xstream.alias(alias, _))
    xstream.registerConverter(new DefaultMapConverter(xstream.getMapper))
    xstream
  }
}

class DefaultMapConverter(override val mapper : Mapper) extends MapConverter(mapper) {
  import DefaultMapConverter._

  override def canConvert(clazz : Class[_]) = defaultMaps(clazz)

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val r = unmarshal(reader, context, x => Map(x : _*))
    log.debug("defaultmap:: " + r)
    r
  }
}

object HashSetConverter {
  def configure(xstream : XStream, alias : String = "hash-set") = {
    xstream.aliasType(alias, classOf[HashSet[_]])
    xstream.registerConverter(new HashSetConverter(xstream.getMapper))
    xstream
  }
}

class HashSetConverter(override val mapper : Mapper) extends SetConverter(mapper) {
  override def canConvert(clazz : Class[_]) = classOf[HashSet[_]].isAssignableFrom(clazz)

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    throw new Exception("Disabled persistance of Sets as the order is not consistent so you get duplicates. Use a SortedSet")
  }

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val r = unmarshal(reader, context, x => HashSet(x : _*))
    log.debug("hashset:: " + r)
    r
  }
}

object HashMapConverter {
  def configure(xstream : XStream, alias : String = "hash-map") = {
    xstream.aliasType(alias, classOf[HashMap[_, _]])
    xstream.registerConverter(new HashMapConverter(xstream.getMapper))
    xstream
  }
}

class HashMapConverter(override val mapper : Mapper) extends MapConverter(mapper) {
  import ConverterUtils._

  override def canConvert(clazz : Class[_]) = classOf[HashMap[_, _]].isAssignableFrom(clazz)

  override def marshal(value : AnyRef, writer : HierarchicalStreamWriter, context : MarshallingContext) = {
    println("Here's the hashmap: " + value)
    throw new Exception("Disabled persistance of HashMaps as the order is not consistent so you get duplicates. Use a SortedMap")
  }

  override def unmarshal(reader : HierarchicalStreamReader, context : UnmarshallingContext) = {
    val r = unmarshal(reader, context, x => HashMap(x : _*))
    log.debug("hashmap:: " + r)
    r
  }
}