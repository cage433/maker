package com.trafigura.services

import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl
import java.lang.reflect.{ParameterizedType, Type}
import java.util.ArrayList


trait TypeConverter {
  val baseType: Class[_]
  val genericType: Type

  def convert(value: AnyRef): AnyRef
}

object TypeConverter {
  def apply(`type`: Class[_], genericType: Type) = if (`type` == classOf[Option[_]])
    OptionTypeConverter(listClass, parameterizedList(genericType)) else NullTypeConverter(`type`, genericType)

  private def parameterizedList(genericType: Type): ParameterizedType =
    ParameterizedTypeImpl.make(listClass, genericType.asInstanceOf[ParameterizedType].getActualTypeArguments, null)

  private val listClass = classOf[java.util.List[_]]
}

case class OptionTypeConverter(baseType: Class[_], genericType: Type) extends TypeConverter {
  def convert(value: AnyRef) = {
    val list = value.asInstanceOf[ArrayList[_]]

    if (list.isEmpty) None else Some(list.get(0))
  }
}

case class NullTypeConverter(baseType: Class[_], genericType: Type) extends TypeConverter {
  def convert(value: AnyRef) = value
}