package starling.webservice

import java.lang.annotation.Annotation
import meta.ResteasyTypes
import org.jboss.resteasy.util.FindAnnotation._
import javax.ws.rs._
import core.Context
import org.jboss.resteasy.annotations.{Form, Suspend}
import org.jboss.resteasy.core._
import java.lang.reflect.{Type, AccessibleObject}
import org.jboss.resteasy.spi.{Failure, HttpResponse, HttpRequest, ResteasyProviderFactory}
import starling.utils.Pattern._

class ScalaInjectorFactory extends InjectorFactoryImpl(ResteasyProviderFactory.getInstance) {
//  ResteasyServiceApi.registerProviderInstance
  private val factory = ResteasyProviderFactory.getInstance

  override def createParameterExtractor(targetClass: Class[_], target: AccessibleObject, clazz : Class[_], genericType: Type,
                                        annotations: Array[Annotation], useDefault: Boolean): ValueInjector = {

    val default = defaultValue(annotations)

    annotations.toList.find(ResteasyTypes.matchingAnnotation).map(_ match {
      case uriParam: PathParam  => new ScalaPathParamInjector(TypeConverter(clazz, genericType), target, uriParam.value, annotations)
      case query: QueryParam    => new ScalaQueryParamInjector(TypeConverter(clazz, genericType), target, query.value, annotations)
      case header: HeaderParam  => new HeaderParamInjector(clazz, genericType, target, header.value, default, annotations, factory)
      case formParam: FormParam => new FormParamInjector(clazz, genericType, target, formParam.value, default, annotations, factory)
      case cookie: CookieParam  => new CookieParamInjector(clazz, genericType, target, cookie.value, default, annotations, factory)
      case matrix: MatrixParam  => new MatrixParamInjector(clazz, genericType, target, matrix.value, default, annotations, factory)
      case _: Form              => new FormInjector(clazz, factory)
      case suspend: Suspend     => new SuspendInjector(suspend, clazz)
      case _: Context           => new ContextParameterInjector(clazz, factory)
    }).getOrElse {
      if (!useDefault) null else new MessageBodyParameterInjector(targetClass, target, clazz, genericType, annotations, factory)
    }
  }

  trait ConvertingValueInjector extends ValueInjector {
    val typeConverter: TypeConverter

    abstract override def inject(request: HttpRequest, response: HttpResponse) = try {
      typeConverter.convert(super.inject(request, response))
    } catch {
      case NestedException(_, NestedException(_, nested)) => throw new Failure(nested)
      case NestedException(_, nested) => throw new Failure(nested)
    }
  }

  class ScalaPathParamInjector(val typeConverter: TypeConverter, target: AccessibleObject, paramName: String, annotations: Array[Annotation])
    extends PathParamInjector(typeConverter.baseType, typeConverter.genericType, target, paramName, defaultValue(annotations),
      shouldEncode(annotations, target, typeConverter.baseType), annotations, factory) with ConvertingValueInjector

  class ScalaQueryParamInjector(val typeConverter: TypeConverter, target: AccessibleObject, paramName: String, annotations: Array[Annotation])
    extends QueryParamInjector(typeConverter.baseType, typeConverter.genericType, target, paramName, defaultValue(annotations),
      shouldEncode(annotations, target, typeConverter.baseType), annotations, factory) with ConvertingValueInjector


  private def defaultValue(annotations: Array[Annotation]): String = {
    val annotation = findAnnotation(annotations, classOf[DefaultValue])
    if (annotation != null) annotation.value else null
  }

  private def shouldEncode(annotations: Array[Annotation], target: AccessibleObject, `type` : Class[_]) =
    findAnnotation(annotations, classOf[Encoded]) != null || target.isAnnotationPresent(classOf[Encoded]) ||
      `type`.isAnnotationPresent(classOf[Encoded])
}

