package cherry.lamr.norm.umami

import cherry.lamr.BuiltinType
import cherry.lamr.norm.Process

trait SimpleBinaryFn extends FnValueBase:
  def typ: NormType
  
  override def calcBodyType: Process[NormType] = Process.pure(typ)
  override def domain: NormType = typ

trait SimpleBuiltinBinaryFn extends SimpleBinaryFn:
  def builtinType: BuiltinType
  val typ = BuiltinNormType(builtinType)
