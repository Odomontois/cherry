package cherry.lamr.norm.bools

import cherry.lamr.Lang.Builtin
import cherry.lamr.BuiltinType
import cherry.lamr.norm.umami.{BooleanValue, BuiltinNormType, FnValueBase, RecordType, SimpleBuiltinBinaryFn}
import cherry.lamr.{Lang, LibRef, RecordKey}
import cherry.lamr.norm.{Cause, Library, NameResolutionLibrary, NormValue, Normalizer, Process, Term}
import cherry.utils.Act

object BooleansLibrary extends NameResolutionLibrary("bools"):
  val members = Vector(
    "xor"    -> Fn("xor", _ ^ _),
    "bitand" -> Fn("bitand", _ & _),
    "bitor"  -> Fn("bitor", _ | _),
  )

  class Fn(name: String, call: (Boolean, Boolean) => Boolean) extends NormValue with SimpleBuiltinBinaryFn:
    def builtinType     = BuiltinType.Bool
    override def toTerm = Process.pure(Lang.External(LibRef("bools", Lang.get(name))))

    override def apply(term: NormValue): Process[NormValue] =
      term.first.flatMap(_.asBool).map2Par(term.second.flatMap(_.asBool))((x, y) => BooleanValue(call(x, y)))
