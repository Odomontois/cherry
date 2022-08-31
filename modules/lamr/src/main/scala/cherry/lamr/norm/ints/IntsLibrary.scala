package cherry.lamr.norm.ints
import cherry.lamr.norm.umami.{BuiltinNormType, FnValueBase, IntegerValue, SimpleBinaryFn, SimpleBuiltinBinaryFn}
import cherry.lamr.{BuiltinType, Lang, LibRef, RecordKey}
import cherry.lamr.norm.{Cause, Library, NameResolutionLibrary, NormValue, Normalizer, Process, Term}
import cherry.utils.Act

object IntsLibrary extends NameResolutionLibrary("ints"):
  val members = Vector(
    "plus"   -> Fn("plus", _ + _),
    "minus"  -> Fn("minus", _ - _),
    "div"    -> Fn("div", _ / _),
    "mul"    -> Fn("mul", _ * _),
    "xor"    -> Fn("xor", _ ^ _),
    "bitand" -> Fn("bitand", _ & _),
    "bitor"  -> Fn("bitor", _ | _),
    "gcd"    -> Fn("gcd", _ gcd _),
  )

  class Fn(name: String, call: (BigInt, BigInt) => BigInt) extends NormValue with SimpleBuiltinBinaryFn:
    def builtinType     = BuiltinType.Integer
    override def toTerm = Process.pure(Lang.External(LibRef("ints", Lang.get(name))))

    override def apply(term: NormValue): Process[NormValue] =
      term.first.flatMap(_.asInt).map2Par(term.second.flatMap(_.asInt))((x, y) => IntegerValue(call(x, y)))
