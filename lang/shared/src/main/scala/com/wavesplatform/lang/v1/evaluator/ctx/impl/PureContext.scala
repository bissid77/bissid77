package com.wavesplatform.lang.v1.evaluator.ctx.impl

import java.nio.charset.StandardCharsets

import cats.data.EitherT
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Types}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.parser.BinaryOperation._
import scodec.bits.ByteVector

import scala.util.Try

object PureContext {
  private val defaultThrowMessage = "Explicit script termination"
  val MaxStringResult = Short.MaxValue
  val MaxBytesResult = 65536

  val mulLong: BaseFunction   = createTryOp(MUL_OP, LONG, LONG, MUL_LONG, "Integer multiplication", "multiplyer", "multiplyer")((a, b) => Math.multiplyExact(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val divLong: BaseFunction   = createTryOp(DIV_OP, LONG, LONG, DIV_LONG, "Integer devision", "divisible", "divisor")((a, b) => Math.floorDiv(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val modLong: BaseFunction   = createTryOp(MOD_OP, LONG, LONG, MOD_LONG, "Modulo", "divisible", "divisor")((a, b) => Math.floorMod(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val sumLong: BaseFunction   = createTryOp(SUM_OP, LONG, LONG, SUM_LONG, "Integer sum", "term", "term")((a, b) => Math.addExact(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val subLong: BaseFunction   = createTryOp(SUB_OP, LONG, LONG, SUB_LONG, "Integer substitution", "term", "term")((a, b) => Math.subtractExact(a.asInstanceOf[Long], b.asInstanceOf[Long]))
  val sumString: BaseFunction = createRawOp(SUM_OP, STRING, STRING, SUM_STRING, "Limited strings concatination", "prefix", "suffix", 10)((a, b) => {
               val astr = a.asInstanceOf[String]
               val bstr = b.asInstanceOf[String]
               val al = astr.length
               val bl = bstr.length
               Either.cond(al + bl <= MaxStringResult, astr + bstr, "String is too large")
             })
  val sumByteVector: BaseFunction =
    createRawOp(SUM_OP, BYTEVECTOR, BYTEVECTOR, SUM_BYTES, "Limited bytes vectors concatination", "prefix", "suffix", 10)((a, b) => {
               val avec = a.asInstanceOf[ByteVector]
               val bvec = b.asInstanceOf[ByteVector]
               val al = avec.length
               val bl = bvec.length
               Either.cond(al + bl <= MaxBytesResult, ByteVector.concat(Seq(avec, bvec)), "ByteVector is too large")
       })
  val ge: BaseFunction = createOp(GE_OP, LONG, BOOLEAN, GE_LONG, "Integer grater or equal comparation", "term", "term")((a, b) => a.asInstanceOf[Long] >= b.asInstanceOf[Long])
  val gt: BaseFunction = createOp(GT_OP, LONG, BOOLEAN, GT_LONG, "Integer grater comparation", "term", "term")((a, b) => a.asInstanceOf[Long] > b.asInstanceOf[Long])

  val eq: BaseFunction =
    NativeFunction(EQ_OP.func, 1, EQ, BOOLEAN, "Equality", ("a", TYPEPARAM('T'), "value"), ("b", TYPEPARAM('T'), "value")) {
      case a :: b :: Nil => Right(a == b)
      case _             => ???
    }

  val ne: BaseFunction =
    UserFunction(NE_OP.func, BOOLEAN, "Unequality", ("a", TYPEPARAM('T'), "value"), ("b", TYPEPARAM('T'), "value")) {
      case a :: b :: Nil => FUNCTION_CALL(uNot, List(FUNCTION_CALL(eq, List(a, b))))
      case _             => ???
    }

  val throwWithMessage: BaseFunction = NativeFunction("throw", 1, THROW, NOTHING, "Fail script", ("err", STRING, "Error message")) {
    case (err: String) :: Nil => Left(err)
    case _ => Left(defaultThrowMessage)
  }

  val throwNoMessage: BaseFunction = UserFunction("throw", NOTHING, "Fail script") { _ =>
    FUNCTION_CALL(throwWithMessage, List(CONST_STRING(defaultThrowMessage)))
  }

  val isDefined: BaseFunction =
    UserFunction("isDefined", BOOLEAN, "Check the value is defined", ("a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)), "Option value")) {
      case a :: Nil => FUNCTION_CALL(ne, List(a, REF("unit")))
      case _        => ???
    }

  val extract: BaseFunction =
    UserFunction("extract", TYPEPARAM('T'), "Exytract value from option or fail", ("a", PARAMETERIZEDUNION(List(TYPEPARAM('T'), UNIT)),"Option value")) {
      case a :: Nil =>
        IF(FUNCTION_CALL(eq, List(a, REF("unit"))),
          FUNCTION_CALL(throwWithMessage, List(CONST_STRING("extract() called on unit value"))),
          a)
      case _        => ???
    }

  val fraction: BaseFunction = NativeFunction("fraction", 1, FRACTION, LONG,
                        "Multiply and dividion with big integer intermediate representation",
                        ("value", LONG, "multiplyer"), ("numerator", LONG, "multiplyer"), ("denominator", LONG, "divisor")) {
    case (v: Long) :: (n: Long) :: (d: Long) :: Nil =>
      val result = BigInt(v) * n / d
      for {
        _ <- Either.cond(result < Long.MaxValue, (), s"Long overflow: value `$result` greater than 2^63-1")
        _ <- Either.cond(result > Long.MinValue, (), s"Long overflow: value `$result` less than -2^63-1")
      } yield result.toLong
    case _ => ???
  }

  val _isInstanceOf: BaseFunction = NativeFunction("_isInstanceOf", 1, ISINSTANCEOF, BOOLEAN, "Internal function to check value type", ("obj", TYPEPARAM('T'), "value"), ("of", STRING, "type name")) {
    case (p: Boolean) :: ("Boolean") :: Nil       => Right(true)
    case (p: ByteVector) :: ("ByteVector") :: Nil => Right(true)
    case (p: String) :: ("String") :: Nil         => Right(true)
    case (p: Long) :: ("Int") :: Nil              => Right(true)
    case (()) :: ("Unit") :: Nil                  => Right(true)
    case (p: CaseObj) :: (s: String) :: Nil       => Right(p.caseType.name == s)
    case _                                        => Right(false)
  }

  val sizeBytes: BaseFunction = NativeFunction("size", 1, SIZE_BYTES, LONG, "Size of bytes vector", ("byteVector", BYTEVECTOR, "vector")) {
    case (bv: ByteVector) :: Nil => Right(bv.size)
    case xs                      => notImplemented("size(byte[])", xs)
  }

  val toBytesBoolean: BaseFunction = NativeFunction("toBytes", 1, BOOLEAN_TO_BYTES, BYTEVECTOR, "Bytes array representation", ("b", BOOLEAN, "value")) {
    case (b: Boolean) :: Nil => Right(ByteVector(if (b) 1 else 0))
    case _ => ???
  }

  val toBytesLong: BaseFunction = NativeFunction("toBytes", 1, LONG_TO_BYTES, BYTEVECTOR, "Bytes array representation", ("n", LONG, "value")) {
    case (n: Long) :: Nil => Right(ByteVector.fromLong(n))
    case _ => ???
  }

  val toBytesString: BaseFunction = NativeFunction("toBytes", 1, STRING_TO_BYTES, BYTEVECTOR, "Bytes array representation", ("s", STRING, "value")) {
    case (s: String) :: Nil => Right(ByteVector(s.getBytes(StandardCharsets.UTF_8)))
    case _ => ???
  }

  val sizeString: BaseFunction = NativeFunction("size", 1, SIZE_STRING, LONG, "Scting size in characters", ("xs", STRING, "string")) {
    case (bv: String) :: Nil => Right(bv.length.toLong)
    case xs                  => notImplemented("size(String)", xs)
  }

  val toStringBoolean: BaseFunction = NativeFunction("toString", 1, BOOLEAN_TO_STRING, STRING, "String representation", ("b", BOOLEAN, "value")) {
    case (b: Boolean) :: Nil => Right(b.toString)
    case _ => ???
  }

  val toStringLong: BaseFunction = NativeFunction("toString", 1, LONG_TO_STRING, STRING, "String representation", ("n", LONG, "value")) {
    case (n: Long) :: Nil => Right(n.toString)
    case _ => ???
  }

  val takeBytes: BaseFunction = NativeFunction("take", 1, TAKE_BYTES, BYTEVECTOR, "Take firsts bytes subvector", ("xs", BYTEVECTOR, "vector"), ("number", LONG, "Bytes number")) {
    case (xs: ByteVector) :: (number: Long) :: Nil => Right(xs.take(number))
    case xs                                        => notImplemented("take(xs: byte[], number: Long)", xs)
  }

  val dropBytes: BaseFunction = NativeFunction("drop", 1, DROP_BYTES, BYTEVECTOR, "Skip firsts bytes", ("xs", BYTEVECTOR, "vector"), ("number", LONG, "Bytes number")) {
    case (xs: ByteVector) :: (number: Long) :: Nil => Right(xs.drop(number))
    case xs                                        => notImplemented("drop(xs: byte[], number: Long)", xs)
  }

  val dropRightBytes: BaseFunction = UserFunction("dropRight", "dropRightBytes", BYTEVECTOR, "Cut vectors tail", ("xs", BYTEVECTOR, "vector"), ("number", LONG, "cuting size")) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        takeBytes,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeBytes, List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("dropRight(xs: byte[], number: Long)", xs)
  }

  val takeRightBytes: BaseFunction = UserFunction("takeRight", "takeRightBytes", BYTEVECTOR, "Take vector tail", ("xs", BYTEVECTOR, "vector"), ("number", LONG, "taking size")) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        dropBytes,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeBytes, List(xs)),
              number
            )
          )
        )
      )
    case xs => ???
  }

  private def trimLongToInt(x: Long): Int = Math.toIntExact(Math.max(Math.min(x, Int.MaxValue), Int.MinValue))

  val takeString: BaseFunction = NativeFunction("take", 1, TAKE_STRING, STRING, "Take string prefix", ("xs", STRING, "sctring"), ("number", LONG, "prefix size in characters")) {
    case (xs: String) :: (number: Long) :: Nil => Right(xs.take(trimLongToInt(number)))
    case xs                                    => notImplemented("take(xs: String, number: Long)", xs)
  }

  val dropString: BaseFunction = NativeFunction("drop", 1, DROP_STRING, STRING, "Remmove sring prefix", ("xs",STRING, "string"), ("number", LONG, "prefix size")) {
    case (xs: String) :: (number: Long) :: Nil => Right(xs.drop(trimLongToInt(number)))
    case xs                                    => notImplemented("drop(xs: String, number: Long)", xs)
  }

  val takeRightString: BaseFunction = UserFunction("takeRight", STRING, "Take string suffix", ("xs", STRING, "String"), ("number", LONG, "suffix size in characters")) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        dropString,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeString, List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("takeRight(xs: String, number: Long)", xs)
  }

  val dropRightString: BaseFunction = UserFunction("dropRight", STRING, "Remove string suffix", ("xs", STRING, "string"), ("number", LONG, "suffix size in characters")) {
    case (xs: EXPR) :: (number: EXPR) :: Nil =>
      FUNCTION_CALL(
        takeString,
        List(
          xs,
          FUNCTION_CALL(
            subLong,
            List(
              FUNCTION_CALL(sizeString, List(xs)),
              number
            )
          )
        )
      )
    case xs => notImplemented("dropRight(xs: String, number: Long)", xs)
  }

  def createRawOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, docString: String, arg1Doc: String, arg2Doc: String, complicity: Int = 1)(body: (Any, Any) => Either[String, Any]): BaseFunction =
    NativeFunction(opsToFunctions(op), complicity, func, r, docString, ("a", t, arg1Doc), ("b", t, arg2Doc)) {
      case a :: b :: Nil => body(a, b)
      case _             => ???
    }

  def createOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, docString: String, arg1Doc: String, arg2Doc: String, complicity: Int = 1)(body: (Any, Any) => Any): BaseFunction =
    NativeFunction(opsToFunctions(op), complicity, func, r, docString, ("a", t, arg1Doc), ("b", t, arg2Doc)) {
      case a :: b :: Nil => Right(body(a, b))
      case _             => ???
    }

  def createTryOp(op: BinaryOperation, t: TYPE, r: TYPE, func: Short, docString: String, arg1Doc: String, arg2Doc: String, complicity: Int = 1)(body: (Any, Any) => Any): BaseFunction =
    NativeFunction(opsToFunctions(op), complicity, func, r, docString, ("a", t, arg1Doc), ("b", t, arg2Doc)) {
      case a :: b :: Nil =>
        try {
          Right(body(a, b))
        } catch {
          case e: Throwable => Left(e.getMessage)
        }
      case _ => ???
    }

  val getElement: BaseFunction =
    NativeFunction("getElement", 2, GET_LIST, TYPEPARAM('T'), "Get list element by position", ("arr", PARAMETERIZEDLIST(TYPEPARAM('T')), "list"), ("pos", LONG, "element position")) {
      case (arr: IndexedSeq[_]) :: (pos: Long) :: Nil => Try(arr(pos.toInt)).toEither.left.map(_.toString)
      case _                                          => ???
    }

  val getListSize: BaseFunction = NativeFunction("size", 2, SIZE_LIST, LONG, "Size of list", ("arr", PARAMETERIZEDLIST(TYPEPARAM('T')), "list")) {
    case (arr: IndexedSeq[_]) :: Nil => Right(arr.size.toLong)
    case _                           => ???
  }

  val uMinus: BaseFunction = UserFunction("-", LONG, "Change integer sign", ("n", LONG, "value")) {
    case n :: Nil => FUNCTION_CALL(subLong, List(CONST_LONG(0), n))
    case _        => ???
  }

  val uNot: BaseFunction = UserFunction("!", BOOLEAN, "Boolean `not`", ("p", BOOLEAN, "value")) {
    case p :: Nil => IF(FUNCTION_CALL(eq, List(p, FALSE)), TRUE, FALSE)
    case _        => ???
  }

  private val operators: Seq[BaseFunction] = Seq(
    mulLong,
    divLong,
    modLong,
    sumLong,
    subLong,
    sumString,
    sumByteVector,
    eq,
    ne,
    ge,
    gt,
    getElement,
    getListSize,
    uMinus,
    uNot
  )

  private val vars: Map[String, ((Types.FINAL, String), LazyVal)] = Map(("unit", ((Types.UNIT,"Single instance value"), LazyVal(EitherT.pure(())))))
  private val functions = Seq(
    fraction,
    sizeBytes,
    toBytesBoolean,
    toBytesLong,
    toBytesString,
    takeBytes,
    dropBytes,
    takeRightBytes,
    dropRightBytes,
    sizeString,
    toStringBoolean,
    toStringLong,
    takeString,
    dropString,
    takeRightString,
    dropRightString,
    _isInstanceOf,
    isDefined,
    extract,
    throwWithMessage,
    throwNoMessage
  ) ++ operators

  lazy val ctx = CTX(
    Seq(
      new DefinedType { val name = "Unit"; val typeRef       = Types.UNIT       },
      new DefinedType { val name = "Int"; val typeRef        = Types.LONG       },
      new DefinedType { val name = "Boolean"; val typeRef    = Types.BOOLEAN    },
      new DefinedType { val name = "ByteVector"; val typeRef = Types.BYTEVECTOR },
      new DefinedType { val name = "String"; val typeRef     = Types.STRING     }
    ),
    vars,
    functions
  )
  lazy val evalContext: EvaluationContext   = ctx.evaluationContext
  lazy val compilerContext: CompilerContext = ctx.compilerContext

  def fromOption[T](v: Option[T]): Any = {
    v.getOrElse((): Any)
  }
}
