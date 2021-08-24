package com.wavesplatform.transaction

import java.math.BigInteger

import scala.reflect.ClassTag

import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto.EthereumKeyLength
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionLike
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.utils.EthEncoding
import monix.eval.Coeval
import org.web3j.abi.TypeDecoder
import org.web3j.abi.datatypes.{Address => EthAddress}
import org.web3j.abi.datatypes.generated.Uint256
import org.web3j.crypto._
import org.web3j.crypto.Sign.SignatureData
import play.api.libs.json._

final case class EthereumTransaction(
    payload: EthereumTransaction.Payload,
    underlying: RawTransaction,
    signatureData: SignatureData,
    override val chainId: Byte
) extends Transaction(TransactionType.Ethereum) {
  import EthereumTransaction._

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(encodeTransaction(underlying, signatureData))

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(TransactionEncoder.encode(underlying, chainId.toLong))

  override val id: Coeval[ByteStr] = Coeval.evalOnce(ByteStr(Hash.sha3(bodyBytes())))

  override def assetFee: (Asset, Long) = Asset.Waves -> underlying.getGasLimit.longValueExact()

  override val timestamp: TxTimestamp = underlying.getNonce.longValueExact()

  val signerPublicKey: Coeval[PublicKey] = Coeval.evalOnce {
    require(signatureData != null, "empty signature data")
    val v          = BigInt(1, signatureData.getV)
    val recoveryId = if (v > 28) v - chainId * 2 - 35 else v - 27
    val sig        = new ECDSASignature(new BigInteger(1, signatureData.getR), new BigInteger(1, signatureData.getS))

    PublicKey(
      ByteStr(
        Sign
          .recoverFromSignature(recoveryId.intValue, sig, id().arr)
          .toByteArray
          .takeRight(EthereumKeyLength)
      )
    )
  }

  val senderAddress: Coeval[Address] = Coeval.evalOnce(signerPublicKey().toAddress(chainId))

  val baseJson: Coeval[JsObject] = for {
    idValue <- id
  } yield Json.obj(
    "id"    -> idValue.toString,
    "type"  -> tpe.id,
    "bytes" -> EthEncoding.toHexString(bytes()),
    "from"  -> EthEncoding.toHexString(senderAddress().publicKeyHash)
  )

  override val json: Coeval[JsObject] = Coeval.evalOnce(baseJson())

  override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = payload.smartAssets(blockchain)
}

object EthereumTransaction {
  sealed trait Payload {
    def smartAssets(blockchain: Blockchain): Seq[IssuedAsset]
  }

  case class Transfer(asset: Either[Asset.Waves.type, ERC20Address], amount: Long, recipient: Address) extends Payload {
    override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = asset match {
      case Right(erc20) => blockchain.resolveERC20Address(erc20).filter(blockchain.hasAssetScript).toSeq
      case _            => Seq.empty
    }

    def json(senderAddress: Address): JsObject =
      Json.obj(
        "transfer" -> Json.obj(
          "sender"    -> senderAddress.toString,
          "recipient" -> recipient.toString,
          "amount"    -> amount,
          "asset" -> (asset match {
            case Left(_)      => JsNull
            case Right(erc20) => EthEncoding.toHexString(erc20.arr)
          })
        )
      )
  }

  case class Invocation(dApp: Address, hexCallData: String) extends Payload {
    override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = Seq.empty

    def toInvokeScriptLike(tx: EthereumTransaction, script: Script): InvokeScriptTransactionLike = new InvokeScriptTransactionLike {
      lazy val (funcCall, payments)                                      = ABIConverter(script).decodeFunctionCall(hexCallData)
      override def id: Coeval[ByteStr]                                   = tx.id
      override def dApp: AddressOrAlias                                  = Invocation.this.dApp
      override def sender: PublicKey                                     = tx.signerPublicKey()
      override def root: InvokeScriptTransactionLike                     = this
      override def assetFee: (Asset, TxTimestamp)                        = tx.assetFee
      override def timestamp: TxTimestamp                                = tx.timestamp
      override def chainId: TxVersion                                    = tx.chainId
      override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = Invocation.this.smartAssets(blockchain)
    }

    def json(senderAddress: Address): JsObject =
      Json.obj("invokeScript" -> Json.obj("sender" -> senderAddress.toString, "dApp" -> dApp.toString))
  }

  val AmountMultiplier = 10000000000L

  private val decodeMethod = {
    val m = classOf[TypeDecoder].getDeclaredMethod("decode", classOf[String], classOf[Int], classOf[Class[_]])
    m.setAccessible(true)
    m
  }

  private def decode[A](source: String, offset: Int)(implicit ct: ClassTag[A]): A =
    decodeMethod.invoke(null, source, offset, ct.runtimeClass.asInstanceOf[Class[A]]).asInstanceOf[A]

  private val encodeMethod = {
    val m = classOf[TransactionEncoder].getDeclaredMethod("encode", classOf[RawTransaction], classOf[SignatureData])
    m.setAccessible(true)
    m
  }

  private def encodeTransaction(tx: RawTransaction, signatureData: SignatureData): Array[Byte] =
    encodeMethod.invoke(null, tx, signatureData).asInstanceOf[Array[Byte]]

  def apply(bytes: Array[Byte]): EthereumTransaction =
    apply(TransactionDecoder.decode(EthEncoding.toHexString(bytes)).asInstanceOf[SignedRawTransaction])

  val ERC20TransferPrefix: String = "a9059cbb"

  private def extractPayload(underlying: RawTransaction): Payload = {
    val hexData          = EthEncoding.cleanHexPrefix(underlying.getData)
    val recipientAddress = ByteStr(EthEncoding.toBytes(underlying.getTo))
    if (hexData.isEmpty) {
      Transfer(
        Left(Asset.Waves),
        underlying.getValue.divide(BigInt(AmountMultiplier).bigInteger).longValueExact(),
        Address(recipientAddress.arr)
      )
    } else if (hexData.startsWith(ERC20TransferPrefix)) {
      val recipient = decode[EthAddress](hexData, 8)
      val amount    = decode[Uint256](hexData, 72)
      Transfer(
        Right(ERC20Address(recipientAddress)),
        amount.getValue.longValueExact(),
        Address(EthEncoding.toBytes(recipient.toString))
      )
    } else Invocation(Address(recipientAddress.arr), hexData)
  }

  def apply(underlying: RawTransaction): EthereumTransaction =
    new EthereumTransaction(
      extractPayload(underlying),
      underlying,
      new SignatureData(Array.emptyByteArray, Array.emptyByteArray, Array.emptyByteArray),
      AddressScheme.current.chainId
    )

  def apply(underlying: SignedRawTransaction): EthereumTransaction =
    new EthereumTransaction(
      extractPayload(underlying),
      underlying,
      underlying.getSignatureData,
      underlying.getChainId.toByte
    )
}
