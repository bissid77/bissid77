package com.wavesplatform.transaction

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.IssuedAsset
import monix.eval.Coeval
import play.api.libs.json.JsObject

trait TransactionBase {
  def assetFee: (Asset, Long)
  def timestamp: Long
  def chainId: Byte
  def id: Coeval[ByteStr]
  def smartAssets(blockchain: Blockchain): Seq[IssuedAsset]
}

object TransactionBase {
  implicit class TBExt(val t: TransactionBase) extends AnyVal {
    def fee: Long = t.assetFee._2
    def feeAssetId: Asset = t.assetFee._1
  }
}

abstract class Transaction(val tpe: TransactionType.TransactionType, protected val checkedAssets: Seq[IssuedAsset] = Nil) extends TransactionBase {
  def bytesSize: Int         = bytes().length
  val protoSize: Coeval[Int] = Coeval(PBTransactions.protobuf(this).serializedSize)
  val bodyBytes: Coeval[Array[Byte]]
  val bytes: Coeval[Array[Byte]]
  val json: Coeval[JsObject]

  override def toString: String = json().toString

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  override def smartAssets(blockchain: Blockchain): Seq[IssuedAsset] = checkedAssets.filter(blockchain.hasAssetScript)
}

object Transaction {
  type Type = TransactionType.TransactionType

  val V1: TxVersion = TxVersion.V1
  val V2: TxVersion = TxVersion.V2
}
