package jsonModels
import _root_.gameClasses._
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import zio.json._

case class DepositJSON(transaction_id: Long, var active: Boolean, tg_id: Long, amount: Long, time_stamp: Long, jetton_signature: String, commentary: String)
object DepositJSON {
  implicit val encoder: JsonEncoder[DepositJSON] = DeriveJsonEncoder.gen[DepositJSON]
}
case class WithdrawJSON(transaction_id: Long, var status: Short, tg_id: Long, wallet: String, amount: Long, time_stamp: Long)
object WithdrawJSON {
  implicit val encoder: JsonEncoder[WithdrawJSON] = DeriveJsonEncoder.gen[WithdrawJSON]
}

object transaction_infoJSON
{
    def fromDeposits(deposits: Array[Deposit]): Array[DepositJSON] =
    {
        deposits.map((dep) => {
            DepositJSON(dep.transaction_id.longValue(), dep.active, dep.tg_id.longValue(), dep.amount.longValue(), dep.time_stamp.longValue(), dep.jetton_signature, dep.commentary)
        })
    }
    
    def fromWithdraws(withdraws: Array[Withdraw]): Array[WithdrawJSON] =
    {
        withdraws.map((w) => 
        {
            WithdrawJSON(w.transaction_id.longValue(), w.status, w.tg_id.longValue(), w.wallet, w.amount.longValue(), w.time_stamp.longValue())
        })
    }
}