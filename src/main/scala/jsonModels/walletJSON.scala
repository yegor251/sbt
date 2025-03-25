package jsonModels

import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json

import _root_.gameClasses.Player

case class WalletJSON(usdtBalance: Long, tonBalance: Long, tokenBalance: Long)
object WalletJSON
{
	def fromPlayer(player: Player): WalletJSON = 
	{
		val wallet = player.Wallet;

		WalletJSON(wallet.usdtBalance, wallet.tonBalance, wallet.tokenBalance)
	}

	implicit val gameDataEncoder: JsonEncoder[WalletJSON] = DeriveJsonEncoder.gen[WalletJSON]
}