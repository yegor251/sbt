import gameClasses._
import zio.json.JsonEncoder
import zio.json.DeriveJsonEncoder
import zio.json.ast.Json
import zio.json._
import java.time.{Instant, Duration}
import jsonModels._

case class GameJSON(dataType: String, 
                    player: PlayerJSON, 
                    world: Option[WorldJSON], 
                    deposits: Option[Array[DepositJSON]], withdraws: Option[Array[WithdrawJSON]],
                    wallet: WalletJSON,
                    availableDeals: Option[Map[String, Deal]],
                    availableBoosters: Option[Array[BoosterJSON]],
                    activeBoosters: ActiveBoostersJSON, 
                    businesses: Array[Option[BusinessJSON]], 
                    refAmount: Int)
object GameJSON {
  def fromGame(game: Game): GameJSON =
    {
      GameJSON(
          "game-session",
          PlayerJSON.fromPlayer(game.player),
          Some(WorldJSON.fromWorld(game.world)),
          Some(transaction_infoJSON.fromDeposits(game.TRANSFERS_INFO.deposits)),
          Some(transaction_infoJSON.fromWithdraws(game.TRANSFERS_INFO.withdraws)),
          WalletJSON.fromPlayer(game.player),
          Some(Shop.deals.filter((_s, _d) => !game.player.Deals.boughtDeals(_d.name))),
          Some(game.player.Deals.boosters),
          ActiveBoostersJSON.fromPlayer(game.player),
          BusinessJSON.fromPlayer(game.player),
          game.CLIENT_INFO.referrals.size
        )
    } 
  def fromGameRegen(game: Game): GameJSON =
    {
      GameJSON(
          "game-session-regen",
          PlayerJSON.fromPlayer(game.player),
          None,
          None,
          None,
          WalletJSON.fromPlayer(game.player),
          None,
          Some(game.player.Deals.boosters),
          ActiveBoostersJSON.fromPlayer(game.player),
          BusinessJSON.fromPlayer(game.player),
          game.CLIENT_INFO.referrals.size
        )
    }

  implicit val gameDataEncoder: JsonEncoder[GameJSON] = DeriveJsonEncoder.gen[GameJSON]
}

case class OperationJSON(dataType: String, code: Int)
object OperationJSON
{
  def fromCode(code: Int): OperationJSON = OperationJSON("result-code", code)
  implicit val gameDataEncoder: JsonEncoder[OperationJSON] = DeriveJsonEncoder.gen[OperationJSON]
}
