package jsonModels

import zio._
import zio.json._
import gameClasses.Player
import gameClasses.Boosters


case class ActiveBoosterJSON(percentage: Int, time: Int, activateTimeStamp: Long)
object ActiveBoosterJSON
{
	def fromBooster(booster: Option[Boosters]): Option[ActiveBoosterJSON] = { booster match
		case Some(_b) => Some(ActiveBoosterJSON(_b.percentage, _b.time, _b.activateTimeStamp.getEpochSecond()))
		case None 		=> None
	}

	implicit val encoder: JsonEncoder[ActiveBoosterJSON] = DeriveJsonEncoder.gen[ActiveBoosterJSON]
}

case class ActiveBoostersJSON(OrderMoney: Option[ActiveBoosterJSON], OrderItems: Option[ActiveBoosterJSON],
															WorkSpeed: Option[ActiveBoosterJSON], GrowSpeed: Option[ActiveBoosterJSON])
object ActiveBoostersJSON
{
	def fromPlayer(player: Player): ActiveBoostersJSON = {
		val _b = player.ActiveBoosters
		ActiveBoostersJSON(	ActiveBoosterJSON.fromBooster(_b.OrderMoney), 
												ActiveBoosterJSON.fromBooster(_b.OrderItems), 
												ActiveBoosterJSON.fromBooster(_b.WorkSpeed), 
												ActiveBoosterJSON.fromBooster(_b.GrowSpeed) )
	}
	implicit val encoder: JsonEncoder[ActiveBoostersJSON] = DeriveJsonEncoder.gen[ActiveBoostersJSON]
}