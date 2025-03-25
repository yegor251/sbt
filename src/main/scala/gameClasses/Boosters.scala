package gameClasses


import _root_.`<empty>`.GE
import zio._
import zio.json._
import java.time.Instant

case class BoosterJSON(boosterType: String, percentage: Int, time: Int)
object BoosterJSON {
	implicit val decoder: JsonDecoder[BoosterJSON] = DeriveJsonDecoder.gen[BoosterJSON]
	implicit val encoder: JsonEncoder[BoosterJSON] = DeriveJsonEncoder.gen[BoosterJSON]
}

sealed trait Boosters extends Serializable {
  def percentage: Int
  def time: Int
  def activateTimeStamp: Instant
}

object Boosters {
  case class OrderMoney		(percentage: Int, time: Int, activateTimeStamp: Instant) extends Boosters
  case class OrderItems		(percentage: Int, time: Int, activateTimeStamp: Instant) extends Boosters
  case class WorkSpeed		(percentage: Int, time: Int, activateTimeStamp: Instant) extends Boosters
  case class GrowSpeed		(percentage: Int, time: Int, activateTimeStamp: Instant) extends Boosters
}

import Boosters._

object BoosterManager
{
	def toClass(_b: BoosterJSON): Boosters ={
		_b.boosterType match
			case "OrderMoney" 	=> OrderMoney(_b.percentage, _b.time, null)
			case "OrderItems" 	=> OrderItems(_b.percentage, _b.time, null)
			case "WorkSpeed" 		=> WorkSpeed(_b.percentage, _b.time, null)
			case "GrowSpeed" 		=> GrowSpeed(_b.percentage, _b.time, null)
			case _ 							=> null
	}

	def checkBooster(booster: Option[Boosters]): Boolean = { booster match
		case Some(_b) 	=> !(_b.activateTimeStamp.plusSeconds(_b.time).isBefore(Instant.now()))
		case None 			=> false
	}
	
	def getPercentage(booster: Option[Boosters]): Int = {
		BoosterManager.renewBooster(booster) match
			case Some(_b) => _b.percentage
			case None			=> 0
	}

	def getInfo(booster: Option[Boosters]): (Int, Int) = {
		BoosterManager.renewBooster(booster) match
			case Some(_b) => (_b.percentage, ((_b.activateTimeStamp.plusSeconds(_b.time).getEpochSecond()) - (Instant.now().getEpochSecond())).toInt)
			case None			=> (0, 0)
	}

	def renewBooster(booster: Option[Boosters]): Option[Boosters] = if (checkBooster(booster)) booster else None
	def regenerate(player: Player): Int = {
		try {	
			player.ActiveBoosters.OrderItems = renewBooster(player.ActiveBoosters.OrderItems).asInstanceOf[Option[OrderItems]]
			player.ActiveBoosters.OrderMoney = renewBooster(player.ActiveBoosters.OrderMoney).asInstanceOf[Option[OrderMoney]]
			player.ActiveBoosters.WorkSpeed = renewBooster(player.ActiveBoosters.WorkSpeed).asInstanceOf[Option[WorkSpeed]]
			player.ActiveBoosters.GrowSpeed = renewBooster(player.ActiveBoosters.GrowSpeed).asInstanceOf[Option[GrowSpeed]]
			GE.OK
		}
		catch {
			case _ => GE.InternalClassMatchingError
		}
	}

	def activate(player: Player, booster: Boosters): Int = { booster match
		case _b: OrderMoney => 		player.ActiveBoosters.OrderMoney = Some(OrderMoney(_b.percentage, _b.time, Instant.now()))
		case _b: OrderItems => 		player.ActiveBoosters.OrderItems = Some(OrderItems(_b.percentage, _b.time, Instant.now()))
		case _b: WorkSpeed 	=> 		player.ActiveBoosters.WorkSpeed = Some(WorkSpeed(_b.percentage, _b.time, Instant.now()))
		case _b: GrowSpeed 	=> 		player.ActiveBoosters.GrowSpeed = Some(GrowSpeed(_b.percentage, _b.time, Instant.now()))

		GE.OK
	}

	def activate(player: Player, index: Int): Int = {
		if (player.Deals.boosters.isDefinedAt(index)) {
			val booster = player.Deals.boosters(index);
			player.Deals.boosters = player.Deals.boosters.patch(index, Nil, 1)
			activate(player, toClass(booster))
			GE.OK
		} else GE.GameEventOutOfBoundaries
	}
}