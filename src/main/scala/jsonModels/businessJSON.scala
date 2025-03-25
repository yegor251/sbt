package jsonModels

import zio._
import zio.json._
import gameClasses.Player
import gameClasses.Business
import java.time.Instant

//case class BusinessINFO(price: Double, profitPerCoin: Double, maxCapacity: Double)

case class BusinessJSON(id: Int, lvl: Int, activateData: Option[(Long, Int)])
object BusinessJSON
{
    def fromBusiness(business: Business): BusinessJSON = {
        BusinessJSON(business.ID, business.lvl, activateInfoFromBusiness(business))
    }

    def activateInfoFromBusiness(business: Business): Option[(Long, Int)] = {
        business.activateINFO match
            case Some(b) => Some(b._1.getEpochSecond(), b._2)
            case None => None
    }

    def fromPlayer(player: Player): Array[Option[BusinessJSON]] = {
        player.Businesses.businesses.map((business) => {
            business match
                case Some(b) => Some(fromBusiness(b))
                case None => None
        })
    }

    implicit val encoder: JsonEncoder[BusinessJSON] = DeriveJsonEncoder.gen[BusinessJSON]
}