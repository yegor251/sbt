package gameClasses

import _root_.`<empty>`.GE
import _root_.`<empty>`.ENVars
import scala.util.Random
import ENVars.ASSETS.itemlist
import java.time.{Instant, Duration}

class Spin(wheelItems: Array[(String, Int)], dropItem: (String, Int)) extends Serializable
{
  val items: Array[(String, Int)] = wheelItems;
  val drop = dropItem;
  var activated = false;
  var generateTimeStamp = Instant.now()
}

object SpinManager
{
  val random = Random;
  def regenerate(player: Player): Int =
  {
    val spin = player.spin;

    val curtime = Instant.now();
    val spintime = spin.generateTimeStamp.plusSeconds(ENVars.GAME_SETTINGS.SPIN.timeToSpin);
    if (spintime.isBefore(curtime)) {
      generateSpin(player)
    }
    else GE.SpinNotReady
  }
  def generateSpin(player: Player): Int =
  {
    val keySet = player.Inventory.pullRandom(5);
    val items = keySet.map((el) => (el, random.nextInt(6) + 1)).appended(("money", random.nextInt(41) + 10))
    player.spin = Spin(items, items(random.nextInt(6)))
    GE.OK
  }
  def spinWheel(player: Player): Int = 
  {
    val spin = player.spin;
    if (spin == null)
      GE.SpinNotGenerated
    else
    {
      if (!spin.activated)
      {
        if (!spin.generateTimeStamp.plusSeconds(ENVars.GAME_SETTINGS.SPIN.timeToSpin).isBefore(Instant.now()))
        {
          val (dropItem, dropAmount) = spin.drop
          dropItem match {
            case "money" =>
              player.money += dropAmount
            case item => player.Inventory.addAmount(item, dropAmount)
            case null => println("wtf")  // handle unexpected drop item if necessary
          }
          player.spin.activated = true;

          GE.OK
        } else GE.CannotUseBuilding
      }
      else
      {
        GE.SpinNotReady
      }
    }
  }
}
