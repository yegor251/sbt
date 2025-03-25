package serialization

import java.io._
import java.time.Instant
import java.math.BigInteger
import java.util.LinkedList
import _root_.gameClasses.ItemManager

object SHelper {
  def writeString(out: DataOutputStream, str: String): Unit = {
    if (str != null) {
      out.writeBoolean(true)
      out.writeUTF(str)
    } else {
      out.writeBoolean(false)
    }
  }

  def readString(in: DataInputStream): String = {
    if (in.readBoolean()) in.readUTF() else null
  }

  def writeOptionInt(out: DataOutputStream, value: Option[Int]): Unit = {
    out.writeBoolean(value.isDefined)
    value.foreach(out.writeInt)
  }

  def readOptionInt(in: DataInputStream): Option[Int] = {
    if (in.readBoolean()) Some(in.readInt()) else None
  }

  def writeOptionLong(out: DataOutputStream, value: Option[Long]): Unit = {
    out.writeBoolean(value.isDefined)
    value.foreach(out.writeLong)
  }

  def readOptionLong(in: DataInputStream): Option[Long] = {
    if (in.readBoolean()) Some(in.readLong()) else None
  }

  def writeInstant(out: DataOutputStream, instant: Instant): Unit = {
    out.writeBoolean(!(instant == null))
    if (!(instant == null))
    {
      out.writeLong(instant.getEpochSecond())
      out.writeInt(instant.getNano())
    }
  }

  def readInstant(in: DataInputStream): Instant = {
    if (in.readBoolean()) Instant.ofEpochSecond(in.readLong(), in.readInt())
    else null
  }

  def writeBigInteger(out: DataOutputStream, value: BigInteger): Unit = {
    val byteArray = value.toByteArray
    out.writeInt(byteArray.length)
    out.write(byteArray)
  }

  def readBigInteger(in: DataInputStream): BigInteger = {
    val length = in.readInt()
    val byteArray = new Array[Byte](length)
    in.readFully(byteArray)
    new BigInteger(byteArray)
  }

  def writeMapString(map: Map[String, Int], dos: DataOutputStream): Unit = {
    dos.writeInt(map.size)
    for ((key, value) <- map) {
      writeString(dos, key)
      dos.writeInt(value)
    }
  }

  def readMapString(dis: DataInputStream): Map[String, Int] = {
    val size = dis.readInt()
    val map = scala.collection.mutable.Map[String, Int]()
    var left = size;
    while (left > 0)
    {
      val key = readString(dis)
      val value = dis.readInt()
      map += (key -> value)
      left -= 1;
    }
    map.toMap
  }



  def writeSpinSlotsArray(dos: DataOutputStream, spin: sSpin): Unit = {
    val array = spin.items
    val size = array.size
    dos.writeInt(size)
    var index = 0
    while (index < size) {
      writeString(dos, array(index)._1)
      dos.writeInt(array(index)._2)
      index += 1
    }
  }


  def readSpinSlotsArray(dis: DataInputStream): Array[(String, Int)] = {
    val size = dis.readInt()
    val array = new Array[(String, Int)](size)

    var index = 0
    while (index < size) {
      val _s = readString(dis)
      val _i = dis.readInt()
      array.update(index, (_s, _i))
      index += 1
    }

    array
  }

  def writeOrder(dos: DataOutputStream, order: sOrder) = {
    dos.writeBoolean(order.completed) // completed
    writeMapString(order.items, dos) // items
    dos.writeInt(order.price) // price
    writeInstant(dos, order.startTimeStamp) // start time stamp
    dos.writeInt(order.tokenPrice) // tokenPrice
  }

  def readOrder(dis: DataInputStream): sOrder = {
    val completed = dis.readBoolean()
    val items = readMapString(dis)
    val price = dis.readInt()
    val startTimeStamp = readInstant(dis)
    val tokenPrice = dis.readInt()

    sOrder(items, price, tokenPrice, completed, startTimeStamp)
  }

  def writeBusiness(dos: DataOutputStream, business: sBusiness) = {
    dos.writeInt(business.id)
    dos.writeInt(business.lvl)
    business.activateINFO match
      case Some(v) => {
        dos.writeBoolean(true)
        writeInstant(dos, v._1)
        dos.writeInt(v._2)
      }
      case None => {
        dos.writeBoolean(false)
      }
  }

  def readBusiness(dis: DataInputStream): sBusiness = {
    val id = dis.readInt()
    val lvl = dis.readInt()
    val active = dis.readBoolean()

    val activateInfo: Option[(Instant, Int)] = active match
      case true => Some(readInstant(dis), dis.readInt())
      case false => None

    sBusiness(id, lvl, activateInfo)
  }

  def writeSlot(dos: DataOutputStream, slot: sSlot) = {
    writeString(dos, slot.name)
    writeInstant(dos, slot.workStartTimeStamp)
    writeInstant(dos, slot.workEndTimeStamp)
  }

  def readSlot(dis: DataInputStream): sSlot = {
    sSlot(readString(dis), readInstant(dis), readInstant(dis))
  }

  def writeBakery(dos: DataOutputStream, bakery: sBakery) = {
    dos.writeByte(1)
    writeString(dos, bakery.bakeryType)
    dos.writeInt(bakery.curlevel)
    dos.writeInt(bakery.lastEmptySlot)
    writeInstant(dos, bakery.lastEndTime)
    dos.writeInt(bakery.purchasedSlots)
    bakery.slots.forEach((slot) => writeSlot(dos, slot))
  }

  def readBakery(dis: DataInputStream): sBakery = {
    val btype = readString(dis)
    val curlevel = dis.readInt()
    val lastEmptySlot = dis.readInt()
    val lastEndTime = readInstant(dis)
    val purchasedSlots = dis.readInt()

    var slots = new LinkedList[sSlot]
    var index = 0
    while (index < lastEmptySlot) {
      val slot = readSlot(dis)
      slots.add(slot)
      index += 1
    }

    sBakery(btype, curlevel, slots, lastEmptySlot, purchasedSlots, lastEndTime)
  }

  def writeBush(dos: DataOutputStream, bush: sBush) = {
    dos.writeByte(2)
    writeString(dos, bush.bushType)
    dos.writeInt(bush.collectedAmount)
    writeInstant(dos, bush.lastTimeCollected)
    writeInstant(dos, bush.readyTimeStamp)
  }

  def readBush(dis: DataInputStream): sBush = {
    val btype = readString(dis)
    val collectedAmount = dis.readInt()
    val lastTimeCollected = readInstant(dis)
    val readyTimeStamp = readInstant(dis)

    sBush(btype, lastTimeCollected, readyTimeStamp, collectedAmount)
  }

  def writeCorral(dos: DataOutputStream, corral: sCorral) = {
    dos.writeByte(3)
    dos.writeInt(corral.animalAmount)
    writeString(dos, corral.corralType)
    dos.writeInt(corral.curlevel)
    writeInstant(dos, corral.workStartTimeStamp)
    writeInstant(dos, corral.workEndTimeStamp)
  }
  
  def readCorral(dis: DataInputStream): sCorral = {
    val animalCount = dis.readInt()
    val cname = readString(dis)
    val curlevel = dis.readInt()
    val workStartTimeStamp = readInstant(dis)
    val workEndTimeStamp = readInstant(dis)

    sCorral(cname, curlevel, workStartTimeStamp, workEndTimeStamp, animalCount)
  }

  def writeGarden(dos: DataOutputStream, garden: sGarden) = {
    dos.writeByte(4)
    writeString(dos, garden.curplant)
    writeInstant(dos, garden.plantTimeStamp)
    writeInstant(dos, garden.growTimeStamp)
  }

  def readGarden(dis: DataInputStream): sGarden = {
    val curplant = readString(dis)
    val plantTimeStamp = readInstant(dis)
    val growTimeStamp = readInstant(dis)

    sGarden(curplant, plantTimeStamp, growTimeStamp)
  }

  def writeObstacle(dos: DataOutputStream, obstacle: sObstacle) = {
    dos.writeByte(5)
    writeString(dos, obstacle.name)
  }

  def readObstacle(dis: DataInputStream): sObstacle = {
    sObstacle(readString(dis));
  }

  def writeBuildable(dos: DataOutputStream, buildable: sBuildable) = {
    buildable match
      case _b: sBakery => writeBakery(dos, _b)
      case _b: sBush => writeBush(dos, _b)
      case _b: sCorral => writeCorral(dos, _b)
      case _b: sGarden => writeGarden(dos, _b)
      case _b: sObstacle => writeObstacle(dos, _b)
  }

  def readBuildable(dis: DataInputStream): sBuildable = {
    val buildingType = dis.readByte().toInt
    buildingType match
      case 1 => readBakery(dis)
      case 2 => readBush(dis)
      case 3 => readCorral(dis)
      case 4 => readGarden(dis)
      case 5 => readObstacle(dis)
  }

  def writeOccupiedBooleanArray(dos: DataOutputStream, tilesArray: Array[Array[sTile]]) = {
    val byteArray = Array.fill[Byte]((tilesArray.size * tilesArray(0).size) / 8)(0)

    var byteIndex = 0
    var bitIndex = 0

    dos.writeInt(tilesArray.size)
    dos.writeInt(tilesArray(0).size)
    for (row <- tilesArray) {
      for (tile <- row) {
        if (tile.occupied) {
          byteArray(byteIndex) = (byteArray(byteIndex) | (1 << (7 - bitIndex))).toByte
        }
        bitIndex += 1
        if (bitIndex == 8) {
          bitIndex = 0
          byteIndex += 1
        }
      }
    }

    dos.write(byteArray)
  }

  def readOccupiedBooleanArray(dis: DataInputStream): Array[Array[Boolean]] = {
    val sizex = dis.readInt()
    val sizey = dis.readInt()

    val matrix = Array.ofDim[Boolean](sizex, sizey)
    var byteIndex = 0
    var bitIndex = 0
    val byteArray = Array.fill[Byte]((sizex * sizey + 7) / 8)(0)

    dis.read(byteArray)

    for (i <- 0 until sizex) {
      for (j <- 0 until sizey) {
        matrix(i)(j) = (byteArray(byteIndex) & (1 << (7 - bitIndex))) != 0
        bitIndex += 1
        if (bitIndex == 8) {
          bitIndex = 0
          byteIndex += 1
        }
      }
    }

    matrix
  }


  def writeDeposit(dos: DataOutputStream, deposit: sDeposit) = {
    dos.writeBoolean(deposit.active)
    writeBigInteger(dos, deposit.amount)
    writeString(dos, deposit.commentary)
    writeString(dos, deposit.jetton_signature)
    writeBigInteger(dos, deposit.tg_id)
    writeBigInteger(dos, deposit.time_stamp)
    writeBigInteger(dos, deposit.transaction_id)
  }

  def readDeposit(dis: DataInputStream): sDeposit = {
    val active = dis.readBoolean()
    val amount = readBigInteger(dis)
    val commentary = readString(dis)
    val jetton_signature = readString(dis)
    val tg_id = readBigInteger(dis)
    val time_stamp = readBigInteger(dis)
    val transaction_id = readBigInteger(dis)

    sDeposit(transaction_id, active, tg_id, amount, time_stamp, jetton_signature, commentary)
  }

  def writeWithdraw(dos: DataOutputStream, wt: sWithdraw) = {
    writeBigInteger(dos, wt.amount)
    writeBigInteger(dos, wt.tg_id)
    writeBigInteger(dos, wt.time_stamp)
    writeBigInteger(dos, wt.transaction_id)
    dos.writeShort(wt.status)
    writeString(dos, wt.wallet)
  }

  def readWithdraw(dis: DataInputStream): sWithdraw = {
    val amount = readBigInteger(dis)
    val tg_id = readBigInteger(dis)
    val time_stamp = readBigInteger(dis)
    val transaction_id = readBigInteger(dis)
    val status = dis.readShort()
    val wallet = readString(dis)

    sWithdraw(transaction_id, status, tg_id, wallet, amount, time_stamp)
  }
}