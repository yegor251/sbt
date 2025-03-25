package serialization

import _root_.gameClasses._
import java.io._
import java.nio.ByteBuffer
import java.nio.file.{Files, Paths}
import java.time.Instant
import java.math.BigInteger
import _root_.`<empty>`.ENVars

object BSerializer {

//                  ▄▄ •  ▄▄▄· • ▌ ▄ ·. ▄▄▄ .                      
//                 ▐█ ▀ ▪▐█ ▀█ ·██ ▐███▪▀▄.▀·                      
//                 ▄█ ▀█▄▄█▀▀█ ▐█ ▌▐▌▐█·▐▀▀▪▄                      
//                 ▐█▄▪▐█▐█ ▪▐▌██ ██▌▐█▌▐█▄▄▌                      
//                 ·▀▀▀▀  ▀  ▀ ▀▀  █▪▀▀▀ ▀▀▀                       
// .▄▄ · ▄▄▄ .▄▄▄  ▪   ▄▄▄· ▄▄▌  ▪  ·▄▄▄▄• ▄▄▄· ▄▄▄▄▄▪         ▐ ▄ 
// ▐█ ▀. ▀▄.▀·▀▄ █·██ ▐█ ▀█ ██•  ██ ▪▀·.█▌▐█ ▀█ •██  ██ ▪     •█▌▐█
// ▄▀▀▀█▄▐▀▀▪▄▐▀▀▄ ▐█·▄█▀▀█ ██▪  ▐█·▄█▀▀▀•▄█▀▀█  ▐█.▪▐█· ▄█▀▄ ▐█▐▐▌
// ▐█▄▪▐█▐█▄▄▌▐█•█▌▐█▌▐█ ▪▐▌▐█▌▐▌▐█▌█▌▪▄█▀▐█ ▪▐▌ ▐█▌·▐█▌▐█▌.▐▌██▐█▌
//  ▀▀▀▀  ▀▀▀ .▀  ▀▀▀▀ ▀  ▀ .▀▀▀ ▀▀▀·▀▀▀ • ▀  ▀  ▀▀▀ ▀▀▀ ▀█▄▀▪▀▀ █▪


    def writeGame(game: sGame) = {
        val dir = s"sessions/${game.CLIENT_INFO.tgID}"
        new java.io.File(dir).mkdirs()
        
        writePlayer(game.player, dir)
        writeWorld(dir, game.world)
        writeTransferInfo(dir, game.TRANSFERS_INFO)
        writeClientInfo(dir, game.CLIENT_INFO)
    }

    def readGame(tgID: String): sGame = {
        val dir = s"sessions/${tgID}"

        val player = readPlayer(dir)
        val world = readWorld(dir)
        val TransferInfo = readTransferInfo(dir)
        val ClientInfo = readClientInfo(dir)

        sGame(player, world, TransferInfo, ClientInfo, 200)
    }

    def writeTransferInfo(dir: String, TransferInfo: sTransferInfo) = {
        val file = new java.io.File(s"$dir/game")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/transfer-info.bin")
        val dos = new DataOutputStream(fos)

        dos.writeInt(TransferInfo.deposits.size)
        for (dep <- TransferInfo.deposits) {
            SHelper.writeDeposit(dos, dep)
        }
        dos.writeInt(TransferInfo.withdraws.size)
        TransferInfo.withdraws.foreach((wt) => {
            SHelper.writeWithdraw(dos, wt)
        })

        dos.close()
        fos.close()
    }

    def readTransferInfo(dir: String): sTransferInfo = {
        val fis = new FileInputStream(s"${dir}/game/transfer-info.bin")
        val dis = new DataInputStream(fis)

        val sized = dis.readInt()
        val arrayd = new Array[sDeposit](sized)
        for (i <- 0 until sized) {
            arrayd.update(i, SHelper.readDeposit(dis))
        }

        val sizew = dis.readInt()
        val arrayw = new Array[sWithdraw](sizew)
        for (i <- 0 until sizew) {
            arrayw.update(i, SHelper.readWithdraw(dis))
        }
        

        dis.close()
        fis.close()

        sTransferInfo(arrayd, arrayw)
    }

    def writeClientInfo(dir: String, ClientInfo: sClientInfo) = {
        val file = new java.io.File(s"$dir/game")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/client-info.bin")
        val dos = new DataOutputStream(fos)

        dos.writeBoolean(ClientInfo.banned)
        dos.writeLong(ClientInfo.tgID)
        dos.writeLong(ClientInfo.refID)
        SHelper.writeString(dos, ClientInfo.wallet)
        dos.writeInt(ClientInfo.strikes)
        dos.writeInt(ClientInfo.referrals.size)
        ClientInfo.referrals.foreach((el) => {
            dos.writeLong(el)
        })

        dos.close()
        fos.close()
    }

    def readClientInfo(dir: String): sClientInfo = {
        val fis = new FileInputStream(s"${dir}/game/client-info.bin")
        val dis = new DataInputStream(fis)

        val banned = dis.readBoolean()
        val tgId = dis.readLong()
        val refID = dis.readLong()
        val wallet = SHelper.readString(dis)
        val strikes = dis.readInt()
        val size = dis.readInt()
        var array: Array[Long] = Array.empty;
        for (i <- 0 until size) {
            array = array.appended(dis.readLong())
        }

        dis.close()
        fis.close()

        sClientInfo(tgId, refID, wallet, banned, strikes, array)
    }


//                  ▄▄▄·▄▄▌   ▄▄▄·  ▄· ▄▌▄▄▄ .▄▄▄                  
//                 ▐█ ▄███•  ▐█ ▀█ ▐█▪██▌▀▄.▀·▀▄ █·                
//                  ██▀·██▪  ▄█▀▀█ ▐█▌▐█▪▐▀▀▪▄▐▀▀▄                 
//                 ▐█▪·•▐█▌▐▌▐█ ▪▐▌ ▐█▀·.▐█▄▄▌▐█•█▌                
//                 .▀   .▀▀▀  ▀  ▀   ▀ •  ▀▀▀ .▀  ▀                
// .▄▄ · ▄▄▄ .▄▄▄  ▪   ▄▄▄· ▄▄▌  ▪  ·▄▄▄▄• ▄▄▄· ▄▄▄▄▄▪         ▐ ▄ 
// ▐█ ▀. ▀▄.▀·▀▄ █·██ ▐█ ▀█ ██•  ██ ▪▀·.█▌▐█ ▀█ •██  ██ ▪     •█▌▐█
// ▄▀▀▀█▄▐▀▀▪▄▐▀▀▄ ▐█·▄█▀▀█ ██▪  ▐█·▄█▀▀▀•▄█▀▀█  ▐█.▪▐█· ▄█▀▄ ▐█▐▐▌
// ▐█▄▪▐█▐█▄▄▌▐█•█▌▐█▌▐█ ▪▐▌▐█▌▐▌▐█▌█▌▪▄█▀▐█ ▪▐▌ ▐█▌·▐█▌▐█▌.▐▌██▐█▌
//  ▀▀▀▀  ▀▀▀ .▀  ▀▀▀▀ ▀  ▀ .▀▀▀ ▀▀▀·▀▀▀ • ▀  ▀  ▀▀▀ ▀▀▀ ▀█▄▀▪▀▀ █▪

    def readPlayer(dir: String): sPlayer = {
        val fis = new FileInputStream(s"${dir}/player/info.bin")
        val dis = new DataInputStream(fis)

        val money = dis.readInt()
        val netWorth = dis.readInt()

        dis.close()
        fis.close()

        val Inventory = readInventory(dir)
        val Spin = readSpin(dir)
        val Orders = readOrders(dir)
        val Businesses = readBusinesses(dir)
        val Stats = readStats(dir)
        val Deals = readDeals(dir)
        val ActiveBoosters = readActiveBoosters(dir)
        val Wallet = readWallet(dir)

        sPlayer(Inventory, money, netWorth, Spin, Orders, Businesses, Stats, Deals, ActiveBoosters, Wallet)
    }

    def readInventory(dir: String): sItemsContainer = {
        val fis = new FileInputStream(s"${dir}/player/inventory.bin")
        val dis = new DataInputStream(fis)

        var map = SHelper.readMapString(dis)
        map = map.filter((el, am) => ItemManager.isLegalItem(el))
        val capacity = dis.readInt()
        val itemsAmount = dis.readInt()
        val level = dis.readInt()

        dis.close()
        fis.close()

        sItemsContainer(map, itemsAmount, 70 + level * 10, level)
    }

    def readSpin(dir: String): sSpin = {
        val fis = new FileInputStream(s"${dir}/player/spin.bin")
        val dis = new DataInputStream(fis)

        val spin = SHelper.readSpinSlotsArray(dis)
        val dropName = SHelper.readString(dis)
        val dropAmount = dis.readInt()
        val activated = dis.readBoolean()
        val generateTimeStamp = SHelper.readInstant(dis)

        dis.close()
        fis.close()

        sSpin(spin, (dropName, dropAmount), activated, generateTimeStamp)
    }

    def readOrders(dir: String): Array[sOrder] = {
        val fis = new FileInputStream(s"${dir}/player/orders.bin")
        val dis = new DataInputStream(fis)

        val size = dis.readInt()
        val array = new Array[sOrder](size)
        var index = 0
        while (index < size) {
            val order = SHelper.readOrder(dis)
            array.update(index, order)
            index += 1
        }   

        dis.close()
        fis.close()

        array
    }

    def readBusinesses(dir: String): Array[Option[sBusiness]] = {
        val fis = new FileInputStream(s"${dir}/player/businesses.bin")
        val dis = new DataInputStream(fis)

        val size = dis.readInt()
        val businesses = new Array[Option[sBusiness]](size)
        var index = 0
        while(index < size)
        {
            val option = dis.readBoolean()
            option match
                case true => businesses.update(index, Some(SHelper.readBusiness(dis)))
                case false => businesses.update(index, None)

            index+=1
        }

        dis.close()
        fis.close()

        businesses
    }

    def readStats(dir: String): sStats = {
        val fis = new FileInputStream(s"${dir}/player/stats.bin")
        val dis = new DataInputStream(fis)

        val ordersCompleted = dis.readInt()
        val buildings = SHelper.readMapString(dis)

        var buildingsPlaced = BuildingManager.typeByName.keySet.map(building => building -> BuildingManager.getMapLimit(building)).toMap
        for ((s, i) <- buildings)
        {
            buildingsPlaced = buildingsPlaced.updated(s, i)
        }

        dis.close()
        fis.close()

        sStats(ordersCompleted, buildingsPlaced)
    }

    def writePlayer(player: sPlayer, dir: String) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/info.bin")
        val dos = new DataOutputStream(fos)

        dos.writeInt(player.money)
        dos.writeInt(player.netWorth)

        dos.close()
        fos.close()

        writeInventory(player, dir)
        if (player.spin != null) writeSpin(dir, player)
        if (player.orders != null) writeOrders(dir, player)
        writeBusinesses(dir, player)
        writeStats(dir, player)
        writeDeals(dir, player)
        writeActiveBoosters(dir, player)
        writeWallet(dir, player)
    }

    def writeInventory(player: sPlayer, dir: String) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/inventory.bin")
        val dos = new DataOutputStream(fos)

        SHelper.writeMapString(player.Inventory.items, dos)
        dos.writeInt(player.Inventory.capacity)
        dos.writeInt(player.Inventory.itemsAmount)
        dos.writeInt(player.Inventory.level)

        dos.close()
        fos.close()
    }

    def writeSpin(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/spin.bin")
        val dos = new DataOutputStream(fos)

        SHelper.writeSpinSlotsArray(dos, player.spin)
        SHelper.writeString(dos, player.spin.drop._1)
        dos.writeInt(player.spin.drop._2)
        dos.writeBoolean(player.spin.activated)
        SHelper.writeInstant(dos, player.spin.generateTimeStamp)

        dos.close()
        fos.close()
    }

    def writeOrders(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/orders.bin")
        val dos = new DataOutputStream(fos)

        val size = player.orders.size
        dos.writeInt(size);
        player.orders.foreach((order) => 
            SHelper.writeOrder(dos, order)
        )
        
        dos.close()
        fos.close()
    }

    def writeBusinesses(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/businesses.bin")
        val dos = new DataOutputStream(fos)

        val businesses = player.Businesses

        dos.writeInt(businesses.size)

        for (b <- businesses){
            b match
                case Some(_b) => {
                    dos.writeBoolean(true)
                    SHelper.writeBusiness(dos, _b)
                }
                case None => {
                    dos.writeBoolean(false)
                }
        }

        dos.close()
        fos.close()
    }

    def writeStats(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/stats.bin")
        val dos = new DataOutputStream(fos)

        dos.writeInt(player.Stats.ordersCompleted)
        SHelper.writeMapString(player.Stats.buildingsPlaced, dos)

        dos.close()
        fos.close()
    }

    def writeDeals(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/deals.bin")
        val dos = new DataOutputStream(fos)

        val sizeD = player.Deals.boughtDeals.size
        dos.writeInt(sizeD)
        for (s <- player.Deals.boughtDeals)  {
            SHelper.writeString(dos, s)
        }

        val sizeB = player.Deals.boosters.size
        dos.writeInt(sizeB)
        for (b <- player.Deals.boosters) {
            SHelper.writeString(dos, b.boosterType)
            dos.writeInt(b.percentage)
            dos.writeInt(b.time)
        }

        dos.writeLong(player.Deals.tokenSpent)
        dos.writeLong(player.Deals.tonSpent)
        dos.writeLong(player.Deals.usdtSpent)

        dos.close()
        fos.close()
    }

    def readDeals(dir: String): sDeals = {
        val fis = new FileInputStream(s"${dir}/player/deals.bin")
        val dis = new DataInputStream(fis)

        val sizeD = dis.readInt()
        var index = 0
        var set: Set[String] = Set.empty
        while (index < sizeD) {
            set = set.incl(SHelper.readString(dis))
            index+=1
        }
        
        index = 0;
        val sizeB = dis.readInt()
        val array: Array[sBoosterJSON] = new Array[sBoosterJSON](sizeB)
        while (index < sizeB) {
            val btype = SHelper.readString(dis)
            val percentage = dis.readInt()
            val time = dis.readInt()
            array.update(index, sBoosterJSON(btype, percentage, time))
            index+=1
        }

        val tokenSpent = dis.readLong()
        val tonSpent = dis.readLong()
        val usdtSpent = dis.readLong()

        dis.close()
        fis.close()

        sDeals(set, array, tonSpent, usdtSpent, tokenSpent)
    }

    def writeActiveBoosters(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/active-boosters.bin")
        val dos = new DataOutputStream(fos)

        player.ActiveBoosters.GrowSpeed match
            case None => dos.writeBoolean(false)
            case Some(b) => {
                dos.writeBoolean(true)
                dos.writeInt(b.percentage)
                dos.writeInt(b.time)
                SHelper.writeInstant(dos, b.activateTimeStamp)
            }
        player.ActiveBoosters.OrderItems match
            case None => dos.writeBoolean(false)
            case Some(b) => {
                dos.writeBoolean(true)
                dos.writeInt(b.percentage)
                dos.writeInt(b.time)
                SHelper.writeInstant(dos, b.activateTimeStamp)
            }
        player.ActiveBoosters.OrderMoney match
            case None => dos.writeBoolean(false)
            case Some(b) => {
                dos.writeBoolean(true)
                dos.writeInt(b.percentage)
                dos.writeInt(b.time)
                SHelper.writeInstant(dos, b.activateTimeStamp)
            }
        player.ActiveBoosters.WorkSpeed match
            case None => dos.writeBoolean(false)
            case Some(b) => {
                dos.writeBoolean(true)
                dos.writeInt(b.percentage)
                dos.writeInt(b.time)
                SHelper.writeInstant(dos, b.activateTimeStamp)
            }

        dos.close()
        fos.close()
    }

    def readActiveBoosters(dir: String): sActiveBoosters = {
        val fis = new FileInputStream(s"${dir}/player/active-boosters.bin")
        val dis = new DataInputStream(fis)

        val growSpeed = dis.readBoolean() match
            case true => {
                Some(sBoosters.GrowSpeed(dis.readInt(), dis.readInt(), SHelper.readInstant(dis)))
            }
            case false => None
        val orderItems = dis.readBoolean() match
            case true => {
                Some(sBoosters.OrderItems(dis.readInt(), dis.readInt(), SHelper.readInstant(dis)))
            }
            case false => None
        val orderMoney = dis.readBoolean() match
            case true => {
                Some(sBoosters.OrderMoney(dis.readInt(), dis.readInt(), SHelper.readInstant(dis)))
            }
            case false => None
        val workSpeed = dis.readBoolean() match
            case true => {
                Some(sBoosters.WorkSpeed(dis.readInt(), dis.readInt(), SHelper.readInstant(dis)))
            }
            case false => None

        dis.close()
        fis.close()

        sActiveBoosters(orderMoney, orderItems, workSpeed, growSpeed)
    }

    def writeWallet(dir: String, player: sPlayer) = {
        val file = new java.io.File(s"$dir/player")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/wallet.bin")
        val dos = new DataOutputStream(fos)

        dos.writeLong(player.Wallet.tokenBalance)
        dos.writeLong(player.Wallet.usdtBalance)
        dos.writeLong(player.Wallet.tonBalance)

        dos.close()
        fos.close()
    }

    def readWallet(dir: String): sWallet = {
        val fis = new FileInputStream(s"${dir}/player/wallet.bin")
        val dis = new DataInputStream(fis)

        val wallet = sWallet(dis.readLong(), dis.readLong(), dis.readLong())

        dis.close()
        fis.close()

        wallet
    }




//                 ▄▄▌ ▐ ▄▌      ▄▄▄  ▄▄▌  ·▄▄▄▄                   
//                 ██· █▌▐█▪     ▀▄ █·██•  ██▪ ██                  
//                 ██▪▐█▐▐▌ ▄█▀▄ ▐▀▀▄ ██▪  ▐█· ▐█▌                 
//                 ▐█▌██▐█▌▐█▌.▐▌▐█•█▌▐█▌▐▌██. ██                  
//                  ▀▀▀▀ ▀▪ ▀█▄▀▪.▀  ▀.▀▀▀ ▀▀▀▀▀•                  
// .▄▄ · ▄▄▄ .▄▄▄  ▪   ▄▄▄· ▄▄▌  ▪  ·▄▄▄▄• ▄▄▄· ▄▄▄▄▄▪         ▐ ▄ 
// ▐█ ▀. ▀▄.▀·▀▄ █·██ ▐█ ▀█ ██•  ██ ▪▀·.█▌▐█ ▀█ •██  ██ ▪     •█▌▐█
// ▄▀▀▀█▄▐▀▀▪▄▐▀▀▄ ▐█·▄█▀▀█ ██▪  ▐█·▄█▀▀▀•▄█▀▀█  ▐█.▪▐█· ▄█▀▄ ▐█▐▐▌
// ▐█▄▪▐█▐█▄▄▌▐█•█▌▐█▌▐█ ▪▐▌▐█▌▐▌▐█▌█▌▪▄█▀▐█ ▪▐▌ ▐█▌·▐█▌▐█▌.▐▌██▐█▌
//  ▀▀▀▀  ▀▀▀ .▀  ▀▀▀▀ ▀  ▀ .▀▀▀ ▀▀▀·▀▀▀ • ▀  ▀  ▀▀▀ ▀▀▀ ▀█▄▀▪▀▀ █▪

    def writeWorld(dir: String, world: sWorld) = {
        val file = new java.io.File(s"$dir/world")
        file.mkdir()
        val fos = new FileOutputStream(file.getPath() + "/world.bin")
        val dos = new DataOutputStream(fos)

        var amount = 0;
        world.tileArray.foreach((ta) => {
            amount += ta.count((tile) => tile.place match
                case null => false
                case value => true
            )
        })

        dos.writeInt(amount)

        var x = 0
        while (x < ENVars.GAME_SETTINGS.MAP.SIZE.x) {
            var y = 0
            while (y < ENVars.GAME_SETTINGS.MAP.SIZE.y) {
                val tile = world.tileArray(x)(y)
                if (tile.place != null) {
                    dos.writeInt(x)
                    dos.writeInt(y)
                    SHelper.writeBuildable(dos, tile.place)
                }
                y+=1
            }
            x+=1
        }
        SHelper.writeOccupiedBooleanArray(dos, world.tileArray)

        dos.close()
        fos.close()
    }

    def readWorld(dir: String): sWorld= {
        val fis = new FileInputStream(s"${dir}/world/world.bin")
        val dis = new DataInputStream(fis)

        val tileArray = Array.fill(ENVars.GAME_SETTINGS.MAP.SIZE.x, ENVars.GAME_SETTINGS.MAP.SIZE.y)(sTile(false, null))

        val amount = dis.readInt()
        val buildingArray: Array[(Int, Int, sBuildable)] = new Array[(Int, Int, sBuildable)](amount)
        var index = 0
        while (index < amount) {
            val x = dis.readInt()
            val y = dis.readInt()
            val building = SHelper.readBuildable(dis)

            buildingArray.update(index, (x, y, building))
            index += 1
        }

        val boolArray = SHelper.readOccupiedBooleanArray(dis)

        for (x <- 0 until boolArray.size) {
            for (y <- 0 until boolArray(0).size) {
                if ((x < ENVars.GAME_SETTINGS.MAP.SIZE.x) && (y < ENVars.GAME_SETTINGS.MAP.SIZE.y)) {
                    tileArray(x).update(y, sTile(boolArray(x)(y), null))
                }
            }
        }

        buildingArray.foreach((el) => {
            if ((el._1 < ENVars.GAME_SETTINGS.MAP.SIZE.x) && (el._2 < ENVars.GAME_SETTINGS.MAP.SIZE.y)) {
                tileArray(el._1).update(el._2, sTile(boolArray(el._1)(el._2), el._3))
            }
        })

        dis.close()
        fis.close()

        sWorld(tileArray)
    }
}