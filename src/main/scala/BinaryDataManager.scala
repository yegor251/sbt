import java.io._
import _root_.serialization._

object BinaryDataManager {
  def writeGame(game: Game, username: String) =
    {
        try {
            val writeGame = convertGame(game);
            BSerializer.writeGame(writeGame);
        } catch {
            case e: Exception => e.printStackTrace()
        }
    }
  def readGame(username: String) : Game =
    {
        try {
            val readgame = BSerializer.readGame(username)
            convertToGame(readgame)
        } catch {
            case e: Exception => 
                throw(e)
                null
        }
    }
}
