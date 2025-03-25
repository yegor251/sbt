package gameClasses

import _root_.`<empty>`.GE

class Tile extends Serializable{
    var occupied = false;
    var place: Buildable = null;
    def place(building: String): Int =
    {
        occupied = true;
        place = BuildingManager.getBuildingByName(building);
        GE.OK
    }
    def collect(): Int =
    {
        if (occupied)
        {
            place.collect()
            GE.OK
        } else GE.BuildingNotPlaced
    }
    def isOccupied(): Boolean =
    {
        occupied
    }
}
