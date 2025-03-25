object GE{
	val OK = 200
	val Regeneration = 201
	val Connected = 202

	// Ошибки недопустимые и потенциально угрожающие работе сервера
	val InternalClassMatchingError = -1
	val InternalServerLogicError = -2
	val PaymentError = -3

	// Ошибки отказа работы клиента
	val SocketUnknown = -101
	val SocketWrongFormat = -201

	// Ошибки работы логики клиента
	val NotEnoughMoney = -1001
	val GameEventOutOfBoundaries = -1102
	val NotAbleToPlace = -1003
	val PlantNotGrown = -1004
	val PlantNotPlanted = -1105
	val CannotUseBuilding = -1006
	val NotEnoughBuildingConditions = -1007
	val GardenNotPlaced = -1008
	val BuildingNotPlaced = -1009
	val InvalidAmount = -1010
	val WorkNotReady = -1011
	val WorkNotStarted = -1012
	val SpinNotGenerated = -1013
	val SpinNotEnoughItems = -1014
	val OrdersNotEnoughItems = -1015
	val OrdersNotReady = -1016
	val OrderAlreadyCompleted = -1017
	val OrderNotEnoughItemsToComplete = -1018
	val OrdersNotEnoughRerolls = -1019
	val CorralAlreadyMaxAnimals = -1020
	val BushExpired = -1021
	val BushNotExpired = -1022
	val BuildingAlreadyMaxxedUp = -1023
	val DepositAlreadyActivated = -1024
	val DealHasBeenAlreadyBought = -1025
	val NoSpaceInInventory = -1026
	val BuildingLimitExceeded = -1027
	val BusinessAlreadyActive = -1028
	val BusinessDoesntExist = -1029
	val BusinessAlreadyPurchased = -1029
	val CantMoveObstacle = -1030 

	// Коды операций регенерации
	val SpinNotReady = -10001
}