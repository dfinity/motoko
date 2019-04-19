/**

 Server expressions
 =====================
 
 (The beginnings of) a DSL for scripting interactions with the Produce Exchange server.

*/


/**
 Req
 --------------------------------------
 a server request, as an AS datatype
*/
type Req = {
  #addUser        : UserInfo ;
  #addTruckType   : TruckTypeInfo ;
  #addRegion      : RegionInfo ;
  #addProduce     : ProduceInfo ;
  #addProducer    : ProducerInfo ;
  #addRetailer    : RetailerInfo ;
  #addTransporter : TransporterInfo ;
  #addInventory   : InventoryInfo ;
  #addRoute       : RouteInfo ;
} ;

/**
 EntId
 --------------------------------------
 An entity's ID; the response of bulk server population
*/
type EntId = {
  #user        : UserId ;
  #truckType   : TruckTypeId ;
  #region      : RegionId ;
  #produce     : ProduceId ;
  #producer    : ProducerId ;
  #retailer    : RetailerId ;
  #transporter : TransporterId ;
  #inventory   : InventoryId ;
  #route       : RouteId ;
}


