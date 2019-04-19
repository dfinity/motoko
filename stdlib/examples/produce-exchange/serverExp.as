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
  #add : AddReq ;
} ;

/**
 AddReq
 --------------------------------------
 a server `Add` request, as an AS datatype
*/
type AddReq = {
  #user        : UserInfo ;
  #truckType   : TruckTypeInfo ;
  #region      : RegionInfo ;
  #produce     : ProduceInfo ;
  #producer    : ProducerInfo ;
  #retailer    : RetailerInfo ;
  #transporter : TransporterInfo ;
  #inventory   : InventoryInfo ;
  #route       : RouteInfo ;
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


