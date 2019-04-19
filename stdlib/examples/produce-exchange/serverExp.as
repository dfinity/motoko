/**

 Server expressions
 =====================

 (The beginnings of) a DSL for scripting interactions with the Produce Exchange (PX) server.

 PS: This file demosntrates (one of _many_ reasons) why we aren't using GoLang for the PX.
*/


/**
 Req
 --------------------------------------
 a single PX server **request**, as an AS datatype

 ### See also:
 - [resp](#resp)
 - [bulk request](#bulkreq)
*/
type Req = {
  #add    : AddReq ;
  #update : UpdateReq ;
  #rem    : EntId ;
} ;

/**
 Resp
 --------------------------------------
 a single PX server **response**, as an AS datatype

 ### See also:
 - [request](#req)
 - [bulk response](#bulkresp)
*/
type Resp = {
  #add    : EntId ;
  #update : (?()) ;
  #rem    : (?()) ;
} ;

/**
 BulkReq
 -----------------------------------------------------------------------------------------------------------
 a single "bulk PX server request", where like operations are grouped, so that their results can be easily grouped.

 ### See also:
 - [bulk response](#bulkresp)
 - [response](#resp)
*/
type BulkReq = {
  #add    : [ AddReq    ] ;
  #update : [ UpdateReq ] ;
  #rem    : [ EntId     ] ;
} ;

/**
 BulkResp
 -----------------------------------------------------------------------------------------------------------
 a single "bulk PX server response", where like operations are grouped, so that their results can be easily grouped.

 See also: [bulk request](#bulkreq).
*/
type BulkResp = {
  #add    : [ EntId ] ;
  #update : [ ?() ] ;
  #rem    : [ ?() ] ;
} ;


type UserAddReq = shared {
  user_name: Text;
  public_key: Text;
  description: Text;
  region: RegionId;
  isDeveloper: Bool;
  isProducer: Bool;
  isRetailer: Bool;
  isTransporter: Bool;
};

/**
 AddReq
 --------------------------------------
 a server `Add` request, as an AS datatype
*/
type AddReq = {
  #user        : UserAddReq ;
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
 UpdateReq
 --------------------------------------
 a server `Update` request, as an AS datatype
*/
type UpdateReq = {
  #user        : (UserId,        UserInfo) ;
  #truckType   : (TruckTypeId,   TruckTypeInfo) ;
  #region      : (RegionId,      RegionInfo) ;
  #produce     : (ProduceId,     ProduceInfo) ;
  #producer    : (ProducerId,    ProducerInfo) ;
  #retailer    : (RetailerId,    RetailerInfo) ;
  #transporter : (TransporterId, TransporterInfo) ;
  #inventory   : (InventoryId,   InventoryInfo) ;
  #route       : (RouteId,       RouteInfo) ;
} ;

/**
 EntId
 --------------------------------------
 An entity's ID; the response of an `Add` request
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
