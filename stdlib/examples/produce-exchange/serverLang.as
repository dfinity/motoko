/**

 Server language
 =====================

 A simple DSL for scripting interactions with the Produce Exchange (PX) server.

 PS: This file demosntrates (one of _many_ reasons) why we aren't using GoLang for the PX.
*/

let Result = (import "../../result.as");
type Result<Ok,Err> = Result.Result<Ok,Err>;

let T = import "serverTypes.as";

/**
 `Req`
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
 `Resp`
 --------------------------------------
 a single PX server **response**, as an AS datatype;

 here, we do not include the error case of the `Result` structure; See also [result respose](#resultresp).

 ### See also:
 - [result resp](#result-resp)
 - [request](#req)
 - [bulk response](#bulkresp)
*/
type Resp = {
  #add    : EntId;
  #update : ();
  #rem    : ();
} ;

/**
 `ResultResp`
 --------------------------------------
*/
type ResultResp = Result<Resp, T.ServerErr>;

/**
 `BulkReq`
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
 `BulkResp`
 -----------------------------------------------------------------------------------------------------------
 a single "bulk PX server response", where like operations are grouped, so that their results can be easily grouped.

 See also: [bulk request](#bulkreq).
*/
type BulkResp = {
  #add    : [ Result<EntId, T.ServerErr> ] ;
  #update : [ Result<(),    T.ServerErr> ] ;
  #rem    : [ Result<(),    T.ServerErr> ] ;
} ;


type UserAddReq = shared {
  user_name: Text;
  public_key: Text;
  description: Text;
  region: T.RegionId;
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
  #truckType   : T.TruckTypeInfo ;
  #region      : T.RegionInfo ;
  #produce     : T.ProduceInfo ;
  #producer    : T.ProducerInfo ;
  #retailer    : T.RetailerInfo ;
  #transporter : T.TransporterInfo ;
  #inventory   : T.InventoryInfo ;
  #route       : T.RouteInfo ;
} ;

/**
 UpdateReq
 --------------------------------------
 a server `Update` request, as an AS datatype
*/
type UpdateReq = {
  #user        : (T.UserId,        T.UserInfo) ;
  #truckType   : (T.TruckTypeId,   T.TruckTypeInfo) ;
  #region      : (T.RegionId,      T.RegionInfo) ;
  #produce     : (T.ProduceId,     T.ProduceInfo) ;
  #producer    : (T.ProducerId,    T.ProducerInfo) ;
  #retailer    : (T.RetailerId,    T.RetailerInfo) ;
  #transporter : (T.TransporterId, T.TransporterInfo) ;
  #inventory   : (T.InventoryId,   T.InventoryInfo) ;
  #route       : (T.RouteId,       T.RouteInfo) ;
} ;

/**
 EntId
 --------------------------------------
 An entity's ID; the response of an `Add` request
*/
type EntId = {
  #user        : T.UserId ;
  #truckType   : T.TruckTypeId ;
  #region      : T.RegionId ;
  #produce     : T.ProduceId ;
  #producer    : T.ProducerId ;
  #retailer    : T.RetailerId ;
  #transporter : T.TransporterId ;
  #inventory   : T.InventoryId ;
  #route       : T.RouteId ;
}
