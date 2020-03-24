import Result = "mo:stdlib/Result";
import T = "serverTypes";


module {
/**

 Server language
 =====================

 A simple DSL for scripting interactions with the Produce Exchange (PX) server.

*/

public type Result<Ok,Err> = Result.Result<Ok,Err>;

/**
 `Req`
 --------------------------------------
 a single PX server **request**, as an AS datatype

 ### See also:
 - [resp](#resp)
 - [bulk request](#bulkreq)
*/
public type Req = {
  #reset  : () ;
  #add    : AddReq ;
  #update : UpdateReq ;
  #rem    : T.EntId ;
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
public type Resp = {
  #reset;
  #add    : T.EntId;
  #update : ();
  #rem    : ();
} ;

/**
 `ResultResp`
 --------------------------------------
 A `Result` whose OK type is `Resp`.
*/
public type ResultResp = Result<Resp, T.ServerErr>;

/**
 `BulkReq`
 -----------------------------------------------------------------------------------------------------------
 a single "bulk PX server request", where like operations are grouped, so that their results can be easily grouped.

 ### See also:
 - [bulk response](#bulkresp)
 - [response](#resp)
*/
public type BulkReq = {
  #add    : [ AddReq    ] ;
  #update : [ UpdateReq ] ;
  #rem    : [ T.EntId     ] ;
} ;

/**
 `BulkResp`
 -----------------------------------------------------------------------------------------------------------
 a single "bulk PX server response", where like operations are grouped, so that their results can be easily grouped.

 See also: [bulk request](#bulkreq).
*/
public type BulkResp = {
  #add    : [ Result<T.EntId, T.IdErr> ] ;
  #update : [ Result<(), T.IdErr> ] ;
  #rem    : [ Result<(), T.IdErr> ] ;
} ;


public type UserAddReq = {
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
 `RouteAddReq`
 ----------------
 Note: The `RouteInfo` type contains too much information about truck types to use here in place of this type.
 */
public type RouteAddReq = {
  id : T.RouteId;
  transporter : T.TransporterId;
  truck_type : T.TruckTypeId;
  start_region : T.RegionId;
  end_region : T.RegionId;
  start_date : T.Date;
  end_date : T.Date;
  cost : T.Price;
};

/**
 AddReq
 --------------------------------------
 a server `Add` request, as an AS datatype

 note that each case with a `T.XInfo` payload (where `X` = `Producer`,
 etc.), the identifier is ignored; the request, if successful, will
 generate a fresh identifier for the new entity.
*/
public type AddReq = {
  #user        : UserAddReq ;
  #truckType   : T.TruckTypeInfo ;
  #region      : T.RegionInfo ;
  #produce     : T.ProduceInfo ;
  #producer    : T.ProducerInfo ;
  #retailer    : T.RetailerInfo ;
  #transporter : T.TransporterInfo ;
  #inventory   : T.InventoryInfo ;
  #route       : RouteAddReq ;
} ;

/**
 UpdateReq
 --------------------------------------
 a server `Update` request, as an AS datatype
*/
public type UpdateReq = {
  #user        : T.UserInfo ;
  #truckType   : T.TruckTypeInfo ;
  #region      : T.RegionInfo ;
  #produce     : T.ProduceInfo ;
  #producer    : T.ProducerInfo ;
  #retailer    : T.RetailerInfo ;
  #transporter : T.TransporterInfo ;
  #inventory   : T.InventoryInfo ;
  #route       : T.RouteInfo ;
} ;
}
