/**
 Non-Fungible Tokens
 ----------------------

 ...as abstractions in ActorScript.

 For reference, see these documents:

 1. http://erc721.org/

 2. linked from item 1: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-721.md

 2019.09.24 -- Note to reader: For now, the ActorScript below attempts
to be a "direct" translation of the Solidity types/APIs given above.
This translation is a WIP.  This file is merely a rough sketch, but
may change roles in the future.

*/

type Address = Nat;
type TokenId = Nat;

type Transfer = {
     source: Address;
     target: Address;
     tokenId: TokenId;
};

type Approval = {
     owner: Address;
     approved: Address;
     tokenId: TokenId;
};

type ApprovalForAll = {
     owner: Address;
     operator: Address;
     approved: Bool;
};

type Event = {
     #transfer: Transfer;
     #approval: Approval;
     #approvalForAll: ApprovalForAll;
};

type NonFungibleToken = {
  // todo: instead of the ?_ form in each return type below, 
  //       instead use Result type from stdlib?
  //       if so, what should the error type be? an enum?
  
  balanceOf : shared Address -> async ?Nat;
  
  ownerOf : shared TokenId -> async ?Address;
  
  safeTransferFrom : shared Transfer -> async ?();
  
  transferFrom : shared Transfer -> async ?();
  
  approve : shared Approval -> async ?();

  setApprovalForAll : shared ApprovalForAll -> async ?();

  getApproved : shared TokenId -> async ?Address;
  
  isApprovedForAll : (Address, Address) -> async ?Bool;
  
};
