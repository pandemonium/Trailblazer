namespace Trailblazer.Domain

open FSharpPlus

open Thoth.Json.Net


(* This file is a little untidy; there are a lot of pattern matches
   on event that I don't quite like.
*)

type Event = AccountCreated     of Account
           | PaymentCreated     of Payment
           | PaymentCompleted
           | OrderPaid       
           | OrderCreated       of Order
           | OrderCheckedOut   
           | InventoryItemAdded
           | ProductAdded
           | TrailCreated       of Trail
           | IssueCreated       of Issue
           | IssueAdded         of TrailId * IssueId  (* Wat? *)

(* This ought to be in its own file because there is going to be
   a number of different bounded contexts, each with its own Event
   datatype that must map onto it.
 *)
type ExternalEvent =
  private { Id          : EventId
            When        : Timestamp
            AggregateId : UniqueId
            What        : EventType
            Data        : JsonData }

and EventId =
  private MkEventId of UniqueId

and EventType =
  private MkEventType of string

and JsonData =
  private MkJsonData of string

module EventId =
  let make = MkEventId

  let asGuid (MkEventId id) =
    UniqueId.asGuid id

  let fromGuid = 
    UniqueId.make >> make

module EventType =
  let fromEvent =
    function AccountCreated     _ -> "AccountCreated"
           | PaymentCreated     _ -> "PaymentCreated"
           | PaymentCompleted   _ -> "PaymentCompleted"
           | OrderPaid          _ -> "OrderPaid"
           | OrderCreated       _ -> "OrderCreated"
           | OrderCheckedOut    _ -> "OrderCheckedOut"
           | InventoryItemAdded _ -> "InventoryItemAdded"
           | ProductAdded       _ -> "ProductAdded"
           | TrailCreated       _ -> "TrailCreated"
           | IssueCreated       _ -> "IssueCreated"
           | IssueAdded         _ -> "IssueAdded"
    >> MkEventType

  let parse : string -> EventType Out =
    function "AccountCreated"     
           | "PaymentCreated"     
           | "PaymentCompleted"   
           | "OrderPaid"          
           | "OrderCreated"       
           | "OrderCheckedOut"    
           | "InventoryItemAdded" 
           | "ProductAdded"
           | "TrailCreated" 
           | "IssueCreated" 
           | "IssueAdded" as typeName -> 
              MkEventType typeName
              |> Out.totally
           | otherwise ->
              UnresolvedReference (otherwise, nameof EventType)
              |> Out.domainError

  let asString (MkEventType name) = name

  let resolveDecoder : EventType -> Event Decoder =
    asString >>
    function "AccountCreated"     -> Decode.map AccountCreated Account.Json.decode
           | "PaymentCreated"     -> Decode.map PaymentCreated Payment.Json.decode
           | "PaymentCompleted"   -> Decode.fail "PaymentCompleted"
           | "OrderPaid"          -> Decode.fail "OrderPaid"
           | "OrderCreated"       -> Decode.map OrderCreated Order.Json.decode
           | "OrderCheckedOut"    -> Decode.fail "OrderCheckedOut"
           | "InventoryItemAdded" -> Decode.fail "InventoryItemAdded"
           | "ProductAdded"       -> Decode.fail "ProductAdded"
           | "TrailCreated"       -> Decode.map TrailCreated Trail.Json.decode
           | "IssueCreated"       -> Decode.fail "IssueCreated"
           | "TrailAdded"         -> Decode.fail "TrailAdded"
           | otherwise            -> failwith "Internal error."

module JsonData =
  let fromEvent =
    function AccountCreated     _     -> Encode.string "AccountCreated"
           | PaymentCreated     _     -> Encode.string "PaymentCreated"
           | PaymentCompleted   _     -> Encode.string "PaymentCompleted"
           | OrderPaid          _     -> Encode.string "OrderPaid"
           | OrderCreated       _     -> Encode.string "OrderCreated"
           | OrderCheckedOut    _     -> Encode.string "OrderCheckedOut"
           | InventoryItemAdded _     -> Encode.string "InventoryItemAdded"
           | ProductAdded       _     -> Encode.string "ProductAdded"
           | TrailCreated       trail -> Trail.Json.encode trail
           | IssueCreated       _     -> Encode.string "IssueCreated"
           | IssueAdded         _     -> Encode.string "IssueAdded"
    >> Encode.toString 0
    >> MkJsonData

  let decode (MkJsonData data) : EventType -> Event Out =
    EventType.resolveDecoder 
    >> flip Decode.fromString data
    >> Result.bindError (BadJson >> Out.domainError)

  let asString (MkJsonData data) = data

module Event =
  let aggregateId =
    let crash = failwithf "Unknown event-type %A"
    in function TrailCreated (MkTrail (MkTrailId id, _)) -> id
              | unknown                                  -> crash unknown

  let externalRepresentation (eventId : UniqueId) when' event : ExternalEvent = 
    { Id          = EventId.make eventId
      When        = when'
      AggregateId = aggregateId event
      What        = EventType.fromEvent event
      Data        = JsonData.fromEvent event }

  let parseExternalRepresentation (external : ExternalEvent) : Event Out = 
    JsonData.decode external.Data external.What

module ExternalEvent =
  type Record =
    { Id          : System.Guid
      AggregateId : System.Guid
      Created     : NodaTime.Instant
      Name        : string
      Body        : string }

  let asRecord (rep : ExternalEvent) =
    { Id          = rep.Id          |> EventId.asGuid
      AggregateId = rep.AggregateId |> UniqueId.asGuid
      Created     = rep.When        |> Timestamp.unmake
      Name        = rep.What        |> EventType.asString
      Body        = rep.Data        |> JsonData.asString }

  let parseRecord (record : Record) : ExternalEvent Out =
    EventType.parse record.Name >>= fun eventType ->
      { Id          = record.Id          |> EventId.fromGuid
        When        = record.Created     |> Timestamp.make
        AggregateId = record.AggregateId |> UniqueId.make
        What        = eventType
        Data        = MkJsonData record.Body }
      |> Out.totally
