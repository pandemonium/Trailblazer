namespace Trailblazer.Domain

open System

open FSharpPlus

open NodaTime
open NodaTime.Text

open Thoth.Json.Net

open Trailblazer.Infrastructure


type ('a, 'k) Continue = 'a -> 'k

type DomainError = UnresolvedReference of ref : string * domain : string
                 | BadJson             of message : string
                 | BadCircumstances    of message : string 

module DomainError =
  let asString = function 
    | UnresolvedReference (ref, domain) -> sprintf "`%s` is not in `%s`" ref domain
    | BadJson message                   -> sprintf "Bad JSON `%s`." message
    | BadCircumstances message          -> sprintf "Bad circumstances: `%s`" message

type 'a Out = Result<'a, Error>

and Error = Wtf      of exn
          | Database of Sql.UnitOfWorkError
          | Domain   of DomainError

type Acceptance = Accepted | Rejected of Error

module Out =
  let totally : 'a -> 'a Out = Ok
  let error   : Error -> 'a Out = Error

  let wtf         x : _ Out = Wtf x      |> Error
  let database    x : _ Out = Database x |> Error
  let domainError x : _ Out = Domain x   |> Error

  let ofAcceptance = function Accepted        -> totally ()
                            | Rejected error' -> error error'

  let asString =
    function Wtf      error -> sprintf "%A" error
           | Database error -> error
                               |> Sql.UnitOfWorkError.asString
                               |> sprintf "Database: %s"
           | Domain   error -> error
                               |> DomainError.asString
                               |> sprintf "Domain Error: %s"

type UniqueId = MkUniqueId of Guid

module UniqueId =
  let make = MkUniqueId
  let asGuid (MkUniqueId id) = id

  module Json =
    let encode : UniqueId Encoder = 
      fun (MkUniqueId id) -> Encode.guid id

    let decode : UniqueId Decoder =
      Decode.guid |> Decode.map MkUniqueId


type Timestamp = MkTimestamp of Instant

module Timestamp =
  let make = MkTimestamp
  let unmake (MkTimestamp ts) = ts

  let ofDateTimeUtc = Instant.FromDateTimeUtc

  module Json =
    let encode : Timestamp Encoder = 
      fun (MkTimestamp ts) -> 
        InstantPattern.ExtendedIso.Format ts
        |> Encode.string

    let decode : Timestamp Decoder =
      Decode.string
      |> Decode.andThen (
          InstantPattern.ExtendedIso.Parse >> function
            | result when result.Success -> Decode.succeed (MkTimestamp result.Value)
            | otherwise                  -> Decode.fail otherwise.Exception.Message)


module Entity =
  let mkDecoder constructor decodeId decodeInfo : 'a Decoder =
    Decode.map2 <| curry constructor
                <| Decode.field "id" decodeId
                <| decodeInfo


type Account = MkAccount of AccountId * AccountInfo
and AccountId = MkAccountId of UniqueId
and AccountInfo =
  { Created    : Timestamp
    Name       : string
    ExternalId : string
    Email      : string }

module Account =
  module Json =
    let encodeId : AccountId Encoder = 
      fun (MkAccountId id) -> UniqueId.Json.encode id

    let decodeId : AccountId Decoder =
      UniqueId.Json.decode |> Decode.map MkAccountId

    let encode : Account Encoder = fun (MkAccount (id, info)) ->
      [ "id",         encodeId id
        "created",    Timestamp.Json.encode info.Created
        "name",       Encode.string         info.Name
        "externalId", Encode.string         info.ExternalId
        "email",      Encode.string         info.Email ]
      |> Encode.object

    let decodeInfo : AccountInfo Decoder =
      Decode.object <| fun get ->
        { Created    = get.Required.Field "created"    Timestamp.Json.decode
          Name       = get.Required.Field "name"       Decode.string
          ExternalId = get.Required.Field "externalId" Decode.string
          Email      = get.Required.Field "email"      Decode.string }

    let decode : Account Decoder =
      Entity.mkDecoder MkAccount decodeId decodeInfo


type Order = MkOrder of OrderId * OrderInfo
and OrderId = MkOrderId of UniqueId
and OrderInfo =
  { Created   : Timestamp
    Reference : string
    Customer  : AccountId }

module Order =
  module Json =
    let encodeId : OrderId Encoder = 
      fun (MkOrderId id) -> UniqueId.Json.encode id

    let decodeId : OrderId Decoder =
      UniqueId.Json.decode |> Decode.map MkOrderId

    let encode : Order Encoder = fun (MkOrder (id, info)) ->
      [ "id",        encodeId              id
        "created",   Timestamp.Json.encode info.Created
        "reference", Encode.string         info.Reference
        "customer",  Account.Json.encodeId info.Customer ]
      |> Encode.object

    let decodeInfo : OrderInfo Decoder =
      Decode.object <| fun get ->
        { Created   = get.Required.Field "created"   Timestamp.Json.decode
          Reference = get.Required.Field "reference" Decode.string
          Customer  = get.Required.Field "customer"  Account.Json.decodeId }

    let decode : Order Decoder =
      Entity.mkDecoder MkOrder decodeId decodeInfo


type Payment = MkPayment of PaymentId * PaymentInfo
and PaymentId = MkPaymentId of UniqueId
and PaymentInfo =
  { Created : Timestamp
    OrderId : OrderId
    Amount  : uint }

module Payment =
  module Json =
    let encodeId : PaymentId Encoder = 
      fun (MkPaymentId id) -> UniqueId.Json.encode id
    
    let decodeId : PaymentId Decoder =
      UniqueId.Json.decode |> Decode.map MkPaymentId

    let encode : Payment Encoder = fun (MkPayment (id, info)) ->
      [ "id",      encodeId              id
        "created", Timestamp.Json.encode info.Created
        "order",   Order.Json.encodeId   info.OrderId
        "amount",  Encode.uint32         info.Amount ]
      |> Encode.object

    let decodeInfo : PaymentInfo Decoder =
      Decode.object <| fun get ->
        { Created = get.Required.Field "created" Timestamp.Json.decode
          OrderId = get.Required.Field "order"   Order.Json.decodeId
          Amount  = get.Required.Field "amount"  Decode.uint32 }

    let decode : Payment Decoder =
      Entity.mkDecoder MkPayment decodeId decodeInfo


type Trail = MkTrail of TrailId * TrailInfo
and TrailId = MkTrailId of UniqueId
and TrailInfo =
  { Created   : Timestamp
    Name      : string
    Frequency : uint }

module Trail =
  module Json =
    let encodeId : TrailId Encoder =
      fun (MkTrailId id) -> UniqueId.Json.encode id

    let decodeId : TrailId Decoder =
      UniqueId.Json.decode |> Decode.map MkTrailId

    let encode : Trail Encoder = fun (MkTrail (id, info)) ->
      [ "id",        encodeId id
        "created",   Timestamp.Json.encode info.Created
        "name",      Encode.string         info.Name
        "frequency", Encode.uint32         info.Frequency ]
      |> Encode.object

    let decodeInfo : TrailInfo Decoder =
      Decode.object <| fun get ->
        { Created   = get.Required.Field "created"   Timestamp.Json.decode
          Name      = get.Required.Field "name"      Decode.string
          Frequency = get.Required.Field "frequency" Decode.uint32 }

    let decode : Trail Decoder =
      Entity.mkDecoder MkTrail decodeId decodeInfo

type Issue = MkIssue of IssueId * IssueInfo
and IssueId = MkIssueId of UniqueId
and IssueInfo =
  { Created : Timestamp
    Subject : string
    Body    : string }

module Issue =
  module Json =
    let encodeId : IssueId Encoder =
      fun (MkIssueId id) -> UniqueId.Json.encode id

    let decodeId : IssueId Decoder =
      UniqueId.Json.decode |> Decode.map MkIssueId

    let encode : Issue Encoder = fun (MkIssue (id, info)) ->
      [ "id",        encodeId id
        "created",   Timestamp.Json.encode info.Created
        "subject",   Encode.string         info.Subject
        "body",      Encode.string         info.Body ]
      |> Encode.object

    let decodeInfo : IssueInfo Decoder =
      Decode.object <| fun get ->
        { Created   = get.Required.Field "created"   Timestamp.Json.decode
          Subject   = get.Required.Field "subject"   Decode.string
          Body      = get.Required.Field "body"      Decode.string }

    let decode : Issue Decoder =
      Entity.mkDecoder MkIssue decodeId decodeInfo
