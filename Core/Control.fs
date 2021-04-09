module rec Trailblazer.Control

open System

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open NodaTime

open Trailblazer.Infrastructure
open Trailblazer.Domain


type 'a Script = ReaderT<IContext, 'a Out>

(* How would this thing carry a TransactionContext or something
   of that nature so that a chain of events from Command to Acceptance
   all persist atomically. 

   Command -> Acceptance is in and out of a Bounded Context.
 *)
type IContext =
  abstract Settings : string
  abstract Database : Sql.Database
  abstract EventBus : EventBus.Driver
  
let script = monad.fx

let runScript context script : 'a Out =
  try flip ReaderT.run context script
  with e -> Out.wtf e

let context : IContext Script = ask

let liftResult (liftError : 'b -> Error) (r : Result<'a, 'b>) : 'a Script  =
  Result.mapError liftError r
  |> lift

let liftOut = ReaderT.lift

let liftUnitOfWork uow : 'a Script = script {
  let! ctx = context
  return! Sql.Database.apply ctx.Database uow
          |> liftResult Database
}

let liftAsync block : 'a Script = 
  Async.RunSynchronously block |> result

let uniqueId : UniqueId Script = script {
  return Guid.NewGuid ()
         |> UniqueId.make
}

let currentTimestamp : Timestamp Script = script {
  return SystemClock.Instance.GetCurrentInstant ()
         |> Timestamp.make
}

let gtfo error =
  Out.asString error |> failwith

module EventBus =  
  type Sink = Event -> unit Script

  type BusState =
    { Subscribers : Sink list }

  module BusState =
    let empty : BusState = { Subscribers = [] }

    let addSubscriber sink ({ Subscribers = ss } as state) : BusState Script =
      { state with Subscribers = sink :: ss }
      |> result

    (* There are multiple UnitOfWorks happning downstream of this that
       ought to be in the same transaction. They won't be, yet. *)
    let multicast event ({ Subscribers = ss } as state) : BusState Script =
      traverse (fun publish -> publish event) ss
      |> liftM (konst state)

  type Protocol = Publish   of Event
                | Subscribe of Sink
  type Driver = private MkDriver of Protocol MailboxProcessor

  let runBus context (inbox : Protocol MailboxProcessor) : unit Async =
    let update =
      function Publish   e -> BusState.multicast e
             | Subscribe s -> BusState.addSubscriber s
      |> flip

    let rec runLoop busState =
      inbox.Receive () >>= applyProtocol busState
    and applyProtocol busState =
      update busState >> runScript context
      >> function Ok busState     -> runLoop busState
                | Error otherwise -> gtfo otherwise
    in runLoop BusState.empty

  let private dispatch msg : unit Script = script {
    let! ctx = context
    let (MkDriver driver) = ctx.EventBus
    driver.Post msg
    return ()
  }

  let emit =
    Publish >> dispatch

  let attach =
    Subscribe >> dispatch

  let make context : Driver =
    let startBus =
      runBus context
    in MailboxProcessor.Start startBus
       |> MkDriver