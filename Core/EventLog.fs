namespace Trailblazer.Infrastructure

open System

open NodaTime

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Trailblazer.Control
open Trailblazer.Domain


type private R = ExternalEvent.Record

module EventStore =
  let insert (event : R) : unit Sql.UnitOfWork =
    let text = """
      INSERT INTO event_log (id, aggregate_id, created, "event_type", data) 
           VALUES (@Id, @AggregateId, @Created, @Name, @Body)"""
    in ignore <!> Sql.UnitOfWork.write text event

  let queryAggregate aggregateId : R list Sql.UnitOfWork =
    let query = """SELECT id
                        , aggregate_id
                        , created
                        , event_type
                        , data
                     FROM event_log
                    WHERE aggregate_id = @AggregateId"""
    in Sql.Query.select
         { let! id          = Sql.Query.getGuid
           let! aggregateId = Sql.Query.getGuid (* Do all events belong to an aggregate? *)
           let! created     = Timestamp.ofDateTimeUtc <!> Sql.Query.getDateTime
           let! eventType   = Sql.Query.getString
           let! data        = Sql.Query.getString
   
           return { R.Id          = id
                    R.AggregateId = aggregateId
                    R.Created     = created
                    R.Name        = eventType
                    R.Body        = data } }
       |> Sql.Query.list
       |> Sql.UnitOfWork.read query {| AggregateId = aggregateId |}

(* This code must not return Script. It must be UnitOfWork
   because client code must be able to append more than
   one event in the same transaction.

   How does a replay happen? Events must not trigger new events during
   it - how does that happen? Can the EventBus be made to forbid it?
 *)
module EventLog =
  let append event : unit Script = script {
    let! id  = uniqueId
    let! now = currentTimestamp
    return! event
            |> Event.externalRepresentation id now
            |> ExternalEvent.asRecord
            |> EventStore.insert
            |> liftUnitOfWork
  }

  let parseEvent =
    ExternalEvent.parseRecord >=> Event.parseExternalRepresentation

  let readAggregate aggregateId : Event list Script = script {
    let! stream = EventStore.queryAggregate aggregateId
                  |> liftUnitOfWork

    return! stream
            |> traverse parseEvent
            |> liftOut
  }