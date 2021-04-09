open System

open FSharpPlus

open Trailblazer.Infrastructure
open Trailblazer.Control
open Trailblazer.Domain
open Trailblazer.Core

module Postgres =
  open Npgsql

  let mkDatabase connectionString : Sql.Database = 
    NpgsqlConnection.GlobalTypeMapper.UseNodaTime ()
    |> ignore

    let connect () =
      new NpgsqlConnection (connectionString) :> Data.IDbConnection
    in Sql.Database.make connect

module Bootstrap =
  let connectionString = "User ID=pa;Password=;Host=localhost;Port=5432;Database=trailblzr;Pooling=true;"
  (* This will eventually return a record of all
     components / aggregates.
   *)
  let assembleContext inContext =
    EventBus.make inContext
  type ConcreteContext () as this =
    let eventBus = assembleContext this
    interface IContext with
      member __.Settings = "Hi!"
      member __.Database = Postgres.mkDatabase connectionString
      member __.EventBus = eventBus

  let makeContext : IContext =
    new ConcreteContext () :> IContext

[<EntryPoint>]
let main argv =
  let listener =
    function PaymentCompleted _ -> 
              script {
                printfn "Payment Completed"
                return ()
              }
           | otherwise          -> 
              script {
                printfn "listener: %A" otherwise
                do! EventBus.emit PaymentCompleted
              }

  let p = 
    script { 
      let! x = uniqueId
      let! y = uniqueId

      (* I could pass a predicate that indicates which events are interesting. *)
//      do! EventBus.attach listener
//      do! EventBus.emit OrderPaid
//      do! EventBus.emit OrderCheckedOut

      let! trails = Trails.make

      let! ts = currentTimestamp
      do! Trails.createTrail { Created = ts; Name = "Matlagning."; Frequency = 5u } trails
      do! Trails.createTrail { Created = ts; Name = "Kalle Anka."; Frequency = 1u } trails

      (* Eventually consistent so this does not always have all the items. *)
      let! tls = Trails.getTrails trails

      printfn "---- Begins %d" (tls.Length)
      List.iter (printf "Program.fs: `%A`") tls
      printfn "---- Ends"

      return sprintf "%A;%A" x y
    }

  let context =
      Bootstrap.makeContext

  p 
  |> runScript context
  |> printfn "%A"

  Async.Sleep 2500 |> Async.RunSynchronously

  0