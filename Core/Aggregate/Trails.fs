namespace Trailblazer.Core

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Trailblazer.Domain
open Trailblazer.Control


module CQ = CommandQueryAggregateHelper

module Trails =
  (* All indices are going to be very similar. *)
  (* This is really an external ReadModel. *)
  (* Maybe the aggregate is going to need a WriteModel as well? *)
  module Index =
    type Self    = private MkIndex of Protocol MailboxProcessor
    and Protocol = Update of Event | Read of Query
    and Query    = GetTrails      of CQ.Channel<unit, Trail list>
                 | GetIssues      of CQ.Channel<unit, Issue list>
                 | GetTrailIssues of CQ.Channel<TrailId, Issue list>
    and State    = { TrailById   : Map<TrailId, TrailInfo> 
                     IssueById   : Map<IssueId, IssueInfo>
                     TrailIssues : (TrailId * IssueId) Set }

    module State =
      let empty = { TrailById   = Map.empty
                    IssueById   = Map.empty
                    TrailIssues = Set.empty }

      let addTrail (MkTrail (id, info)) state =
        { state with TrailById = Map.add id info state.TrailById }

      let addIssue (MkIssue (id, info)) state =
        { state with IssueById = Map.add id info state.IssueById }

      let addTrailIssue trail issue state =
        { state with TrailIssues = Set.add (trail, issue) state.TrailIssues }

      let trails state =
        state.TrailById |> Map.toList |> map MkTrail

      let issues state =
        state.IssueById |> Map.toList |> map MkIssue

      let trailIssues trail state =
        let joinIssues =
          function t, i when t = trail -> Map.tryFind i state.IssueById
                                          |> map (fun info -> MkIssue (i, info))
                 | otherwise           -> None
        in state.TrailIssues
           |> Set.toList              (* This can be solved with a fold. *)
           |> List.choose joinIssues

    let update state =
      let updateIndex =
        function TrailCreated trail        -> State.addTrail trail
               | IssueCreated issue        -> State.addIssue issue
               | IssueAdded (trail, issue) -> State.addTrailIssue trail issue
               | otherwise                 -> id
      let queryIndex =
        function GetTrails (_, issuer) ->
                  State.trails state |> CQ.respond issuer state
               | GetIssues (_, issuer) ->
                  State.issues state |> CQ.respond issuer state
               | GetTrailIssues (trail, issuer) ->
                  State.trailIssues trail state |> CQ.respond issuer state
      in function Update event -> updateIndex event state |> result
                | Read    query -> queryIndex query

    let read query (MkIndex self) : unit Script = script {
      Read query |> self.Post
    }

    let receiveEvent (MkIndex self) event : unit Script = script {
      Update event |> self.Post
    }

    let instantiate : Self Script = script {
      let! context = context
      let mkState _ = State.empty
      let self = CQ.start mkState update context |> MkIndex
      do! receiveEvent self |> EventBus.attach
      return self
    }

  module Aggregate =
    type Self      = private MkAggregate of Protocol MailboxProcessor
    and Protocol   = Issue of Command | Execute of Index.Query

    (* Can I have a command to indicate that it should get a snapshot
       from the/ a Read-Model?

       Might not have to be a command; maybe an Out-of-band message that
       is not part of any public API?
     *)
    and Command    = CreateTrail   of CQ.Channel<TrailInfo,         Acceptance>
                   | CreateIssue   of CQ.Channel<IssueInfo,         Acceptance>
                   | AddTrailIssue of CQ.Channel<TrailId * IssueId, Acceptance>
    and Runtime    = { Index      : Index.Self
                       WriteModel : WriteModel }
    and WriteModel = { Trails : TrailId Set
                       Issues : IssueId Set }

    type 'a Handler = ReaderT<Runtime, 'a Script>

    module Handler =
      let handle = monad.fx
      let runtime : Runtime Handler = ask
      let writeModel : WriteModel Handler = 
        (fun runtime -> runtime.WriteModel) <!> runtime

      let verify (p : WriteModel -> bool) : bool Handler =
        p <!> writeModel

      let updateWriteModel (f : WriteModel -> WriteModel) : Runtime Handler = 
        let apply rt =
          { rt with WriteModel = f rt.WriteModel }
        in apply <!> runtime

      let eventId constructor : 'a Handler =
        constructor <!> uniqueId |> lift

      let publish event : unit Handler = 
        CQ.publish event |> lift

      let accept issuer : unit Handler =
        CQ.accepted issuer

      let reject issuer error : unit Handler =
        CQ.rejected issuer error

      let triviallyAccept issuer (mkEventId : UniqueId -> 'a) (makeEvent : 'a -> Event Handler) : Runtime Handler = handle {
        let! id    = eventId mkEventId
        let! event = makeEvent id
        do! publish event
        do! accept issuer
        return! runtime
      }

      let run (handler : 'a Handler) runtime : 'a Script =
        ReaderT.run handler runtime

    (* How is this thing populated? *)
    module WriteModel =
      let emptyWriteModel = { Trails = Set.empty
                              Issues = Set.empty }

      let addTrail trail model =
        { model with Trails = Set.add trail model.Trails }

      let addIssue issue model =
        { model with Issues = Set.add issue model.Issues }

      let has where what (model : WriteModel) =
        model |> where |> Set.contains what

      let hasTrail = has (fun m -> m.Trails)

      let hasIssue = has (fun m -> m.Issues)

      let make index = { Index      = index
                         WriteModel = emptyWriteModel }

    let handleCommand = Handler.run << function
      | CreateTrail (info, issuer) ->
        let mkEvent id = Handler.handle {
            let! _ = WriteModel.addTrail id 
                     |> Handler.updateWriteModel
            return MkTrail (id, info) |> TrailCreated }
        in Handler.triviallyAccept issuer MkTrailId mkEvent

      | CreateIssue (info, issuer) ->
        let mkEvent id = Handler.handle {
          let! _ = WriteModel.addIssue id
                   |> Handler.updateWriteModel
          return MkIssue (id, info) |> IssueCreated }
        in Handler.triviallyAccept issuer MkIssueId mkEvent

      | AddTrailIssue ((t, i), issuer) ->
        Handler.handle {
          let! p = Handler.verify (WriteModel.hasTrail t)
          let! q = Handler.verify (WriteModel.hasIssue i)

          do! if p && q then
                Handler.accept issuer 
                *> (IssueAdded (t, i) |> Handler.publish)
              else
                sprintf "Issue (%A) or trail (%A) missing." i t
                |> BadCircumstances
                |> Domain
                |> Handler.reject issuer

          return! Handler.runtime }

    let update runtime =
      function Issue command -> handleCommand command runtime

               (* I don't think this should be here. *)
             | Execute query -> konst runtime <!> Index.read query runtime.Index

    (* How does this deal with the WriteModel initialization? *)
    let instantiate index : Self Script = script {
      let! context    = context
      let mkRuntime _ = WriteModel.make index
      return CQ.start mkRuntime update context
             |> MkAggregate }

    let mkCommand mkRequest parameter (MkAggregate self) : CQ.CommandHandler =
      CQ.mkCommandHandler Issue mkRequest parameter self

    let mkQuery mkRequest parameter (MkAggregate self) =
      CQ.mkQueryHandler Execute mkRequest parameter self

  let createTrail =
    Aggregate.mkCommand Aggregate.CreateTrail

  let createIssue =
    Aggregate.mkCommand Aggregate.CreateIssue

  let getTrails =
    Aggregate.mkQuery Index.GetTrails ()

  let getIssues =
    Aggregate.mkQuery Index.GetIssues ()

  let getTrailIssues =
    Aggregate.mkQuery Index.GetTrailIssues

  let make : Aggregate.Self Script =
    Index.instantiate >>= Aggregate.instantiate