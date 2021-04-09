namespace Trailblazer.Core

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open Trailblazer.Domain
open Trailblazer.Control
open Trailblazer.Infrastructure


module CommandQueryAggregateHelper =
  type CommandHandler = unit Script

  type ('a, 'b) Channel = 'a * 'b AsyncReplyChannel

  let dispatch (channel : Channel<'a, 'b> -> 'c) (protocol : 'c -> 'p) (driver : 'p MailboxProcessor) =
    curry channel >> fun compose ->
      compose >> protocol
      |> driver.PostAndAsyncReply

  let mkCommandHandler inject =
    fun request parameter self ->
      dispatch request inject self parameter
      |> liftAsync
      >>= (Out.ofAcceptance >> liftOut)

  let mkQueryHandler inject =
    fun query parameter self ->
      dispatch query inject self parameter 
      |> liftAsync

  let mkRunLoop initialState update context (inbox : 'a MailboxProcessor) =
    let rec runLoop state =
      inbox.Receive () >>= applyProtocol state
    and applyProtocol state msg =
      update state msg
      |> Result.protect (runScript context)
      |> either id Out.wtf
      |> function Ok state        -> runLoop state
                | Error otherwise -> gtfo otherwise
    in runLoop <| initialState ()

  let start init update =
    mkRunLoop init update >> MailboxProcessor.Start
    >> tap (fun mp -> mp.Error.Add (printfn "%A"))

  (* This could be typed in a way that would require that it be called. *)
  let inline accepted (issuer : Acceptance AsyncReplyChannel) =
    result (issuer.Reply Accepted)

  let inline rejected (issuer : Acceptance AsyncReplyChannel) error =
    result (issuer.Reply <| Rejected error)

  let respond (issuer : 'a AsyncReplyChannel) state reply : 'b Script = script {
    issuer.Reply reply
    return state
  }

  let publish event : unit Script = 
    EventLog.append event *> EventBus.emit event

  let publishUnique (mkEvent : UniqueId -> Event) : unit Script =
    mkEvent >> publish =<< uniqueId