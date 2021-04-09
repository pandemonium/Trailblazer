namespace Trailblazer.Infrastructure.Sql

open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Data

open System.Data
open System.Reflection

open Microsoft.Data.SqlClient

open Trailblazer.Infrastructure


type TransactionContext =
  { Connection  : IDbConnection
    Transaction : IDbTransaction
    Exception   : exn option }

type 'a UnitOfWork = ReaderT<TransactionContext, 'a Value>

and 'a Value = Result<'a, UnitOfWorkError>

and UnitOfWorkError = Connection of exn
                    | Execution  of exn

module UnitOfWorkError =
  let inline private lift error : _ UnitOfWork =
    Error error |> ReaderT.lift
  let connection e    = Connection e |> lift
  let withExecution e = Execution e  |> lift

  let asString =
    function Connection ex -> sprintf "Connection: %A" ex
           | Execution  ex -> sprintf "Execution: %A" ex

type 'a ConnectionReader = ReaderT<IDbConnection, 'a Value>

module TransactionContext =
  let current : TransactionContext UnitOfWork = ask

  let enter (c : IDbConnection) : TransactionContext =
    printfn "enter: BeginTransaction"
    { Connection = c
      Transaction = c.BeginTransaction ()
      Exception = None }

  let leave = function
            | { Transaction = t; Exception = Some _ } -> printfn "leave: Rollback"; t.Rollback ()
            | { Transaction = t }                     -> printfn "leave: Commit"; t.Commit ()

  (* Change from ReaderT to StateT so that I can
     change the TransactionContext to include
     an exception and perhaps introduce a setRollbackOnly-flag.
   *)
  let execute (action : 'a UnitOfWork) : 'a UnitOfWork = monad {
    let! result  = action
    let! context = current
    leave context
    return result
  }

  let demarcate (action : 'a UnitOfWork) : 'a ConnectionReader =
    local enter <| execute action

  let enlist (connection : IDbConnection) (action : 'a UnitOfWork) =
    try
      printfn "enlist: Open"
      connection.Open ()
      ReaderT.run (demarcate action) connection
    finally
      printfn "enlist: Close"
      connection.Close ()

type Database = 
  private { Connect : unit -> IDbConnection }

module Database =
  let make factory =
    { Connect = factory }

  (* This probably does not work because the semantic scoping does not
     translate well in conjunction with closures.
   *)
  let apply { Connect = mkConnection } =
    try
      let connection = mkConnection ()
      TransactionContext.enlist connection
    with 
      e -> Connection e |> Error |> konst

type ReadState =
  private { Reader  : IDataReader
            Ordinal : int
            CanRead : bool }

module ReadState =
  let make reader =
    { Reader  = reader
      Ordinal = 0
      CanRead = false }

  let getField (extract : IDataReader -> int -> 'a) state : 'a =
    extract state.Reader state.Ordinal

  let nextField state =
    { state with Ordinal = state.Ordinal + 1 }

  let resetOrdinal state =
    { state with Ordinal = 0 }

  let isNull state : bool =
    state.Reader.IsDBNull state.Ordinal

  let canRead state : bool =
    state.CanRead

  let nextRow state : ReadState =
    { resetOrdinal state with CanRead = state.Reader.Read () }


type 'a Query = State<ReadState, 'a>

module Query =
  let field extract : 'a Query = monad {
    let! value = State.gets <| ReadState.getField extract
    do! State.modify ReadState.nextField
    return value
  }

  let nullable (query : 'a Query) : 'a option Query = monad {
    let! isNull = State.gets ReadState.isNull
    if isNull
      then return None
      else return! map Some query
  }

  let getString   = field (fun r -> r.GetString)
  let getInt      = field (fun r -> r.GetInt32)
  let getGuid     = field (fun r -> r.GetGuid)
  let getDateTime = field (fun r -> r.GetDateTime)
  let getDecimal  = field (fun r -> r.GetDecimal)
  let getFloat    = field (fun r -> r.GetFloat)
  let getDouble   = field (fun r -> r.GetDouble)
  let getBoolean  = field (fun r -> r.GetBoolean)

  let rec enumerate (query : 'a Query) (iteratee : Iteratee<'a, 'b>) : 'b Query = monad {
    match iteratee with
    | Return x ->
      return x
    | Continue f ->
      do! State.modify ReadState.nextRow
      let! canRead = State.gets ReadState.canRead
      
      if canRead
        then let! x = query
             let xs = f <| Emit x
             return! enumerate query xs
        else return! enumerate query <| f End
  }

  let optional query =
    enumerate query Iteratee.tryHead

  let list query =
    enumerate query Iteratee.list

  let select = monad

  let apply (query : 'a Query) =
    State.eval query

module Command =
  let parameterize (command : IDbCommand) =
    let converter = System.ComponentModel.TypeDescriptor.GetConverter typeof<DbType>

    let resolveDbType (property : PropertyInfo) =
      if property.PropertyType.Name = "Instant"
        then DbType.DateTime2
        else downcast converter.ConvertFrom property.PropertyType.Name

    let mkParam (property : PropertyInfo) value =
      let param = command.CreateParameter ()
      param.ParameterName <- property.Name
      param.DbType        <- resolveDbType property
      param.Value         <- value
      param

    let addParam (collection : IDataParameterCollection) (prop, value) =
      mkParam prop value
      |> collection.Add
      |> ignore
      collection

    command.Parameters
    |> Reflect.linearize addParam

  let make text data : IDbCommand UnitOfWork = monad {
    let! context = TransactionContext.current
    let command  = context.Connection.CreateCommand ()
    command.Transaction <- context.Transaction
    command.CommandText <- text
    parameterize command data |> ignore

    return command
  }

module UnitOfWork =
  let write text data : int UnitOfWork = monad {
    use! command = Command.make text data

    try return command.ExecuteNonQuery ()
    with e -> return! UnitOfWorkError.withExecution e
  }

  let read text data query : 'a UnitOfWork = monad {
    use! command = Command.make text data    

    try
      (* Let's see if this IDataReader crosses the semantic barrier. *)
      use dataReader = command.ExecuteReader ()
      return dataReader
             |> ReadState.make
             |> Query.apply query
    with
      e -> return! UnitOfWorkError.withExecution e
  }

  let script = monad.fx