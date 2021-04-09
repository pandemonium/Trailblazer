namespace Cewn

open System
open System.Security.Cryptography
open FSharpPlus
open FSharpPlus.Data
open NodaTime


type Digest = private Sha256 of string

module Digest =
  let hash : string -> byte array =
    use sha = SHA256.Create ()
    in Text.Encoding.UTF8.GetBytes
       >> sha.ComputeHash

  let base64 =
    hash >> Convert.ToBase64String 
         >> Sha256

  let hex =
    hash >> map (sprintf "%02X")
         >> String.Concat
         >> Sha256

  let asString (Sha256 digest) = digest

type Blockchain = 
  private Ledger of BlockchainInfo

and BlockchainInfo =
  { Pending : Transaction list
    Blocks  : Block NonEmptyList }

and Transaction =
  private MkTransaction of TransactionInfo

and TransactionInfo =
  { Sender   : Digest
    Receiver : Digest
    Amount   : uint }

and Block =
  private Genesis      of proof : int * hash : Digest
        | Transactions of BlockInfo

and BlockInfo =
  { Index        : int
    Timestamp    : Instant
    Transactions : Transaction list
    Proof        : int
    PreviousHash : Digest }

module Transaction =
  let make sender receiver amount =
    { Sender   = sender
      Receiver = receiver
      Amount   = amount }
    |> MkTransaction

  let asText (MkTransaction tx) =
    sprintf "%s%s%d"
    <| Digest.asString tx.Sender 
    <| Digest.asString tx.Receiver 
    <| tx.Amount

module Block =
  let genesis = curry Genesis

  let index = function Genesis _          -> 0
                     | Transactions block -> block.Index

  let proof = function Genesis (proof, _) -> proof
                     | Transactions block -> block.Proof

  let computeDigest (info : BlockInfo) : Digest =
    [ info.Index.ToString ()
      info.Timestamp.ToString ()
      List.map Transaction.asText info.Transactions |> String.Concat
      info.Proof.ToString ()
      info.PreviousHash |> Digest.asString ]
    |> String.Concat
    |> Digest.base64

  let hash = function Genesis (_, hash)  -> hash
                    | Transactions block -> computeDigest block

  let transactions timestamp transactions proof previous =
    { Index        = index previous + 1
      Timestamp    = timestamp
      Transactions = transactions
      Proof        = proof
      PreviousHash = hash previous }
    |> Transactions

module Blockchain =
  let empty rootHash =
    { Pending = []
      Blocks  = Block.genesis 100 rootHash
                |> NonEmptyList.singleton } 
    |> Ledger

  let insertTransaction tx (Ledger chain) =
    { chain with Pending = tx :: chain.Pending }
    |> Ledger

  let commmitBlock timestamp proof (Ledger chain) =
    let insertBlock blocks =
      NonEmptyList.head blocks
      |> Block.transactions timestamp chain.Pending proof
      |> flip NonEmptyList.cons blocks
    in { chain with Pending = []
                    Blocks  = insertBlock chain.Blocks }
       |> Ledger

  let head (Ledger chain) =
    chain.Blocks |> NonEmptyList.head

  let verifyProof last =
    sprintf "%d%d" last 
    >> Digest.hex
    >> Digest.asString
    >> String.startsWith "0000"

  let proveWork lastProof =
    let rec loop proof =
      if not <| verifyProof lastProof proof 
        then loop <| 1 + proof
        else proof
    in loop 0

type Node = 
  private MkNode of self : Digest * network : Digest

module Node =
  let ownAddress (MkNode (own, _)) = own

  let networkAddress (MkNode (_, network)) = network

  let mine chain node =
    let head      = Blockchain.head chain
    let lastProof = Block.proof head
    let proof     = Blockchain.proveWork lastProof
    let now : Instant = failwith "hi"

    Transaction.make (networkAddress node) (ownAddress node) 1u
    |> flip Blockchain.insertTransaction chain
    |> Blockchain.commmitBlock now proof