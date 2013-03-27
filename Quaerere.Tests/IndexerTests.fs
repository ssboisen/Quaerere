namespace Quaerere.Tests

open Xunit
open FsUnit.Xunit
open Xunit.Extensions
open Ploeh.AutoFixture
open Ploeh.AutoFixture.Xunit
open Moq
open Quaerere
open System

module IndexerTests =

[<Theory>]
[<AutoData>]
let ``indexer should index correctly`` (indexer : Indexer) =
    let docs = [ "hello world" ] |> Seq.ofList
    let actual = indexer.IndexDocuments(docs, fun d -> d)
    actual.Count |> should equal 2
    actual |> Map.containsKey "hello" |> should be True
    actual |> Map.containsKey "world" |> should be True

[<Theory>]
[<AutoData>]
let ``indexer should ignore whitespace`` (indexer : Indexer) =
    let docs = [ "    hello   world  " ] |> Seq.ofList
    let actual = indexer.IndexDocuments(docs, fun d -> d)
    actual.Count |> should equal 2
    actual |> Map.containsKey "hello" |> should be True
    actual |> Map.containsKey "world" |> should be True

[<Theory>]
[<AutoData>]
let ``indexer should ignore any none alphanumeric characters`` (indexer : Indexer) =
    let docs = [ "hello! world?" ] |> Seq.ofList
    let actual = indexer.IndexDocuments(docs, fun d -> d)
    actual.Count |> should equal 2
    actual |> Map.containsKey "hello" |> should be True
    actual |> Map.containsKey "world" |> should be True