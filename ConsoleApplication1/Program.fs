﻿open VectorSpaceModel
open System
open System.Data
open System.Data.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq

type dbSchema = SqlDataConnection<"Data Source=.\MSSQLSERVER2012;Initial Catalog=PTMatch;Integrated Security=SSPI;">
let db = dbSchema.GetDataContext()

let lls = query {
    for ll in db.LocalizedLabels do
    where (ll.LocaleName.Equals("da-DK"))
    select (ll.LabelId, ll.LabelText)
}

let docs = lls |> Seq.toList

//Example
let docsWithTerms = extractAllTerms docs snd fst
let query = "analyser aftalt"
let queryTerms = extractWords query
let numberOfDocs = docs.Length |> float
let localeTermFrequencies = calcTermFrequencies docsWithTerms
let numOfDocsPerTerm = calcNumOfDocsPerTerm docsWithTerms
let inverseDocFreq = calculateInverseDocFrequency numOfDocsPerTerm numberOfDocs

let sw = System.Diagnostics.Stopwatch.StartNew()
let docWeightVectors = calculateDocumentWeightVectors docsWithTerms localeTermFrequencies inverseDocFreq
let elapsedBuild = sw.ElapsedMilliseconds

let queryWeights = generateQueryWeights queryTerms inverseDocFreq

let querySimilarity = calculateSimilarity queryWeights
sw.Restart()
let topDocs = docWeightVectors
                |> Seq.map (fun (d, docWeights) -> d, querySimilarity docWeights)
                |> Seq.filter (fun (_, s) -> s > 0.0)
                |> Seq.sortBy (fun (_, s) -> -s - 1.0)
                |> List.ofSeq

let thedoc = topDocs |> Seq.filter (fun (d, _) -> d.Equals(5699))
let elapsedSearch = sw.ElapsedMilliseconds

[<EntryPoint>]
let main argv = 
    let searchResult = topDocs |> Seq.map (fun (docId, freq) -> (docs |> Seq.find (fun d -> (fst d).Equals(docId)), freq) )
    printfn "%A" searchResult
    printfn "%A" elapsedBuild
    printfn "%A" elapsedSearch
    0 // return an integer exit code
