open VectorSpaceModel
open System
open System.Data
open System.Data.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq

type dbSchema = SqlDataConnection<"Data Source=.\;Initial Catalog=PTMatch;Integrated Security=SSPI;">
let db = dbSchema.GetDataContext()

let lls = query {
    for ll in db.LocalizedLabels do
    where (ll.LocaleName.Equals("da-DK"))
    take 10000
    select (ll.LabelId, ll.LabelText)
}

let docs = lls |> Seq.toList

//Example
let docsWithTerms = extractAllTerms docs snd fst
let numberOfDocs, terms, distinctTerms = extractTermInfo docsWithTerms
let query = "glemmer fordybet"
let queryTerms = extractWords query

let localeTermFrequencies = calculateLocaleTermFrequencies docsWithTerms
let globalTermFrequencies = calculateGlobalTermFrequencies terms
let inverseDocFreq = calculateInverseDocFrequency globalTermFrequencies numberOfDocs

let sw = System.Diagnostics.Stopwatch.StartNew()
let docWeightVectors = calculateDocumentWeightVectors docsWithTerms localeTermFrequencies inverseDocFreq
let elapsedBuild = sw.ElapsedMilliseconds

let queryWeights = generateQueryWeights queryTerms inverseDocFreq

let querySimilarity = calculateSimilarity queryWeights
sw.Restart()
let topDocs = docWeightVectors
                |> Seq.map (fun (d, wv) -> d, querySimilarity wv)
                |> Seq.filter (fun (_, s) -> s > 0.0)
                |> Seq.sortBy (fun (_, s) -> -s - 1.0)
                |> List.ofSeq
let elapsedSearch = sw.ElapsedMilliseconds

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
