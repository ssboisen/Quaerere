open VectorSpaceModel
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
    take 1000
    select (ll.LabelId, ll.LabelText)
}

let docs = lls |> Seq.toList

//Example
let docsWithTerms = extractAllTerms docs snd fst
let termInfo = extractTermInfo docsWithTerms
let query = "glemmer fordybet"
let queryTerms = extractWords query

let docWeightVectors = calculateDocumentWeightVectors docsWithTerms termInfo

let _, _, distinctTerms = termInfo

let queryVector = generateQueryWeightVector queryTerms distinctTerms

let querySimilarity = calculateSimilarity queryVector

let topDocs = docWeightVectors
                |> Seq.map (fun (d, wv) -> d, querySimilarity wv)
                |> Seq.filter (fun (_, s) -> s > 0.0)
                |> Seq.sortBy (fun (_, s) -> -s - 1.0)
                |> List.ofSeq

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
