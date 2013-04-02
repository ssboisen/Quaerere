module VectorSpaceModel

let square x = x * x

let extractWords (s : string) =
    s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun s ->
                        let filteredChars = Seq.filter (fun c -> System.Char.IsLetterOrDigit c) s |> Seq.toArray
                        new string(filteredChars))

let indexDocs docs keySelector docIdSelector =
    let findKeys doc =
        keySelector doc |> extractWords
    docs |> Seq.map (fun d -> (docIdSelector d, findKeys d))

let indexMetaData docIndex =
    let numberOfDocs = docIndex |> Seq.length |> float
    let terms = docIndex |> Seq.collect snd |> Seq.sort |> List.ofSeq
    let distinctTerms = terms |> Seq.distinct |> List.ofSeq

    (numberOfDocs, terms, distinctTerms)

let calculateDocumentWeightVectors docIndex indexMetaData =
    let numberOfDocs, terms, distinctTerms = indexMetaData
    let globalTermFrequencies = terms |> Seq.countBy id |> Seq.map (fun (t, c) -> (t, float c)) |> Map.ofSeq

    let localTermFrequencies = docIndex |> Seq.collect (fun (d, keys) ->
                                                        keys |> Seq.countBy id
                                                             |> Seq.map (fun (key, count) -> (key, (d, float count))))
                                        |> Seq.groupBy fst
                                        |> Seq.map (fun (key, seq) -> (key, seq |> Seq.map snd |> Map.ofSeq))
                                        |> Map.ofSeq

    docIndex
        |> Seq.map (fun (docId, _) ->
                        let weightVector = distinctTerms
                                            |> Seq.map (fun term ->
                                                            let localTermFreq = localTermFrequencies
                                                                                    |> Map.find term
                                                                                    |> Map.tryFind docId
                                                                                    |> function
                                                                                        | Some(freq) -> freq
                                                                                        | None -> 0.0
                                                            let globalTermFreq = globalTermFrequencies |> Map.find term
                                                            let inverseDocFreq = log (numberOfDocs / globalTermFreq)
                                                            localTermFreq * inverseDocFreq)
                                            |> List.ofSeq
                        (docId, weightVector))
       |> List.ofSeq

let generateQueryWeightVector queryTerms documentTerms =
    let querySet = queryTerms |> Set.ofSeq
    seq { 
        for term in documentTerms ->
        if querySet.Contains term then 1.0 else 0.0
    }

let calculateSimilarity queryWeightVector docWeightVector =
    let nominator = docWeightVector
                        |> Seq.zip queryWeightVector
                        |> Seq.sumBy (fun (d, q) -> d * q)
    let denominator =
                (docWeightVector
                    |> Seq.sumBy square
                    |> sqrt) *
                (queryWeightVector
                    |> Seq.sumBy square
                    |> sqrt)
    nominator / denominator

//Example
let docIndex = indexDocs [| "hello world"; "yo mama"; "hello other world"; "hello simon" |] id id
let metaData = indexMetaData docIndex
let query = "mama hello"
let queryTerms = extractWords query |> Seq.sort

let docWeightVectors = calculateDocumentWeightVectors docIndex metaData

let _, _, distinctTerms = metaData

let queryVector = generateQueryWeightVector queryTerms distinctTerms

let querySimilarity = calculateSimilarity queryVector

let topDocs = docWeightVectors
                |> Seq.map (fun (d, wv) -> d, querySimilarity wv)
                |> Seq.sortBy (fun (d, s) -> -s - 1.0)
                |> List.ofSeq