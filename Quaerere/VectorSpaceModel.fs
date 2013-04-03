module VectorSpaceModel

let square x = x * x

let extractWords (s : string) =
    s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

let extractAllTerms docs contentSelector docIdSelector =
    let findTerms doc = contentSelector doc |> extractWords
    docs |> Seq.map (fun d -> (docIdSelector d, findTerms d))

let extractTermInfo docsWithTerms =
    let numberOfDocs = docsWithTerms |> Seq.length |> float
    let terms = docsWithTerms |> Seq.collect snd |> Seq.sort |> List.ofSeq
    let distinctTerms = terms |> Seq.distinct |> List.ofSeq

    (numberOfDocs, terms, distinctTerms)

let calculateDocumentWeightVectors docsWithTerms termInfo =
    let numberOfDocs, terms, distinctTerms = termInfo
    let globalTermFrequencies = terms |> Seq.countBy id |> Seq.map (fun (t, c) -> (t, float c)) |> Map.ofSeq

    let localTermFrequencies = docsWithTerms |> Seq.collect (fun (d, keys) ->
                                                        keys |> Seq.countBy id
                                                             |> Seq.map (fun (key, count) -> (key, (d, float count))))
                                        |> Seq.groupBy fst
                                        |> Seq.map (fun (key, seq) -> (key, seq |> Seq.map snd |> Map.ofSeq))
                                        |> Map.ofSeq

    docsWithTerms
        |> Seq.map (fun (docId, _) ->
                        let weightVector = distinctTerms
                                            |> Seq.map (fun term ->
                                                            let localTermFreq = localTermFrequencies.Item term
                                                                                    |> Map.tryFind docId
                                                                                    |> function
                                                                                        | Some(freq) -> freq
                                                                                        | None -> 0.0
                                                            let globalTermFreq = globalTermFrequencies.Item term
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
    let rss xs = xs |> Seq.sumBy square |> sqrt
    let dotProduct = docWeightVector
                        |> Seq.zip queryWeightVector
                        |> Seq.sumBy (fun (d, q) -> d * q)
    let norms = rss docWeightVector * rss queryWeightVector
    dotProduct / norms

//Example
let docsWithTerms = extractAllTerms [| "hello world"; "yo mama"; "hello other world"; "hello simon" |] id id
let termInfo = extractTermInfo docsWithTerms
let query = "mama hello"
let queryTerms = extractWords query

let docWeightVectors = calculateDocumentWeightVectors docsWithTerms termInfo

let _, _, distinctTerms = termInfo

let queryVector = generateQueryWeightVector queryTerms distinctTerms

let querySimilarity = calculateSimilarity queryVector

let topDocs = docWeightVectors
                |> Seq.map (fun (d, wv) -> d, querySimilarity wv)
                |> Seq.sortBy (fun (d, s) -> -s - 1.0)
                |> List.ofSeq