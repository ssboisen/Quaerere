module VectorSpaceModel

let square x = x * x

let extractWords (s : string) =
    let s = System.Text.RegularExpressions.Regex.Replace(s, @"<[^>]*>", System.String.Empty)
    s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.filter (fun w -> System.Text.RegularExpressions.Regex.IsMatch(w, @"^\p{L}+$"))
    |> Seq.map (fun s -> 
                    s.Trim().ToLower())
    |> List.ofSeq

let extractAllTerms docs contentSelector docIdSelector =
    let findTerms doc = contentSelector doc |> extractWords
    docs 
        |> Seq.map (fun d -> (docIdSelector d, findTerms d))
        |> Seq.filter (fun (_, terms) -> terms |> List.length > 0)
        |> List.ofSeq

let extractTermInfo docsWithTerms =
    let numberOfDocs = docsWithTerms |> Seq.length |> float
    let terms = docsWithTerms |> Seq.collect snd |> Seq.sort |> List.ofSeq
    let distinctTerms = terms |> Seq.distinct |> List.ofSeq

    (numberOfDocs, terms, distinctTerms)

let calculateLocaleTermFrequencies docsWithTerms =
    docsWithTerms   |> Seq.collect (fun (d, keys) ->
                                    keys |> Seq.countBy id
                                         |> Seq.map (fun (key, count) -> (key, (d, float count))))
                    |> Seq.groupBy fst
                    |> Seq.map (fun (key, seq) -> (key, seq |> Seq.map snd |> Map.ofSeq))
                    |> Map.ofSeq

let calculateGlobalTermFrequencies terms = 
    terms |> Seq.countBy id |> Seq.map (fun (t, c) -> (t, float c)) |> Map.ofSeq

let calculateInverseDocFrequency globalTermFrequencies numberOfDocs term =
    let globalTermFreq = Map.find term globalTermFrequencies
    log (numberOfDocs / globalTermFreq)

let calculateDocumentWeightVectors docsWithTerms localTermFrequencies inverseDocFreq =
    docsWithTerms
        |> Seq.map (fun (docId, terms) ->
                        let weights = terms
                                        |> Seq.map (fun term ->
                                                        let localTermFreq = localTermFrequencies
                                                                                |> Map.find term
                                                                                |> Map.find docId
                                                        let inverseDocFreq = inverseDocFreq term
                                                        let freq : float = localTermFreq * inverseDocFreq
                                                        (term, freq))
                                        |> Map.ofSeq
                        (docId, weights))
       |> List.ofSeq

let generateQueryWeights queryTerms inverseDocFreq =
    queryTerms |> Seq.map (fun term -> (term, inverseDocFreq term))

let calculateSimilarity queryWeights docWeightMap =
    let rss (xs : seq<float>) = xs |> Seq.sumBy square |> sqrt
    let dotProduct = queryWeights
                        |> Seq.map (fun (term, qFreq) -> docWeightMap 
                                                            |> Map.tryFind term 
                                                            |> function
                                                                | Some(dFreq) -> (dFreq, qFreq)
                                                                | None -> (0.0, qFreq))
                        |> Seq.sumBy (fun (d, q) -> d * q)

    let norms = rss (Seq.map snd queryWeights) * rss (docWeightMap |> Seq.map (fun kvp -> kvp.Value))
    if norms > 0.0 then
        dotProduct / norms
    else
        0.0
