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

let calculateDocumentWeightVectors docsWithTerms termInfo =
    let numberOfDocs, terms, distinctTerms = termInfo
    let globalTermFrequencies = terms |> Seq.countBy id |> Seq.map (fun (t, c) -> (t, float c)) |> Map.ofSeq

    let localTermFrequencies = docsWithTerms |> Seq.collect (fun (d, keys) ->
                                                        keys |> Seq.countBy id
                                                             |> Seq.map (fun (key, count) -> (key, (d, float count))))
                                        |> Seq.groupBy fst
                                        |> Seq.map (fun (key, seq) -> (key, seq |> Seq.map snd |> Map.ofSeq))
                                        |> Map.ofSeq

    let termIndexes = distinctTerms |> Seq.mapi (fun i term -> (term, i)) |> Map.ofSeq

    let numOfDistinctTerms = distinctTerms |> Seq.length

    docsWithTerms
        |> Seq.map (fun (docId, terms) ->
                        let weightVector = Array.create numOfDistinctTerms 0.0
                        terms
                            |> Seq.map (fun term ->
                                            let freq = localTermFrequencies.Item term
                                                        |> Map.tryFind docId
                                                        |> function
                                                            | Some(localTermFreq) -> 
                                                                let globalTermFreq = globalTermFrequencies.Item term
                                                                let inverseDocFreq = log (numberOfDocs / globalTermFreq)
                                                                localTermFreq * inverseDocFreq
                                                            | None -> 0.0
                                            (term, freq))
                                            
                            |> Seq.iter (fun (term, freq) -> weightVector.[termIndexes.Item term] <- freq)
                        (docId, weightVector))
       |> List.ofSeq

let generateQueryWeightVector queryTerms documentTerms =
    let querySet = queryTerms |> Set.ofSeq
    seq { 
        for term in documentTerms ->
        if querySet.Contains term then 1.0 else 0.0
    }

let calculateSimilarity queryWeightVector docWeightVector =
    let rss (xs : seq<float>) = xs |> Seq.sumBy square |> sqrt
    let dotProduct = docWeightVector
                        |> Seq.zip queryWeightVector
                        |> Seq.sumBy (fun (d, q) -> d * q)
    let norms = rss docWeightVector * rss queryWeightVector
    if norms > 0.0 then
        dotProduct / norms
    else
        0.0
