module VectorSpaceModel
open System
let createWeightVectors docs keySelector docIdSelector =
    let numberOfDocs = docs |> Seq.length |> float

    let docsWithKeys =
            let findKeys doc =
                let getKeys = fun (s : string) ->
                                s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
                                    |> Seq.map (fun s ->
                                                    let filteredChars = Seq.filter (fun c -> System.Char.IsLetterOrDigit c) s |> Seq.toArray
                                                    new string(filteredChars))
                keySelector doc |> getKeys
            docs |> Seq.map (fun d -> (docIdSelector d, findKeys d))

    let terms = docsWithKeys |> Seq.collect snd |> Seq.sort
    let distinctTerms = terms |> Seq.distinct

    let globalTermFrequencies = terms |> Seq.countBy id |> Map.ofSeq

    let localTermFrequencies = docsWithKeys |> Seq.collect (fun (d, keys) ->
                                                        keys |> Seq.countBy id
                                                             |> Seq.map (fun (key, count) -> (key, (d, float count))))
                                            |> Seq.groupBy (fun (key, _) -> key)
                                            |> Seq.map (fun (key, seq) -> (key, seq |> Seq.map (fun (_, (doc, count)) -> (doc, count)) |> Map.ofSeq))
                                            |> Map.ofSeq

    let weightVectors =
        docsWithKeys
            |> Seq.map fst
            |> Seq.map (fun docId ->
                            let weightVector = distinctTerms
                                                |> Seq.map (fun term ->
                                                                let localTermFreq = localTermFrequencies
                                                                                        |> Map.find term
                                                                                        |> Map.tryFind docId
                                                                                        |> function
                                                                                            | Some(freq) -> freq
                                                                                            | None -> 0.0
                                                                let globalTermFreq = globalTermFrequencies |> Map.find term |> float
                                                                let inverseDocFreq = Math.Log(numberOfDocs / globalTermFreq)
                                                                localTermFreq * inverseDocFreq)
                                                |> List.ofSeq
                            (docId, weightVector))
             |> List.ofSeq

    (distinctTerms |> List.ofSeq, weightVectors)