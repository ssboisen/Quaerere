namespace Quaerere
open System
type Indexer() =
    member private x.IndexDocument(d, keySelector) =
                            let getKeys = fun (s : string) -> 
                                            s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
                                                |> Seq.map (fun s ->
                                                                let filteredChars = Seq.filter (fun c -> System.Char.IsLetterOrDigit c) s |> Seq.toArray
                                                                new string(filteredChars))
                            keySelector d |> getKeys
    
    member x.IndexDocuments(ds, keySelector) =
                            let keys = ds |> Seq.collect (fun d -> 
                                                x.IndexDocument(d, keySelector)
                                                    |> Seq.map (fun k -> (k, d)))
                            keys
                               |> Seq.groupBy (fun (k, d) -> k)
                               |> Seq.map (fun (k, vs) -> 
                                                let vs = vs |> Seq.map (fun (_, v) -> v)
                                                (k, vs))
                               |> Map.ofSeq