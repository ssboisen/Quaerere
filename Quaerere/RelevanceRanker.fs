namespace Quaerere

type RelevanceRanker<'Doc>(index : Map<string, seq<'Doc>>) =
        member private x.FindFrequencies() =
            ()