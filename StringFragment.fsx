open System
open System.Text

type 
    StringFragment =
    | Empty
    | Fragment of string
    | Concat of StringFragment * StringFragment    
    override x.ToString() =
        let rec flatten frag (sb : StringBuilder) =
            match frag with
            | Empty -> sb
            | Fragment(s) -> sb.Append(s)
            | Concat(s1, s2) -> sb |> flatten s1 |> flatten s2
        (StringBuilder() |> flatten x).ToString()




