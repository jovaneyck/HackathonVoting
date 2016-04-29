#r @"..\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
open FSharp.Data

[<Literal>] 
let pathToCsv = "C:\Users\jo.vaneyck\Desktop\AE Hackathon 2016.csv"

type ResultsType = CsvProvider<pathToCsv>

let csv = new ResultsType()
let responses = csv.Rows

responses
|> Seq.length
|> printfn "Parsing %d responses..."

let doubleEntries = 
    responses
    |> Seq.map(fun r -> r.``Your name``)
    |> Seq.countBy id
    |> Seq.filter (fun (_, votes) -> votes > 1)
    |> Seq.toList

if not doubleEntries.IsEmpty then
    printfn "DOUBLE ENTRIES FOUND!"
    doubleEntries |> Seq.iter (fun (name, nbVotes) -> printfn "%s voted %d times" name nbVotes)

