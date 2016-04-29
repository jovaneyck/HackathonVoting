#r @"..\packages\FSharp.Data.2.2.5\lib\net40\FSharp.Data.dll"
open FSharp.Data

[<Literal>] 
let pathToCsv = "C:\Users\jo.vaneyck\Desktop\AE Hackathon 2016.csv"
let pointsForFirstPlace = 5
let pointsForSecondPlace = 3
let pointsForThirdPlace = 1

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
    doubleEntries 
    |> Seq.iter (fun (name, nbVotes) -> printfn "%s voted %d times" name nbVotes)

type Category =
    | BusinessValue
    | Technical
type Vote = { Category : Category; Team : string; Points : int}

let parse category points vote =
    match vote with
    | "" -> None
    | v -> Some { Category = category; Team = vote; Points = points }

let pointsPerCategory =
    responses
    |> Seq.collect (fun r -> 
        [
            r.``My #1 technical vote`` |> parse Technical pointsForFirstPlace
            r.``My #2 technical vote`` |> parse Technical pointsForSecondPlace
            r.``My #1 business value vote`` |> parse BusinessValue pointsForFirstPlace
            r.``My #2 business value vote``|> parse BusinessValue pointsForSecondPlace
        ])
    |> Seq.choose id
    |> Seq.groupBy (fun vote -> vote.Category)

let total (points : Vote seq) = 
    points
    |> Seq.groupBy (fun v -> v.Team)
    |> Seq.map (fun (team, votes) -> 
        (team, 
            votes 
            |> Seq.map(fun v -> v.Points) 
            |> Seq.reduce (+)))

let totalsPerCategory = 
    pointsPerCategory 
    |> Seq.map(fun (cat, votes) -> 
        (cat, total votes))

let totals =
    totalsPerCategory
    |> Seq.collect (fun (_, votes) -> votes)
    |> Seq.groupBy (fun (team, _) -> team)
    |> Seq.map (fun (team, v) -> (team, v |> Seq.map(fun (_, points) -> points) |> Seq.reduce (+)))
    |> Seq.sortByDescending(fun (_,points) -> points)

printfn "And the results are in..."
printfn "%A" totals