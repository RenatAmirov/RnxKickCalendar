module KickCalendar.Calendar

open System
open System.IO
open System.Text.Json

type Mark =
    | Empty = 0
    | Morning = 1
    | Complete = 2

type Journal =
    { BirthYear: int option
      HorizonYear: int option
      KeepOnTop: bool
      Days: Map<DateOnly, Mark>
      Years: Set<int>
      Goals: Map<int, string> }

let empty =
    { BirthYear = None
      HorizonYear = None
      KeepOnTop = false
      Days = Map.empty
      Years = Set.empty
      Goals = Map.empty }

let firstYear today journal = min today (defaultArg journal.BirthYear 1)
let lastYear today journal = max today (defaultArg journal.HorizonYear (min 9999 (today + 30)))
let mark day journal = Map.tryFind day journal.Days |> Option.defaultValue Mark.Empty
let goals year journal = Map.tryFind year journal.Goals |> Option.defaultValue ""

let changeDay (today: DateOnly) (day: DateOnly) clear journal =
    if day > today || day.Year < firstYear today.Year journal then journal
    else
        let next =
            match clear, mark day journal with
            | true, _ | _, Mark.Complete -> Mark.Empty
            | _, Mark.Empty when day = today -> Mark.Morning
            | _ -> Mark.Complete

        let days =
            if next = Mark.Empty then Map.remove day journal.Days
            else Map.add day next journal.Days

        { journal with Days = days }

let toggleYear today year journal =
    if year < firstYear today journal || year >= today then journal
    else
        let years =
            if Set.contains year journal.Years then Set.remove year journal.Years
            else Set.add year journal.Years

        { journal with Years = years }

let changeGoals year (value: string) journal =
    let goals =
        if String.IsNullOrWhiteSpace value then Map.remove year journal.Goals
        else Map.add year value journal.Goals

    { journal with Goals = goals }

let private options = JsonSerializerOptions(WriteIndented = true)

let load path =
    if File.Exists path then
        let journal = JsonSerializer.Deserialize<Journal>(File.ReadAllText path, options)

        let invalidYear year = year < 1 || year > 9999

        if isNull (box journal) || isNull (box journal.Days) || isNull (box journal.Years)
           || isNull (box journal.Goals) || Option.exists invalidYear journal.BirthYear
           || Option.exists invalidYear journal.HorizonYear then
            raise (InvalidDataException "Не удалось прочитать формат календаря.")

        journal
    else empty

let save path journal =
    let pending = path + ".tmp"
    let json = JsonSerializer.Serialize(journal, options)
    File.WriteAllText(pending, json)
    File.Move(pending, path, true)
