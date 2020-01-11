open System
open FParsec
open Parsers
open Parsers.PhoneNumberParser
open Parsers.AddressParser

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errMsg, _, _) -> printfn "Failure: %s" errMsg

[<EntryPoint>]
let main argv =
    let text = "(419)-252-2799 513-255-6183 1123 Walnut St., Covington, KY 41012 (859)-318-4719 465 Kirk Dr., Findlay, OH 45240 124 Pineview Ln., Indianapolis, IN 46082"
    test pcontactinfo text
    0 // return an integer exit code
