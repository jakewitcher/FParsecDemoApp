
#I @"C:/Users/Jake/.nuget/packages/fparsec/1.1.0-rc/lib/netstandard2.0"
#r "FParsecCS.dll"
#r "FParsec.dll"

#load "Parsers.fs"

open FParsec
open Parsers
open Parsers.PhoneNumberParser
open Parsers.AddressParser
open Parsers.OPP

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errMsg, _, _) -> printfn "Failure: %s" errMsg