module Parsers

open FParsec

type Parser<'t> = Parser<'t, unit>

type PhoneNumber =
    { AreaCode: int
      Prefix: int
      LineNumber: int }
    
type State = OH | KY | IN | TN 
    
type StreetAddress =
    { Street: int * string
      City: string
      State: State
      Zipcode: int }
    
type ContactInfo =
    | Phone of PhoneNumber
    | Address of StreetAddress
    
let createPhoneNumber ac pf ln =
    { AreaCode = ac
      Prefix = pf
      LineNumber = ln }
    
let createStreetAddress str c st zip =
    { Street = str
      City = c
      State = st
      Zipcode = zip }

module PhoneNumberParser =
    let popenparens = pchar '('
    let pcloseparens = pchar ')'
    let pdash: Parser<_> = pchar '-'
    let pareacode =
        between popenparens pcloseparens pint32
        <|> pint32 .>> pdash
    let pprefix = pint32 .>> pdash
    let plinenumber = pint32

    let pphonenumber =
        pipe3 pareacode
              pprefix
              plinenumber
              createPhoneNumber
        |>> Phone
              
module AddressParser =
    let isStreetName c = isLetter c || isAnyOf " ." c
    let pcomma = pchar ',' .>> spaces
    let pstreet =
        pint32
        .>> spaces
        .>>. (many1Satisfy isStreetName)
        .>> pcomma
        |> attempt
    let pcity = many1Satisfy isLetter .>> pcomma
    let pstate =
        choice
            [ stringReturn "OH" OH
              stringReturn "KY" KY
              stringReturn "IN" IN
              stringReturn "TN" TN ]
        .>> spaces
    let pzipcode = pint32

    let paddress =
        pipe4 pstreet
              pcity
              pstate
              pzipcode
              createStreetAddress
        |>> Address
        
let pcontactinfo =
    [ AddressParser.paddress; PhoneNumberParser.pphonenumber ]
    |> List.map (fun p -> p .>> spaces)
    |> choice
    |> many

module OPP =
    type Operator = Addition | Subtraction | Multiplication | Division

    type Value =
        | Number of float
        | Variable of string
        | Operation of Operation
        | Negative of Value
    
    and Operation =
        { Operator : Operator
          LeftOperand : Value
          RightOperand : Value }
    
    let createOperation op x y =
        { Operator = op; LeftOperand = x; RightOperand = y } |> Operation
        
    let ws = spaces
    let str_ws s = pstring s >>. ws
    let pvariable = many1Satisfy isLetter |>> Variable
    let pnumber = pfloat |>> Number
    let opp = new OperatorPrecedenceParser<Value,unit,unit>()
    let poperation = opp.ExpressionParser
    let term = (pnumber <|> pvariable .>> ws) <|> between (str_ws "(") (str_ws ")") poperation
    opp.TermParser <- term

    type Assoc = Associativity

    opp.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, createOperation Addition))
    opp.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, createOperation Subtraction))
    opp.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, createOperation Multiplication))
    opp.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, createOperation Division))
    opp.AddOperator(PrefixOperator("-", ws, 3, true, Negative))