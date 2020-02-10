let jsonString = """
{
  "glossary": {
    "title": "example glossary",
		"GlossDiv": {
      "title": "S",
      "id": 24,
			"GlossList": {
        "GlossEntry": {
          "ID": "SGML",
					"SortAs": "SGML",
					"GlossTerm": "Standard Generalized Markup Language",
					"Acronym": "SGML",
					"Abbrev": "ISO 8879:1986",
					"GlossDef": {
            "para": "A meta-markup language, used to create markup languages such as DocBook.",
						"GlossSeeAlso": ["GML", "XML"]
          },
					"GlossSee": "markup"
        }
      }
    }
  }
}
"""

let json2String = """
[
  { "id": 1, "isCompany": false, "firstName": "Bill"    , "lastName": "Gates" },
  { "id": 2, "isCompany": false, "firstName": "Melinda" , "lastName": "Gates" },

  { "id": 3, "isCompany": true , "companyName": "Microsoft" },
  { "id": 4, "isCompany": true , "companyName": "Apple"     },
  { "id": 5, "isCompany": true , "companyName": "Google"    }
]"""


open System.Buffers
open System.IO
open System.Text
open System.Text.Json

open FsJsonNavigation.System.Text.Json
open FsJsonNavigation.JsonNavigation.JsonNavigate

[<EntryPoint>]
let main argv =
  let toUtf8 (s : string) = ReadOnlySequence<byte> (Encoding.UTF8.GetBytes s)
  let fromUtf8 (bs : byte array) = Encoding.UTF8.GetString bs
               
  let parseJson s = 
    let reader = new Utf8JsonReader (toUtf8 s)
    JsonSerializer.read reader

  let serializeJson j = 
    use stream = new MemoryStream ()
    do
      use writer = new Utf8JsonWriter (stream)
      JsonSerializer.write writer j
    fromUtf8 (stream.ToArray ())

  let parse s = 
    let j = parseJson s
    let k = serializeJson j
    let r = parseJson k
    j, r

  do
    let json, rjson = parse jsonString

    printfn "%A" json
    printfn "%A" (json = rjson) // Roundtrip

    printfn "%A" <| jnstring json jnroot?glossary?title
    printfn "%A" <| jnstring json jnroot?glossary.[0]
    printfn "%A" <| jnstring json jnroot?glossary?GlossDiv?id
    printfn "%A" <| jnstring json jnroot?glossary?missing
    printfn "%A" <| jnstring json jnroot?glossary?GlossDiv?GlossList?GlossEntry?GlossDef?GlossSeeAlso.[0]
    printfn "%A" <| jnstring json jnroot?glossary?GlossDiv?GlossList?GlossEntry?GlossDef?GlossSeeAlso.[3]

    ()

  do
    let json, rjson = parse json2String

    printfn "%A" json
    printfn "%A" (json = rjson) // Roundtrip

    printfn "%A" <| jnstring json jnroot.[0]?firstName
    printfn "%A" <| jnstring json (jnroot.Pick jnroot?companyName).[0]
    printfn "%A" <| jnstring json (jnroot.Filter jnroot?isCompany).[0]?companyName

  0
