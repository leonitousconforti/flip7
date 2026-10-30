// Writes FableEntry.g.fs, the entry Fable compiles the tests through, which
// hands Runner.run every Fact, and every Theory once per InlineData, that the
// project compiles under Fable, in its order, leaving out those in
// #if !FABLE_COMPILER blocks. It stops at a test attribute or an #if it cannot
// read rather than leave a test out. CI's fable job runs the tests on Node so:
//   dotnet fsi test/FableEntry.fsx
//   dotnet fable test/Flip7.Test.fsproj --outDir <dir>
//   node <dir>/FableEntry.g.js, beside a package.json of { "type": "module" }
// On macOS, jsc -m runs a module that sets globalThis.console = { log: print }
// and imports the entry.

open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq

let project = Path.Combine(__SOURCE_DIRECTORY__, "Flip7.Test.fsproj")
let entry = Path.Combine(__SOURCE_DIRECTORY__, "FableEntry.g.fs")

// Every file the project compiles under Fable but the entry, in its order
let files: string list = [
    for compile in XDocument.Load(project).Descendants(XName.Get "Compile") do
        let file = compile.Attribute(XName.Get "Include").Value

        match compile.Attribute(XName.Get "Condition") with
        | _ when file = Path.GetFileName entry -> ()
        | null -> file
        | condition when condition.Value = "'$(FABLE_COMPILER)' == 'true'" -> file
        | condition when condition.Value = "'$(FABLE_COMPILER)' != 'true'" -> ()
        | condition -> failwith $"{project}: cannot tell whether Fable compiles {file} under {condition.Value}"
]

// The arguments of an InlineData, split at the commas outside quotes
let arguments (data: string) : string list =
    let parts = ResizeArray<string>()
    let current = StringBuilder()
    let mutable quoted = false
    let mutable escaped = false

    for c in data do
        if escaped then
            escaped <- false
        elif c = '\\' && quoted then
            escaped <- true
        elif c = '"' then
            quoted <- not quoted

        if c = ',' && not quoted then
            parts.Add(current.ToString().Trim())
            current.Clear() |> ignore
        else
            current.Append c |> ignore

    parts.Add(current.ToString().Trim())
    List.ofSeq parts

let directive = Regex @"^\s*#(if|elif|else|endif)\b(.*)$"
let attribute = Regex @"^\s*\[<.*\b(Fact|Theory|InlineData)\b"
let marker = Regex @"^\[<(Fact|Theory)>\]$"
let inlineData = Regex @"^\[<InlineData(?: (?<data>.+)|\((?<data>.+)\))>\]$"
let testName = Regex @"^let ``([^`]+)``"

// Each test case in a file: its test, named with its module, its InlineData,
// empty for a Fact, and the call that runs it
let casesIn (file: string) : (string * string * string) list =
    let path = Path.Combine(__SOURCE_DIRECTORY__, file)
    let lines = File.ReadAllLines path
    let fail (index: int) (problem: string) =
        failwith $"{path}({index + 1}): {problem}"

    let moduleName =
        lines
        |> Array.tryFind (fun line -> line.StartsWith "module ")
        |> Option.map (fun line -> line.Substring("module ".Length).Trim())

    let cases = ResizeArray<string * string * string>()
    // Whether each enclosing #if is compiled under Fable, innermost first
    let mutable compiled: bool list = []
    // The Fact or Theory above the lines read, and the InlineData under it
    let mutable marked: (string * string list list) option = None

    for index, line in Seq.indexed lines do
        let isDirective = directive.Match line

        if isDirective.Success then
            if marked.IsSome then
                fail index "an #if between a test's attributes and its let"

            match isDirective.Groups[1].Value, isDirective.Groups[2].Value.Trim(), compiled with
            | "if", "FABLE_COMPILER", _ -> compiled <- true :: compiled
            | "if", "!FABLE_COMPILER", _ -> compiled <- false :: compiled
            | "else", "", innermost :: outer -> compiled <- not innermost :: outer
            | "endif", "", _ :: outer -> compiled <- outer
            | _ -> fail index $"cannot read {line.Trim()}"
        elif List.forall id compiled then
            let isMarker = marker.Match line
            let isInlineData = inlineData.Match line

            match marked with
            | None when isMarker.Success -> marked <- Some(isMarker.Groups[1].Value, [])
            | Some("Theory", data) when isInlineData.Success ->
                marked <- Some("Theory", data @ [ arguments isInlineData.Groups["data"].Value ])
            | _ when attribute.IsMatch line -> fail index $"cannot read {line.Trim()}"
            | Some(kind, data) ->
                let isTest = testName.Match line

                match moduleName, kind, data with
                | _ when not isTest.Success -> fail index $"a {kind} on something other than a let ``test``"
                | None, _, _ -> fail index "a test outside a module"
                | _, "Theory", [] -> fail index "a Theory with no InlineData"
                | Some moduleName, _, _ ->
                    let name = isTest.Groups[1].Value
                    let call = $"{moduleName}.``{name}``"

                    if kind = "Fact" then
                        cases.Add($"{moduleName}: {name}", "", $"{call} ()")

                    for arguments in data do
                        let applied = arguments |> List.map (sprintf "(%s)") |> String.concat " "
                        cases.Add($"{moduleName}: {name}", String.concat ", " arguments, $"{call} {applied}")

                marked <- None
            | None -> ()

    if marked.IsSome then
        fail lines.Length "a Fact or Theory on nothing"

    if not compiled.IsEmpty then
        fail lines.Length "an #if with no #endif"

    List.ofSeq cases

let quoted (text: string) =
    "\"" + text.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

File.WriteAllLines(
    entry,
    [
        "// Written by FableEntry.fsx"
        "module FableEntry"
        ""
        "Runner.run ["
        for test, data, call in List.collect casesIn files do
            $"    {quoted test}, {quoted data}, (fun () -> {call})"
        "]"
    ]
)
