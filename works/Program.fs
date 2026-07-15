#if DEBUG
open Rapat
open System.IO
printfn "Result:\n%s" (Compiler.Compile(File.ReadAllText @"C:\Tests\test.rpt"))
#else
open System.IO
open Rapat
open System
open System.Diagnostics

let help _ =
    printfn "Usage:"
    printfn "rptc build <source file> <dest file> <dest format>"
    printfn "rptc version"
    printfn "rptc usage"
    0

[<EntryPoint>]
let rec main argd =
    let softdir = Path.GetDirectoryName(Environment.GetCommandLineArgs()[0])
    let asm = Path.Combine(softdir, "temp.asm")
    if argd.Length = 0 then help()
    else
        if argd.[0].StartsWith '-' then argd.[0] <- argd.[0].TrimStart('-')

        if argd.[0] = "version" then
            printf "Rapat Compiler "
            printfn "Snapshot-20260715-00"
            printfn "Copyright (C) Nam_exx00"
            0
        elif argd.[0] = "help" then help()
        elif argd.[0] = "build" then
            let mutable file = ""
            //Read file
            try
                file <- File.ReadAllText argd.[1]
            with
            | _ ->
                printfn "Fatal: Invalid File."
            try
                File.WriteAllText (Path.Combine(softdir, "temp.asm"),Compiler.Compile file)
                let nasm = ProcessStartInfo()
                nasm.FileName <- Path.Combine(softdir, "nasm.exe")
                nasm.Arguments <- $"-f {argd.[3]} {asm} -o {argd.[2]}";
                nasm.UseShellExecute <- false
                nasm.CreateNoWindow <- true
                nasm.RedirectStandardOutput <- false
                nasm.RedirectStandardError <- false
                Process.Start(nasm).WaitForExit()
                File.Delete(Path.Combine(softdir, "temp.asm"))
                0
            with
            | _ ->
                try
                    main [| "build"; argd.[1]; argd.[2]; "bin" |]
                with
                | _ ->
                    try
                        main [| "build"; argd.[1]; "./result.o"; argd.[2] |]
                    with
                    | _ ->
                        try
                            main [| "build"; argd.[1]; "./result.o"; "bin" |]
                        with
                        | error ->
                            printfn "%s" error.Message
                            -1
        else 
            printfn "Fatal: Invalid Option."
            -1
#endif