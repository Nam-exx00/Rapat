namespace Rapat

open System
open System.IO
open System.Text
open System.Collections.Generic

type Compiler =
    static let cl (s: string, c: char) =
            let mutable count = 0
            let len = s.Length
            while count < len && s.[count] = c do
                count <- count + 1
            count
    //Exceptions
    static let exceptions = List<string>()
    //Sections
    static let text = StringBuilder "section .text\nglobal entry\nentry:"
    static let data = StringBuilder "\nsection .data"
    //Storages
    static let varnames = List<string>()
    static let vartypes = List<int>()
    static let funcnames = List<string>()
    static let functypes = List<int>()
    static let funcpbs = List<string[]>()
    static let funcpvs = List<bool>()
    static let mutable pms = 0
    //Event
    static let rec Run(code : string) =
        let mutable i = 0
        let mutable gb = false
        let mutable gbb = StringBuilder()
        let mutable gbd = 0
        let mutable r = false
        let mutable rn = true
        let mutable rn2 = 0
        let mutable fc = 0
        let mutable note = false
        let mutable nd = 0
        let pb = Stack<string>() 
        let mutable raw = false
        while i < code.Length do
            let mutable op = code.[i]
            if note then 
                if op  = ')' then 
                    if nd = 0 then
                        note <- false
                    else nd <- nd - 1
                elif op = '(' then nd <- nd + 1
            else
                let fatal (s: string) =
                    exceptions.Add $"Fatal: {s} At '{op}' ({i})."
                let tfat (t : string) =
                    fatal $"Type unmatched. Should be {t}."
                let pfat (i : int) =
                    fatal $"Parameters size unmatched. Should be {i} parameters."
                if gb then
                    if raw then
                        gbb.Append op |> ignore
                        raw <- false
                    elif r && (not rn) && gbb.Length < rn2 then
                        gbb.Append op |> ignore
                    else
                        match op with
                        | '(' -> note <- true
                        | '\\' -> raw <- true
                        | '[' -> 
                            gbd <- gbd + 1
                            gbb.Append '[' |> ignore
                        | ',' ->
                            if gbd = 0 then
                                pb.Push(gbb.ToString())
                                gbb.Clear() |> ignore
                            else gbb.Append ',' |> ignore
                        | ']' ->
                            if gbd = 0 then
                                if gbb.Length <> 0 then
                                    pb.Push(gbb.ToString())
                                    gbb.Clear() |> ignore
                                gb <- false
                            else
                                gbd <- gbd - 1
                                gbb.Append ']' |> ignore
                        | '.' -> 
                            if gbd = 0 then
                                if r && rn then
                                    rn <- false
                                    try
                                        rn2 <- Int32.Parse(gbb.ToString())
                                        gbb.Clear() |> ignore
                                    with | _ -> fatal "Invalid number."
                            else gbb.Append '.' |> ignore
                        | '`' ->
                            if gbd = 0 then
                                r <- true
                            else gbb.Append '`' |> ignore
                        | _ -> gbb.Append op |> ignore
                else
                    match op with
                    | '\t' 
                    | '\r'
                    | '\n'
                    | ' ' -> ()
                    | '(' -> note <- true
                    | '[' -> gb <- true
                    | '*' ->
                        if pb.Count = 2 then
                            let mutable a0 = pb.Pop()
                            let mutable a1 = pb.Pop()
                            if a0.StartsWith "n:" then
                                a0 <- a0.[2..]
                            elif a0.StartsWith "x:" then
                                a0 <- $"0x{a0.[2..]}"
                            else tfat "number or hexadecimal"
                            if a1.StartsWith "8bx:" then
                                text.Append $"\nmov [{a0}], 0x{a1.[4..]}" |> ignore
                            elif a1.StartsWith "8b:" then
                                text.Append $"\nmov [{a0}], {a1.[3..]}" |> ignore
                            elif a1.StartsWith "c:" then
                                a1 <- a1.[2..]
                                if a1.Length = 1 then
                                    text.Append $"\nmov [{a0}], '{a1}'" |> ignore
                                else fatal "Character size unmatched. Should be 1 character."
                            else tfat "8 bits number or character"
                        else pfat 2
                    | '#' ->
                        let count = pb.Count - 1
                        for i = 0 to count do
                            let code = pb.Pop()
                            if code.StartsWith "asm:" then
                                text.Append $"\n{code.[4..].Trim()}" |> ignore
                            else tfat "Assembly"
                    | '@' ->
                        if pb.Count = 0 then
                            text.Append $"\n.f{fc}:\njmp .f{fc}" |> ignore
                        elif pb.Count = 1 then
                            text.Append $"\n.f{fc}:" |> ignore
                            fc <- fc + 1
                            Run(pb.Pop())
                            fc <- fc - 1
                            text.Append $"\njmp .f{fc}" |> ignore
                        else pfat 1
                    |  ':' ->
                        if pb.Count = 2 then
                            let mutable name = pb.Pop().Trim()
                            let mutable tpe = -1
                            let mutable dat = pb.Pop()
                            if dat.StartsWith "8bx:" then
                                tpe <- 0
                                dat <- $"0x{dat.[4..]}"
                            elif dat.StartsWith "8b:" then
                                tpe <- 1
                                dat <- dat.[3..]
                            else tfat "supported types"
                            let mutable i = 0
                            let mutable running = true
                            let mutable found = false
                            while i < varnames.Count do
                                let j = varnames.Count - 1 - i
                                if varnames.[j] = name && vartypes.[j] = tpe then
                                    match tpe with
                                    | 0 | 1 -> text.Append $"\nmov al, {dat}\nmov [anb_v{j}], al" |> ignore
                                    | _ -> tfat "supported types"
                                    found <- true
                                    running <- false
                                i <- i + 1
                            if not found then
                                match tpe with
                                | 0 | 1 -> data.Append $"\nanb_v{varnames.Count} db {dat}" |> ignore
                                | _ -> tfat "supported types" 
                                varnames.Add name
                                vartypes.Add tpe
                        else pfat 2
                    | 'f' ->
                        if pb.Count = 0 || pb.Count = 1 then pfat 2
                        if pb.Count = 2 then
                            let mutable name = pb.Pop().Trim()
                            let mutable code = pb.Pop()
                            let n = funcnames.Count
                            if name.StartsWith "private:" then
                                name <- name.[8..].Trim()
                                text.Append $"\n.pv{n}:" |> ignore
                                funcpvs.Add true
                            else
                                text.Append $"\nglobal gb{n}\ngb{n}:" |> ignore
                                funcpvs.Add false
                            funcnames.Add name
                            Run code
                            let result = gbb.ToString().Trim()
                            if String.IsNullOrEmpty result then
                                functypes.Add 0
                            elif result.StartsWith "8b:" then
                                text.Append $"\nmov al, {result.[3..]}" |> ignore
                                functypes.Add 1
                            else tfat "supported types"
                            text.Append "\nret" |> ignore
                        (*else
                            let mutable name = pb.Pop().Trim()
                            let mutable code = pb.Pop()
                            let n = funcnames.Count
                            if name.StartsWith "private:" then
                                name <- name.[8..].Trim()
                                text.Append $"\n.pv{n}:" |> ignore
                                funcpvs.Add true
                            else
                                text.Append $"\nglobal gb{n}\ngb{n}:" |> ignore
                                funcpvs.Add false
                            funcnames.Add name
                            Run code
                            let result = gbb.ToString().Trim()
                            if String.IsNullOrEmpty result then
                                functypes.Add 0
                            elif result.StartsWith "8b:" then
                                text.Append $"\nmov al, {result.[3..]}" |> ignore
                                functypes.Add 1
                            else tfat "supported types"
                            text.Append "\nret" |> ignore*)
                    | '$' ->
                        if pb.Count = 1 then
                            let lib = pb.Pop()
                            try Run(File.ReadAllText(Path.Combine(AppContext.BaseDirectory, ".lib/", lib)))
                            with _ ->
                                try Run(File.ReadAllText lib)
                                with _ -> fatal $"Invalid Library."
                        else pfat 1
                    | _ ->
                        let mutable ip = 0
                        let mutable running = true
                        let mutable found = false
                        while ip < funcnames.Count && running do
                            let j = funcnames.Count - 1 - ip
                            let name = funcnames.[j]
                            let mutable k = 0
                            let mutable matched = true
                            try
                                let mutable running = true
                                while k < name.Length && running do
                                    if code.[i + k] <> name.[k] then 
                                        matched <- false
                                    k <- k + 1
                                if matched then i <- i + k - 1
                            with _ -> ()
                            if matched then
                                found <- true
                                running <- false
                                let count = pb.Count
                                if count <> 0 then
                                    let fpms = funcpbs.[j]
                                    if count = fpms.Length then
                                        for i = 0 to count - 1 do
                                            let fpm = fpms.[i]
                                            let pm = pb.Pop()
                                            if pm.StartsWith "8b:" then
                                                if fpm = "8 bits number" then
                                                    text.Append $"\npush {pm.[3..]}" |> ignore
                                                else tfat fpm
                                            elif pm.StartsWith "8bx:" then
                                                if fpm = "8 bits hexadecimal" then
                                                    text.Append $"\npush 0x{pm.[4..]}" |> ignore
                                                else tfat fpm
                                            elif pm.StartsWith "x:" then
                                                if fpm = "hexadecimal" then
                                                    text.Append $"\npush 0x{pm.[2..]}" |> ignore
                                                else tfat fpm
                                            elif pm.StartsWith "n:" then
                                                if fpm = "number" then
                                                    text.Append $"\npush {pm.[2..]}" |> ignore
                                                else tfat fpm
                                            else tfat "supported types"
                                    else pfat fpms.Length
                                if funcpvs.[j] then
                                    text.Append $"\ncall .pv{j}" |> ignore
                                else
                                    text.Append $"\ncall gb{j}" |> ignore
                            ip <- ip + 1
                        if not found then fatal "Invalid Character."
            i <- i + 1
        if pb.Count = 1 then
            let data = pb.Pop()
            if data.StartsWith "8bx:" then
                text.Append $"\nmov al, 0x{data.[4..]}" |> ignore
            elif data.StartsWith "8b:" then
                text.Append $"\nmov al, {data.[3..]}" |> ignore
    static member Compile(code : string) =
        let out = StringBuilder()
        Run code
        if exceptions.Count = 0 then
            if text.ToString() <> "section .text\nglobal entry\nentry:" then
                out.Append text |> ignore
                out.Append "\nret" |> ignore
            if data.ToString() <> "\nsection .data" then
                out.Append data |> ignore
        else
            printfn "%s" ("Build Unsuccessful.\n\t" + String.Join("\n\t", exceptions))
            failwithf "%s" ("Build Unsuccessful.\n\t" + String.Join("\n\t", exceptions))
        out.ToString()