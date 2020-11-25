namespace V2GameAnalyzer
open System.IO
open System.Collections.Generic
module V2 =

    
    type BattleSide = 
        |ATTACKER
        |DEFENDER

    type BattleActor = {
        side: BattleSide
        country:string
        leader: string
        infantry : int
        cavalry : int
        artillery : int
        hussar : int
        losses:int
    }
    let initActor = {side = BattleSide.ATTACKER;leader = "";infantry = 0; cavalry = 0; artillery = 0;country = "";hussar = 0; losses = 0}


    type Battle ={
        name:string
        result:string
        attacker: BattleActor
        defender : BattleActor
    }
    let initBattle = {name = ""; result = "";attacker = initActor;defender = initActor}
    type War ={
        name:string
        attacker:string
        defender:string
        battles : list<Battle>
    }
    let initWar = {name = "";attacker = "";defender="";battles = List.empty}
    type V2File = {
        wars: list<War>
    
    }

    let init = {wars = List.empty}
    let getValue (string : string) = 
        string.Split("=").[1].Replace("\"","")
    let rec collectBlock index openBrackets closedBrackets (data:string[]) (block:list<string>) =
        
         let currentLine = data.[index]
         if openBrackets = closedBrackets && (openBrackets <> 0 && closedBrackets <> 0) then block
         else
           
           match currentLine with
           | "{" -> collectBlock (index + 1) (openBrackets + 1) closedBrackets data (block @ (List.singleton currentLine))
           | "}" -> collectBlock (index + 1) openBrackets (closedBrackets + 1) data (block @ (List.singleton currentLine))
           | _ ->   collectBlock (index + 1) openBrackets closedBrackets data (block @ (List.singleton currentLine)) 
    let rec parseActor (lines : list<string>) actor (currentIndex : int) =
        let line = lines.[currentIndex]
        match line.Split("=").[0] with
            | "country" -> parseActor lines ({actor with country = getValue line}) (currentIndex + 1)
            | "leader" -> parseActor lines {actor with leader = getValue line} (currentIndex + 1)
            | "artillery" -> parseActor lines {actor with artillery = (int (getValue line))} (currentIndex + 1)
            | "infantry" -> parseActor lines {actor with infantry = (int (getValue line))} (currentIndex + 1)
            | "cavalry" -> parseActor lines {actor with cavalry =(int (getValue line))} (currentIndex + 1)
            | "hussar" -> parseActor lines {actor with hussar =(int (getValue line))} (currentIndex + 1)
            | "losses" -> parseActor lines {actor with losses =(int (getValue line))} (currentIndex + 1)
            | "}" -> actor
            | _ -> parseActor lines actor (currentIndex + 1)
    let rec parseBattle (lines : list<string>) (battle : Battle) (currentIndex : int) =
        if currentIndex >= lines.Length then battle
        else
            let line = lines.[currentIndex]
            match line.Split("=").[0] with
                | "name" -> parseBattle lines {battle with name = getValue line} (currentIndex + 1)
                | "result" -> parseBattle lines {battle with result = getValue line}(currentIndex + 1)
                | "attacker" ->
                    let block = collectBlock currentIndex 0 0 (Array.ofList lines) List.empty
                    parseBattle lines {battle with attacker = (parseActor block {initActor with side = BattleSide.ATTACKER} 0)} (currentIndex + block.Length)
                | "defender" -> 
                    let block = collectBlock currentIndex 0 0 (Array.ofList lines) List.empty
                    parseBattle lines {battle with defender = (parseActor block {initActor with side = BattleSide.DEFENDER} 0)} (currentIndex + block.Length)
                |  _ -> parseBattle lines battle (currentIndex + 1)
        
            

    let parseWar (lines : list<string>) = 
        let mutable war = initWar
        let mutable parent = ""
        let blockStack = new Stack<string>()
        
        for i in 0..lines.Length - 1 do
            let line = lines.[i]
            
            match line with
                | "{" -> blockStack.Push(lines.[i-1])
                | "}" -> blockStack.Pop() |> ignore
                | _ -> ()
            if blockStack.Count <> 0 then
                match blockStack.Peek() with
                    | "previous_war=" ->
                        if line.Contains("name") then war <- {war with name = getValue line} 
                        elif line.Contains("attacker") then war <- {war with attacker = getValue line}
                        elif line.Contains("defender") then war<- {war with defender =getValue line}
                    | "battle=" ->
                        if line = "{" then
                            let block = collectBlock i 0 0  (Array.ofList lines) List.empty
                            let battle= parseBattle block initBattle 0
                            war <- {war with battles = battle::war.battles}
                    | _ -> ()
        war

    
         
         

    let parse file = 
        let lines = File.ReadAllLines (file, System.Text.Encoding.GetEncoding("ISO-8859-1"))
        let lines = Array.map(fun (line : string) -> line.Replace("\t","")) lines
        let mutable wars : list<War> = List.empty
        for i in 0..lines.Length - 1 do
            let line = lines.[i]
            if line = "previous_war=" then
                
                let war = parseWar(collectBlock i 0 0 lines List.empty )
                wars <- war::wars
        {wars = wars}
                    

