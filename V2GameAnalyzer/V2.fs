namespace V2GameAnalyzer
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
module V2 =

    //I hate regex
    let DATE_REGEX_STRING = "^(?:(?:1[7-9][0-9]{2}|200[0-9]|2010)([-/.]?)(?:(?:0?[1-9]|1[0-2])\1(?:0?[1-9]|1[0-9]|2[0-8])|(?:0?[13-9]|1[0-2])\1(?:29|30)|(?:0?[13578]|1[02])\1(?:31))|(?:19(?:0[48]|[2648][048]|[13579][26])|2000|200[48])([-/.]?)0?2\2(?:29))$"
    let dateRegex = new Regex(DATE_REGEX_STRING,RegexOptions.Compiled)


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
   
  



    type EventType =
        | WarGoal of Option<string> * Option<string> * Option<string> * Option<double>  * Option<string>
        | AddAttacker of string
        | RemoveAttacker of string
        | AddDefender of string
        | RemoveDefender of string
        | NoEvent
            
    type Event = {
        date:string
        eventType : EventType
    }

    let initEvent = {date = "";eventType = EventType.NoEvent}

    type War ={
           name:string
           attacker:string
           defender:string
           battles : list<Battle>
           events : list<Event>
    }

    let initWar = {name = "";attacker = "";defender="";battles = List.empty;events = List.empty}
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
    
    let getDict (block : list<string>) =  [for s in block -> s.Split("=").[0],s.Split("=").[1]] |> Map.ofSeq

    let rec parseEvent (lines : list<string>) (event:Event) (currentIndex : int) =
        if currentIndex >= lines.Length then event

        

        else
            let line = lines.[currentIndex].Split("=")
            match line.[0] with
                | "war_goal" ->
                    let block = List.filter(fun s -> s <> "{" && s <> "}") (collectBlock (currentIndex + 1) 0 0 (Array.ofList lines) List.empty)
                    let warGoal  = getDict block
                    let cb = if warGoal.ContainsKey("casus_belli") then Some(warGoal.["casus_belli"]) else None
                    let actor = if warGoal.ContainsKey("actor") then Some(warGoal.["actor"]) else None
                    let receiver = if warGoal.ContainsKey("receiver") then Some(warGoal.["receiver"]) else None
                    let score = if warGoal.ContainsKey("score") then Some(double (warGoal.["score"])) else None
                    let is_fulfilled = if warGoal.ContainsKey("is_fulfilled") then Some(warGoal.["is_fulfilled"]) else None
                    let eventType = WarGoal(cb,actor,receiver,score,is_fulfilled)
                    {event with eventType = eventType}
                | "add_attacker" ->
                    
                    let attacker = line.[1].Replace("\"","")
                    let eventType = AddAttacker(attacker)
                    {event with eventType = eventType}
                | "add_defender" ->
                    
                    let attacker = line.[1].Replace("\"","")
                    let eventType = AddDefender(attacker)
                    {event with eventType = eventType}
                | "rem_attacker" ->
                    
                    let attacker = line.[1].Replace("\"","")
                    let eventType = RemoveAttacker(attacker)
                    {event with eventType = eventType}
                | "rem_defender" ->
                    
                    let attacker = line.[1].Replace("\"","")
                    let eventType = RemoveAttacker(attacker)
                    {event with eventType = eventType}
                | _ -> parseEvent lines event (currentIndex + 1)

    let parseWar (lines : list<string>) = 
        let mutable war = initWar
        let mutable parent = ""
        let blockStack = new Stack<string>()
        
        for i in 0..lines.Length - 1 do
            let line = lines.[i]
            
            match line with
                | "{" -> blockStack.Push(lines.[i-1].Replace("=",""))
                | "}" -> blockStack.Pop() |> ignore
                | _ -> ()
            if blockStack.Count <> 0 then
                match blockStack.Peek() with
                    | "previous_war" ->
                        if line.Contains("name") then war <- {war with name = getValue line} 
                        elif line.Contains("attacker") then war <- {war with attacker = getValue line}
                        elif line.Contains("defender") then war<- {war with defender =getValue line}
                    | "battle" ->
                        if line = "{" then
                            let block = collectBlock i 0 0  (Array.ofList lines) List.empty
                            let battle= parseBattle block initBattle 0
                            war <- {war with battles = battle::war.battles}
                    | date when dateRegex.IsMatch(date) -> 
                        if line = "{" then
                            printfn "New event %s" date
                            let block = collectBlock i 0 0 (Array.ofList lines) List.empty
                            let event = parseEvent block {initEvent with date = date} 0
                            if event.eventType <> NoEvent then 
                                war <- {war with events = event::war.events}
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
                    

