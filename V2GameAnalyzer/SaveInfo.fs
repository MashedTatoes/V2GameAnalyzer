namespace V2GameAnalyzer

open Avalonia.FuncUI.Components

module SaveInfo= 
    open Elmish
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Types
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.FuncUI.DSL

    type State = {
        saveGame : string
        v2File : V2.V2File
        selectedWar : Option< V2.War>
        selectedBattle : Option<V2.Battle>
        
    }
    type Msg = 
        Open of string
        | SelectWar of Option< V2.War>
        | SelectBattle of Option<V2.Battle>
    let init = {saveGame = "";v2File = V2.init;selectedWar = None;selectedBattle = None},Cmd.none
    let update (msg: Msg) (state: State)  =
       let newState = 
            match msg with
                | Msg.Open file -> 
                    printfn "Open file: %s" file
                    let v2 =V2.parse file
                    {state with saveGame = file;v2File = v2 }
                | Msg.SelectWar war ->
                    match war with
                        |Some(w) -> printfn "War selected: %s" w.name
                        | _ -> ()
                    {state with selectedWar = war;selectedBattle = (match war with | Some(w) -> Some( w.battles.[0]) | None -> None)}
       newState,Cmd.none

    

    let showBattle (state:State) dispatch =
        
        let showActor (actor: V2.BattleActor) alignment=
            StackPanel.create[
                StackPanel.spacing 10.0
                StackPanel.horizontalAlignment alignment
                StackPanel.dock Dock.Left
                StackPanel.children[
                    TextBlock.create[TextBlock.text (actor.side.ToString());TextBlock.fontSize 16.0;TextBlock.horizontalAlignment HorizontalAlignment.Center]
                    TextBlock.create[TextBlock.text actor.country;TextBlock.fontSize 16.0;TextBlock.horizontalAlignment HorizontalAlignment.Center]
                    TextBlock.create[TextBlock.text (sprintf "Leader: %s" actor.leader);TextBlock.fontSize 16.0;TextBlock.horizontalAlignment HorizontalAlignment.Center]
                    TextBlock.create[TextBlock.text (sprintf "Losses: %d" actor.losses);TextBlock.fontSize 16.0;TextBlock.horizontalAlignment HorizontalAlignment.Center]
                    TextBlock.create[TextBlock.text "Land Units:";TextBlock.fontSize 16.0;TextBlock.horizontalAlignment HorizontalAlignment.Center]
                    Expander.create[
                        Expander.content actor
                        Expander.horizontalAlignment HorizontalAlignment.Center
                        Expander.contentTemplate(DataTemplateView<V2.BattleActor>.create(fun item -> StackPanel.create[
                            StackPanel.children[
                                 TextBlock.create[TextBlock.text (sprintf "Infantry: %d" item.artillery);TextBlock.fontSize 14.0]
                                 TextBlock.create[TextBlock.text (sprintf "Cavalry: %d" item.infantry);TextBlock.fontSize 14.0]
                                 TextBlock.create[TextBlock.text (sprintf "Arillery: %d" item.artillery);TextBlock.fontSize 14.0]
                                 TextBlock.create[TextBlock.text (sprintf "Hussar: %d" item.hussar);TextBlock.fontSize 14.0]
                                 
                            ]
                        ]))
                    ]
                    

                ]

            ]

        match state.selectedBattle with   
            | Some(battle) ->
                StackPanel.create[
                    StackPanel.spacing 15.0
                    StackPanel.children[
                        
                        TextBlock.create[
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.fontSize 20.0
                            TextBlock.text (sprintf "Battle of %s" battle.name)
                        ]
                        DockPanel.create[
                            DockPanel.children[
                                showActor battle.attacker HorizontalAlignment.Left
                                showActor battle.defender HorizontalAlignment.Right
                            ]
                        ]
                        
                    ]                    
                ]
            | None -> StackPanel.create[]

    
    let showWar (state: State) dispatch  =
        
        let showStats country alignment =
            StackPanel.create[
                StackPanel.horizontalAlignment alignment
                StackPanel.children[
                    TextBlock.create[
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.fontSize 24.0
                        TextBlock.text (match country with | Some(c) -> c | None -> "") ]

                ]
            ]

        DockPanel.create[
            DockPanel.children[
                showStats (match state.selectedWar with Some(war) -> Some( sprintf "Belligerent: %s"war.attacker) | None -> None) HorizontalAlignment.Left
                showStats (match state.selectedWar with Some(war) -> Some( sprintf "Defender: %s" war.defender) | None -> None) HorizontalAlignment.Right
            ]
        ]

    let warsContent (state: State) (dispatch) =
        
        let warItem (item: V2.War) dispatch =
            StackPanel.create[
                StackPanel.spacing 10.0
                
                StackPanel.children[
                    TextBlock.create[
                        
                        TextBlock.text item.name
                        
                    ]
                ]
            ]
            
        DockPanel.create[
            

            DockPanel.children[
                ListBox.create[
                    
                    
                    ListBox.dataItems state.v2File.wars
                    ListBox.itemTemplate(DataTemplateView<V2.War>.create(fun (item) -> warItem item dispatch ))
                    ListBox.onSelectedItemChanged((fun obj -> 
                                                    match obj with
                                                        | :? V2.War as war -> war |> Some |> SelectWar |> dispatch
                                                        | _ -> None |> SelectWar |> dispatch))
                ]
                StackPanel.create[
                    StackPanel.spacing 20.0
                    StackPanel.children[
                        TextBlock.create[
                            TextBlock.horizontalAlignment HorizontalAlignment.Center
                            TextBlock.fontSize 20.0
                            TextBlock.text (match state.selectedWar with | Some(war) -> war.name | None -> "No war selected")
                        ]
                        
                        showWar state dispatch
                        showBattle state dispatch
                    ]
                ]
                
                
                
            ]
        ]
       
        
    let view (state: State) (dispatch) =
        DockPanel.create[
            DockPanel.children[
                TabControl.create [ 
                    TabControl.tabStripPlacement Dock.Top
                    TabControl.viewItems
                      [ TabItem.create
                            [ TabItem.header "Wars"
                              TabItem.content (warsContent state dispatch)]
                      ] 
                ]
            ]
        ]
    
        

