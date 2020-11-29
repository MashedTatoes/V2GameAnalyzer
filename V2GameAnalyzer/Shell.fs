namespace V2GameAnalyzer

/// This is the main module of your application
/// here you handle all of your child pages as well as their
/// messages and their updates, useful to update multiple parts
/// of your application, Please refer to the `view` function
/// to see how to handle different kinds of "*child*" controls
module Shell =
    open Elmish
    open Avalonia
    open Avalonia.Controls
    open Avalonia.Input
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI
    open Avalonia.FuncUI.Builder
    open Avalonia.FuncUI.Components.Hosts
    open Avalonia.FuncUI.Elmish

    type State =
        /// store the child state in your main state
        { aboutState: About.State; saveState : SaveInfo.State}

    type Msg =
        | AboutMsg of About.Msg
        | SaveMsg of SaveInfo.Msg
        | OpenFile
        | OpenDir
        | AfterSelectFile of string array
        | AfterSelectDir of string

    let init =
        let aboutState, aboutCmd = About.init
        let saveState, saveCmd = SaveInfo.init

        { aboutState = aboutState;saveState = saveState  },
        /// If your children controls don't emit any commands
        /// in the init function, you can just return Cmd.none
        /// otherwise, you can use a batch operation on all of them
        /// you can add more init commands as you need
        Cmd.batch [ aboutCmd ]

   

    let update (msg: Msg) (state: State) (window: HostWindow): State * Cmd<_> =
        match msg with
        | AboutMsg bpmsg ->
            let aboutState, cmd =
                About.update bpmsg state.aboutState
            { state with aboutState = aboutState },Cmd.map AboutMsg cmd
        | SaveMsg warsMsg ->
            let warsState, cmd =
                SaveInfo.update warsMsg state.saveState
            { state with saveState = warsState }, Cmd.none
        | OpenFile -> 
            let dialog = V2GameAnalyzer.Dialogs.getVictoriaSaveDialog None

            let showDialog window = dialog.ShowAsync(window) |> Async.AwaitTask
            state, Cmd.OfAsync.perform showDialog window AfterSelectFile
        |OpenDir ->
            let dialog = V2GameAnalyzer.Dialogs.getDirectory
            let showDialog window = dialog.ShowAsync(window) |> Async.AwaitTask
            state, Cmd.OfAsync.perform showDialog window AfterSelectDir
        | AfterSelectFile files ->
            let song = files.[0]
            state, Cmd.map SaveMsg (Cmd.ofMsg(SaveInfo.Msg.Open song))
        |AfterSelectDir dir ->
            state,Cmd.map SaveMsg (Cmd.ofMsg(SaveInfo.Msg.LoadFlags dir))

    let tabControl state dispatch = 
        TabControl.create [ 
            TabControl.tabStripPlacement Dock.Top
            TabControl.viewItems
              [ TabItem.create
                    [ TabItem.header "Wars"
                      TabItem.content (SaveInfo.view state.saveState (SaveMsg >> dispatch)) ]
                TabItem.create
                    [ TabItem.header "About"
                      TabItem.content (About.view state.aboutState (AboutMsg >> dispatch)) ] 
              ] 
        ]
    let menuBar state dispatch =
           Menu.create [
               Menu.dock Dock.Top
               Menu.viewItems [
                   MenuItem.create[
                       MenuItem.header "File"
                       MenuItem.viewItems[
                           MenuItem.create[
                               MenuItem.header "Open"
                               MenuItem.onClick( fun _ -> dispatch OpenFile)
                           ]
                           MenuItem.create[
                               MenuItem.header "Load flags"
                               //MenuItem.onClick(fun _ -> dispatch OpenDir)
                           
                           ]
                       ]
                   ]
               ]
           ]
    let view (state: State) (dispatch) =
        DockPanel.create
            [ DockPanel.children
                [ 
                    menuBar state dispatch
                    (SaveInfo.view state.saveState (SaveMsg >> dispatch))
                ]
                
            ]

    /// This is the main window of your application
    /// you can do all sort of useful things here like setting heights and widths
    /// as well as attaching your dev tools that can be super useful when developing with
    /// Avalonia
    type MainWindow() as this =
        inherit HostWindow()
        do
            base.Title <- "Full App"
            base.Width <- 800.0
            base.Height <- 600.0
            base.MinWidth <- 800.0
            base.MinHeight <- 600.0

            //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
            //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true
            let updateWithServices (msg: Msg) (state: State) =
                update msg state this

            Elmish.Program.mkProgram (fun () -> init) updateWithServices view
            |> Program.withHost this
            
            |> Program.run
