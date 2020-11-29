namespace V2GameAnalyzer

module Dialogs = 
    open System
    open Avalonia.Controls
    let getVictoriaSaveDialog(filters: FileDialogFilter seq option) =
        let dialog = OpenFileDialog()
        let filters =
            match filters with
            | Some filter -> filter
            | None ->
                let filter = FileDialogFilter()
                filter.Extensions <-
                    Collections.Generic.List
                        (seq {
                            "v2"
                             })
                filter.Name <- "Saves"
                seq { filter }

        dialog.AllowMultiple <- false
               
        dialog.Filters <- System.Collections.Generic.List(filters)
        dialog
    let getDirectory =
        let dialog = OpenFolderDialog()
        
        dialog


