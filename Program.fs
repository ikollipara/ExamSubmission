// Program: Exam Submission
// Author:  Ian Kollipara <ian.kollipara@cune.edu>
// Created: 2025-08-04
// Updated: 2025-08-04
// Description:
//   This program contains the code for the GUI
//   Exam Submission Application for CS 131.
//   Included in the application is the ability to parse a
//   simple config file to determine save location, as well
//   as a simple interface for saving files to.

namespace ExamSubmission

module SubmissionApplication =
    module Types =
        /// Shorthand specification for the uploaded file.
        type FileSpec = { Name: string; Contents: string }

        /// The State of the Application.
        type State = {
            BaseSubmitPath: string
            StatusText: string
            UserName: string option
            FileToSubmit: FileSpec option
            ReadyToSubmit: bool
            Submitted: bool
        }

        /// Possible Messages that the application can send
        type Msg =
            | SetBasePath of string
            | ErrorBasePath of string
            | SetUserName of string
            | OpenFileDialog
            | SetFileToSubmit of FileSpec
            | ReadyToSubmit
            | SubmitExam
            | SubmissionResult of Result<string, string>
            | Pass

    /// Handles all configuration parsing.
    module Config =
        open System.IO

        /// "Parse" the config file.
        let parse (filepath: string) =
            async {
                use handle = File.OpenRead filepath
                use stream = new StreamReader(handle)
                return! stream.ReadToEndAsync() |> Async.AwaitTask
            }

    /// Contains all file dialog functions.
    module Dialogs =
        open Avalonia.Platform.Storage
        open Avalonia.Threading
        open System.Collections.Generic
        open System.IO
        open FSharpPlus
        open Types


        exception FailedToReadFile

        /// Select a file and return its spec. Must be a python file.
        let selectPythonFile (provider: IStorageProvider) =
            let filter = FilePickerFileType("Python Files", Patterns = [ "*.py" ])

            let options =
                FilePickerOpenOptions(
                    AllowMultiple = false,
                    Title = "Select Python File to Submit",
                    FileTypeFilter = (filter |> List.singleton)
                )

            async {
                let! files =
                    Dispatcher.UIThread.InvokeAsync<IReadOnlyList<IStorageFile>>(fun () ->
                        provider.OpenFilePickerAsync options)
                    |> Async.AwaitTask

                match files |> IReadOnlyList.toArray with
                | [| file |] ->
                    use! stream = file.OpenReadAsync() |> Async.AwaitTask
                    use reader = new StreamReader(stream)
                    let! contents = reader.ReadToEndAsync() |> Async.AwaitTask

                    return {
                        Name = file.Name
                        Contents = contents
                    }
                | _ -> return! Async.raise FailedToReadFile
            }

    /// Contains all the needed code to make Elmish work
    module Tea =
        open Elmish
        open Avalonia.Controls
        open Avalonia.FuncUI
        open Avalonia.FuncUI.DSL
        open Avalonia.Layout
        open Avalonia.Media
        open Types
        open FSharpPlus
        open System.IO
        open System

        /// Callback to check if the application is ready to submit an exam.
        let private checkIfReadyCmd (state: State) =
            match state.UserName, state.FileToSubmit with
            | Some _, Some _ -> Cmd.ofMsg ReadyToSubmit
            | _ -> Cmd.ofMsg Pass

        /// Submit the exam to the specified file location.
        let private submitExamAsync (state: State) =
            let formattedUserName =
                state.UserName.Value
                |> String.replace " " "_"
                |> String.replace "-" "_"
                |> String.replace "'" ""
                |> String.toLower

            let datetime =
                DateTime.UtcNow
                |> _.ToString("o", Globalization.CultureInfo.GetCultureInfo("en-US", false))

            let name = state.FileToSubmit.Value.Name

            let filename =
                $"{state.BaseSubmitPath}\\{datetime}__{formattedUserName}__{name}.txt"

            async {
                use stream = File.Create filename
                use writer = new StreamWriter(stream)
                do! writer.WriteAsync state.FileToSubmit.Value.Contents |> Async.AwaitTask
                do! writer.FlushAsync() |> Async.AwaitTask
            }

        /// Initial state of the application.
        let init () =
            {
                BaseSubmitPath = ""
                StatusText = "Please enter your name and upload a file."
                UserName = None
                FileToSubmit = None
                ReadyToSubmit = false
                Submitted = false
            },
            Cmd.OfAsync.either Config.parse "\\\\zeta\\acad_cs\\CS131_Exams\\submission.conf" SetBasePath (fun e ->
                e.ToString() |> ErrorBasePath)

        /// TEA Update Function
        let update (window: Hosts.HostWindow) (msg: Msg) (state: State) =
            match msg with
            | SetBasePath path -> { state with BaseSubmitPath = path }, Cmd.none
            | ErrorBasePath msg -> { state with StatusText = msg }, Cmd.none
            | SetUserName "" ->
                {
                    state with
                        UserName = None
                        StatusText =
                            if state.FileToSubmit.IsSome then
                                "Please input a name."
                            else
                                "Please enter your name and upload a file."
                }
                |> fun ns -> ns, checkIfReadyCmd ns

            | SetUserName name ->
                {
                    state with
                        UserName = Some name
                        StatusText = "Please input a file."
                }
                |> fun ns -> ns, checkIfReadyCmd ns
            | OpenFileDialog ->
                state, Cmd.OfAsync.perform Dialogs.selectPythonFile window.StorageProvider SetFileToSubmit
            | SetFileToSubmit fileSpec ->
                {
                    state with
                        FileToSubmit = Some fileSpec
                        StatusText = "Please input a Name."
                }
                |> fun ns -> ns, checkIfReadyCmd ns
            | ReadyToSubmit ->
                {
                    state with
                        ReadyToSubmit = true
                        StatusText = "Ready To Submit."
                },
                Cmd.none
            | SubmitExam ->
                state,
                Cmd.OfAsync.either
                    submitExamAsync
                    state
                    (fun () -> SubmissionResult(Ok "Exam Submitted Successfully. You may close the window."))
                    (fun _ ->
                        SubmissionResult(
                            Error
                                "Something went wrong. Try to submit in a little bit. If this continues to occur, please contact the instructor."
                        ))
            | SubmissionResult sr ->
                match sr with
                | Ok text ->
                    {
                        state with
                            Submitted = true
                            StatusText = text
                    },
                    Cmd.none
                | Error text ->
                    {
                        state with
                            Submitted = false
                            StatusText = text
                    },
                    Cmd.none
            | Pass -> state, Cmd.none


        let view (state: State) (dispatch: Msg -> unit) =
            DockPanel.create [
                DockPanel.children [
                    Border.create [
                        Border.cornerRadius 15.0
                        Border.padding 30.0
                        Border.borderThickness 1.0
                        Border.child (
                            StackPanel.create [
                                StackPanel.spacing 20.0
                                StackPanel.horizontalAlignment HorizontalAlignment.Center
                                StackPanel.children [

                                    TextBlock.create [
                                        TextBlock.text "CS 131 Exam Submission"
                                        TextBlock.fontSize 16.0
                                        TextBlock.textAlignment TextAlignment.Center
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                    ]

                                    TextBlock.create [
                                        TextBlock.text state.StatusText
                                        TextBlock.fontSize 16.0
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        TextBlock.textAlignment TextAlignment.Center
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                    ]


                                    TextBlock.create [ TextBlock.text "Name:"; TextBlock.margin (0, 10, 0, 0) ]

                                    TextBox.create [
                                        TextBox.text (Option.defaultValue "" state.UserName)
                                        TextBox.onTextChanged (SetUserName >> dispatch)
                                        TextBox.width 200.0
                                        TextBox.horizontalAlignment HorizontalAlignment.Center
                                    ]

                                    Button.create [
                                        Button.content (
                                            state.FileToSubmit
                                            |> Option.map _.Name
                                            |> Option.defaultValue "Upload File..."
                                        )
                                        Button.width 200.0
                                        Button.height 50.0
                                        Button.horizontalAlignment HorizontalAlignment.Center
                                        Button.onClick (fun _ -> dispatch OpenFileDialog)
                                    ]

                                    Button.create [
                                        Button.content "Submit"
                                        Button.width 120.0
                                        Button.horizontalAlignment HorizontalAlignment.Center
                                        Button.isEnabled (state.ReadyToSubmit && not state.Submitted)
                                        Button.onClick (fun _ -> dispatch SubmitExam)
                                    ]
                                ]
                            ]
                        )
                    ]
                ]
            ]

module Main =
    open Elmish
    open Avalonia
    open Avalonia.Themes.Fluent
    open Avalonia.Controls
    open Avalonia.Controls.ApplicationLifetimes
    open Avalonia.FuncUI.Elmish
    open Avalonia.FuncUI.Hosts

    type MainWindow() as this =
        inherit HostWindow()

        do
            base.Title <- "CS 131 Exam Submission"
            base.Width <- 800.0
            base.Height <- 600.0
            this.SystemDecorations <- SystemDecorations.Full

            Program.mkProgram
                SubmissionApplication.Tea.init
                (SubmissionApplication.Tea.update this)
                SubmissionApplication.Tea.view
            |> Program.withHost this
#if DEBUG
            |> Program.withConsoleTrace
#endif
            |> Program.runWithAvaloniaSyncDispatch ()

    type App() =
        inherit Application()

        override this.Initialize() : unit =
            this.Styles.Add(FluentTheme())
            this.RequestedThemeVariant <- Styling.ThemeVariant.Dark
            base.Initialize()

        override this.OnFrameworkInitializationCompleted() : unit =
            match this.ApplicationLifetime with
            | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
                desktopLifetime.MainWindow <- MainWindow()
            | _ -> ()


module Program =
    open Avalonia
    open Main

    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime args
