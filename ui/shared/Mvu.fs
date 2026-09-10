module public Mvu

open System
open System.Threading
open System.Threading.Tasks

/// How often the heartbeat fires: the render cadence, and the tempo every
/// polling behavior is measured in.
let public pollMilliseconds = 30

/// A console model-view-update program: every occurrence arrives as a
/// message, Update folds it into the model, and the effects each transition
/// calls for are derived from how the model changed - so a request and the
/// bookkeeping that tracks it are one fact that cannot drift apart. The
/// screen is a function of the model, rendered only on the heartbeat, so a
/// burst of keys coalesces into one redraw and work that lands within a beat
/// never flashes an intermediate state.
type public Program<'model, 'msg, 'effect, 'key, 'sub when 'msg: equality and 'key: equality> = {
    /// The starting model, with nothing in flight: the first heartbeat lands
    /// immediately, so startup work flows through the same declared-in-the-
    /// model path as everything later
    Init: 'model
    /// Folds one message into the model; pure
    Update: 'msg -> 'model -> 'model
    /// Derives the work a model transition calls for; pure
    Effects: 'model -> 'model -> 'effect list
    /// Starts one effect, posting whatever it produces back as a message
    Execute: ('msg -> unit) -> 'effect -> unit
    /// Connects external message sources: called once with dispatch before
    /// the first message, so a background process born outside the loop can
    /// post into it for the program's whole life. Whatever it returns - a
    /// handle to that process, or unit when there is none - is what run
    /// returns once the program finishes
    Subscribe: ('msg -> unit) -> 'sub
    /// What the screen is a function of: rendered only when this changes
    ViewKey: 'model -> 'key
    View: 'model -> unit
    /// True once the program is finished
    Quit: 'model -> bool
    /// Turns a key press into a message
    Key: ConsoleKeyInfo -> 'msg
    /// The heartbeat message
    Tick: 'msg
}

/// Runs a program to completion, returning the subscription's handle: a
/// mailbox serializes every message into Update, the message sources are a
/// background key pump thread (which dies with the process) and a ticker
/// that posts before sleeping so the first beat lands immediately.
let public run (program: Program<'model, 'msg, 'effect, 'key, 'sub>) : Async<'sub> = async {
    let finished = TaskCompletionSource()
    let subscription = TaskCompletionSource<'sub>()

    MailboxProcessor.Start(fun inbox ->
        let execute = program.Execute inbox.Post
        subscription.SetResult(program.Subscribe inbox.Post)

        let rec loop (model: 'model) (viewed: 'key option) = async {
            let! msg = inbox.Receive()
            let model' = program.Update msg model

            if program.Quit model' then
                finished.SetResult()
            else
                program.Effects model model' |> List.iter execute

                if msg = program.Tick && Some(program.ViewKey model') <> viewed then
                    program.View model'
                    return! loop model' (Some(program.ViewKey model'))
                else
                    return! loop model' viewed
        }

        let keys =
            Thread(
                ThreadStart(fun () ->
                    while true do
                        inbox.Post(program.Key(Console.ReadKey true))
                )
            )

        keys.IsBackground <- true
        keys.Start()

        Async.Start(
            async {
                while not finished.Task.IsCompleted do
                    inbox.Post program.Tick
                    do! Async.Sleep pollMilliseconds
            }
        )

        loop program.Init None
    )
    |> ignore

    do! finished.Task |> Async.AwaitTask

    // Set before the first message was processed, so certainly set by now
    return! subscription.Task |> Async.AwaitTask
}
