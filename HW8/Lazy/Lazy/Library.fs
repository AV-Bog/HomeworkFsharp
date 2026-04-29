namespace Lazy

type ILazy<'a> =
    abstract member Get: unit -> 'a

type SimpleLazy<'a>(supplier : unit -> 'a) =
    let mutable cached = None
    
    interface ILazy<'a> with
        member this.Get() =
            match cached with
            | Some value -> value
            | None ->
                let value = supplier()
                cached <- Some value
                value

type ThreadSafeLazy<'a>(supplier : unit -> 'a) =
    let mutable cached = None
    let lockObj = obj()
    
    interface ILazy<'a> with
        member this.Get() =
            match cached with
            | Some value -> value
            | None ->
                lock lockObj (fun () ->
                    match cached with
                    | Some value -> value
                    | None ->
                        let value = supplier()
                        cached <- Some value
                        value
                )

type LockFreeLazy<'a>(supplier : unit -> 'a) =
    let mutable cached = None
    
    interface ILazy<'a> with
        member this.Get() =
            match cached with
            | Some value -> value
            | None ->
                let value = supplier()
                match cached with
                | Some existing -> existing
                | None ->
                    cached <- Some value
                    value