namespace BlockingQueue

open System.Threading
open System.Collections.Generic

// Блокирующая очередь с потокобезопасным доступом
type BlockingQueue<'T>() =
    let queue = Queue<'T>()
    let syncRoot = obj()
    
    // Добавить элемент в очередь
    member this.Enqueue(item: 'T) =
        lock syncRoot <| fun () ->
            queue.Enqueue(item)
            Monitor.Pulse(syncRoot)
    
    // Получить элемент из очереди (блокируется, если очередь пуста)
    member this.Dequeue() =
        lock syncRoot <| fun () ->
            while queue.Count = 0 do
                Monitor.Wait(syncRoot) |> ignore
            queue.Dequeue()
    
    // Попытка получить элемент с таймаутом
    member this.TryDequeue(timeoutMilliseconds: int) : 'T option =
        if timeoutMilliseconds < -1 then
            invalidArg (nameof(timeoutMilliseconds)) "Таймаут не может быть меньше -1"

        lock syncRoot <| fun () ->
            match queue.Count with
            | 0 ->
                match Monitor.Wait(syncRoot, timeoutMilliseconds) with
                | true when queue.Count > 0 -> Some(queue.Dequeue())
                | true -> None
                | false -> None
            | _ -> Some(queue.Dequeue())
    
    // Получить текущий размер очереди
    member this.Count =
        lock syncRoot <| fun () -> queue.Count
    
    // Очистить очередь
    member this.Clear() =
        lock syncRoot <| fun () -> 
            queue.Clear()
            Monitor.PulseAll(syncRoot)
    
    // Проверка, пуста ли очередь
    member this.IsEmpty =
        lock syncRoot <| fun () -> queue.Count = 0
