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
    member this.TryDequeue(timeoutMilliseconds: int) =
        lock syncRoot <| fun () ->
            if queue.Count = 0 then
                Monitor.Wait(syncRoot, timeoutMilliseconds) |> ignore
            if queue.Count > 0 then
                Some(queue.Dequeue())
            else
                None
    
    // Получить текущий размер очереди
    member this.Count =
        lock syncRoot <| fun () -> queue.Count
    
    // Очистить очередь
    member this.Clear() =
        lock syncRoot <| fun () -> queue.Clear()