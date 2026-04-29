module Tests

open Lazy
open NUnit.Framework
open System.Threading

type LazyTests() =
    
    [<Test>]
    member this.``SimpleLazy - вычисление происходит только один раз`` () =
        let callCount = ref 0
        let supplier () = 
            callCount.Value <- callCount.Value + 1
            42
        
        let lazyValue = SimpleLazy(supplier) :> ILazy<int>
        
        let first = lazyValue.Get()
        let second = lazyValue.Get()
        let third = lazyValue.Get()
        
        Assert.That((first = 42), Is.True)
        Assert.That((second = 42), Is.True)
        Assert.That((third = 42), Is.True)
        Assert.That(callCount.Value = 1, Is.True)
    
    [<Test>]
    member this.``SimpleLazy - разные объекты не влияют друг на друга`` () =
        let callCount1 = ref 0
        let supplier1 () = 
            callCount1.Value <- callCount1.Value + 1
            10
        
        let callCount2 = ref 0
        let supplier2 () = 
            callCount2.Value <- callCount2 .Value+ 1
            20
        
        let lazy1 = SimpleLazy(supplier1) :> ILazy<int>
        let lazy2 = SimpleLazy(supplier2) :> ILazy<int>
        
        Assert.That(lazy1.Get() = 10, Is.True)
        Assert.That(lazy2.Get() = 20, Is.True)
        Assert.That(callCount1.Value = 1, Is.True)
        Assert.That(callCount2.Value = 1, Is.True)
    
    [<Test>]
    member this.``SimpleLazy - возвращает тот же объект при повторных вызовах`` () =
        let supplier () = obj()
        let lazyValue = SimpleLazy(supplier) :> ILazy<obj>
        
        let first = lazyValue.Get()
        let second = lazyValue.Get()
        
        Assert.That((first = second), Is.True)
    
    [<Test>]
    member this.``ThreadSafeLazy - в многопоточном окружении вычисляется один раз`` () =
        let callCount = ref 0
        let mutable results = []
        let lockObj = obj()
        
        let supplier () = 
            Thread.Sleep(10)
            Interlocked.Increment(callCount) |> ignore
            42
        
        let lazyValue = ThreadSafeLazy(supplier) :> ILazy<int>
        
        let threads = 
            [for _ in 1..10 ->
                Thread(fun () ->
                    let result = lazyValue.Get()
                    lock lockObj (fun () -> results <- result :: results)
                ) ]
        
        threads |> List.iter _.Start()
        threads |> List.iter _.Join()

        Assert.That(callCount.Value = 1, Is.True)
        Assert.That(results |> List.forall ((=) 42), Is.True)
    
    [<Test>]
    member this.``ThreadSafeLazy - не блокирует чтение после вычисления`` () =
        let supplier () = 
            Thread.Sleep(50)
            100
        
        let lazyValue = ThreadSafeLazy(supplier) :> ILazy<int>
        
        let first = lazyValue.Get()
        
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        let second = lazyValue.Get()
        stopwatch.Stop()
        
        Assert.That((first = 100), Is.True)
        Assert.That((second = 100), Is.True)
        Assert.That(stopwatch.ElapsedMilliseconds < 30, Is.True)
    
    [<Test>]
    member this.``LockFreeLazy - всегда возвращает один и тот же результат`` () =
        let callCount = ref 0
        let supplier () = 
            Interlocked.Increment(callCount) |> ignore
            99
        
        let lazyValue = LockFreeLazy(supplier) :> ILazy<int>
        
        let results = System.Collections.Concurrent.ConcurrentBag<int>()
        
        let threads = 
            [for _ in 1..10 ->
                Thread(fun () ->
                    results.Add(lazyValue.Get())
                ) ]
        
        threads |> List.iter _.Start()
        threads |> List.iter _.Join()

        let resultsList = results |> Seq.toList
        Assert.That(resultsList |> List.forall ((=) 99), Is.True)
    