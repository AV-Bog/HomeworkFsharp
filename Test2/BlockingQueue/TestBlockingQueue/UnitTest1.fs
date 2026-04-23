module TestBlockingQueue

open NUnit.Framework
open System.Threading

open BlockingQueue

[<TestFixture>]
type BlockingQueueTests() =
    
    [<Test>]
    member this.Enqueue_And_Dequeue_Should_Work_In_Single_Thread() =
        let queue = BlockingQueue<int>()
        
        queue.Enqueue(42)
        let result = queue.Dequeue()
        
        Assert.That(result, Is.EqualTo(42))
    
    [<Test>]
    member this.Dequeue_Should_Block_When_Queue_Is_Empty() =
        let queue = BlockingQueue<int>()
        let mutable blockedFlag = false
        
        let consumer = Thread(fun () ->
            blockedFlag <- true
            let value = queue.Dequeue()
            blockedFlag <- false
            Assert.That(value, Is.EqualTo(1))
        )
        
        consumer.Start()
        Thread.Sleep(100)
        
        Assert.That(blockedFlag, Is.True, "Поток должен быть заблокирован")
        
        queue.Enqueue(1)
        Thread.Sleep(100)
        
        Assert.That(blockedFlag, Is.False, "Поток должен разблокироваться")
        consumer.Join()
    
    [<Test>]
    member this.TryDequeue_Should_Return_None_On_Timeout() =
        let queue = BlockingQueue<int>()
        
        let result = queue.TryDequeue(100)
        
        Assert.That(result, Is.EqualTo(None))
    
    [<Test>]
    member this.TryDequeue_Should_Return_Value_When_Available() =
        let queue = BlockingQueue<int>()
        
        queue.Enqueue(42)
        let result = queue.TryDequeue(100)
        
        Assert.That(result, Is.EqualTo(Some(42)))
    
    [<Test>]
    member this.Count_Should_Return_Correct_Number_Of_Elements() =
        let queue = BlockingQueue<int>()
        
        Assert.That(queue.Count, Is.EqualTo(0))
        
        queue.Enqueue(1)
        queue.Enqueue(2)
        queue.Enqueue(3)
        
        Assert.That(queue.Count, Is.EqualTo(3))
        
        queue.Dequeue() |> ignore
        
        Assert.That(queue.Count, Is.EqualTo(2))
    
    [<Test>]
    member this.Clear_Should_Remove_All_Elements() =
        let queue = BlockingQueue<int>()
        
        queue.Enqueue(1)
        queue.Enqueue(2)
        queue.Enqueue(3)
        queue.Clear()
        
        Assert.That(queue.Count, Is.EqualTo(0))
    
    [<Test>]
    member this.Multiple_Producers_And_Consumers_Should_Work_Correctly() =
        let queue = BlockingQueue<int>()
        let producedCount = 100
        let mutable consumedCount = 0
        let lockObj = obj()
        
        let producers = Array.init 5 (fun _ ->
            Thread(fun () ->
                for i in 1..producedCount/5 do
                    queue.Enqueue(i)
            )
        )
        
        let consumers = Array.init 5 (fun _ ->
            Thread(fun () ->
                for _ in 1..producedCount/5 do
                    queue.Dequeue() |> ignore
                    lock lockObj <| fun () -> consumedCount <- consumedCount + 1
            )
        )
        
        producers |> Array.iter (fun p -> p.Start())
        consumers |> Array.iter (fun c -> c.Start())
        
        producers |> Array.iter (fun p -> p.Join())
        consumers |> Array.iter (fun c -> c.Join())
        
        Assert.That(consumedCount, Is.EqualTo(producedCount))
        Assert.That(queue.Count, Is.EqualTo(0))
    
    [<Test>]
    member this.Dequeue_Should_Wait_For_Enqueue_From_Another_Thread() =
        let queue = BlockingQueue<int>()
        let mutable enqueued = false
        let mutable dequeued = false
        
        let producer = Thread(fun () ->
            Thread.Sleep(500)
            queue.Enqueue(100)
            enqueued <- true
        )
        
        let consumer = Thread(fun () ->
            let value = queue.Dequeue()
            dequeued <- true
            Assert.That(value, Is.EqualTo(100))
        )
        
        consumer.Start()
        Thread.Sleep(100)
        producer.Start()
        
        producer.Join()
        consumer.Join()
        
        Assert.That(enqueued, Is.True)
        Assert.That(dequeued, Is.True)