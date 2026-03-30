module Tests

open System
open NUnit.Framework
open Workflow.Calculats

[<TestFixture>]
type CalculationTests() =
    let calculate = theNumberCalculate()
    let rounding precision = RoundingBuilder(precision)
    
    [<Test>]
    member this.``Test_Calculate_With_Valid_Numbers_Returns_Sum`` () =
        let result = calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }
        
        Assert.That(result, Is.EqualTo(Some("3")), "должно возвращать значение, содержащее 3")

    [<Test>]
    member this.``Test_Calculate_With_Invalid_String_Returns_None`` () =
        let result = calculate {
            let! x = "1"
            let! y = "Ъ"
            let z = x + y
            return z
        }
        
        Assert.That(result, Is.EqualTo(None), "должно возвращать значение, указывающее на отсутствие результата")
    

    [<Test>]
    member this.``Test_Rounding_With_Precision_3_Returns_0_048`` () =
        let rez = rounding 3 {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }
        
        match rez with
        | Some value -> 
            Assert.That(Math.Round(value, 3), Is.EqualTo(0.048), 
                "Результат должен быть 0.048 после округления")
        | None -> 
            Assert.Fail("Результат не должен быть None")
            