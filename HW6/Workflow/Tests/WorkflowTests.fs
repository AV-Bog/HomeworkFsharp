module WorkflowTests

open NUnit.Framework
open Workflow.Calculator
open Workflow.RoundingBuilder

let calculate = StringCalculationBuilder()
let rounding precision = RoundingBuilder(precision)

[<Test>]
let Test_Calculate_With_Valid_Numbers_Returns_Sum () =
    let result = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    
    Assert.That(result, Is.EqualTo(Some "3"), "должно возвращать значение, содержащее 3")

[<Test>]
let Test_Calculate_With_Invalid_String_Returns_None () =
    let result = calculate {
        let! x = "1"
        let! y = "b"
        let z = x + y
        return z
    }
    
    Assert.That(result, Is.EqualTo(None), "должно возвращать значение, указывающее на отсутствие результата")

[<Test>]
let Test_Rounding_With_Precision_3_Returns_0_048 () =
    let rez = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    
    Assert.That(rez, Is.EqualTo(Some 0.048), "Результат должен быть Some 0.048")

[<Test>]
let Test_Rounding_With_Precision_0_Returns_Integer () =
    let rez = rounding 0 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    
    Assert.That(rez, Is.EqualTo(Some 0.0), "Округление до 0 знаков должно дать 0")

[<Test>]
let Test_Rounding_With_Negative_Precision_Minus_1_Returns_None () =
    let rez = rounding -1 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    
    Assert.That(rez, Is.EqualTo(None), "Отрицательная точность должна возвращать None")

[<Test>]
let Test_Rounding_With_Precision_2_Returns_Correct_Rounded_Value () =
    let rez = rounding 2 {
        let! a = 1.23456
        let! b = 3.0
        return a * b
    }
    
    Assert.That(rez, Is.EqualTo(Some 3.69), "Округление до 2 знаков должно дать 3.69")

[<Test>]
let Test_Calculate_With_Empty_String_Returns_None () =
    let result = calculate {
        let! x = ""
        let! y = "5"
        let z = x + y
        return z
    }
    
    Assert.That(result, Is.EqualTo(None), "Пустая строка должна возвращать None")

[<Test>]
let Test_Calculate_With_Negative_Numbers_Returns_Sum () =
    let result = calculate {
        let! x = "-10"
        let! y = "7"
        let z = x + y
        return z
    }
    
    Assert.That(result, Is.EqualTo(Some "-3"), "Сумма отрицательных чисел должна быть -3")