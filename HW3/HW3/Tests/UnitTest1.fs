namespace HW3.Tests

open NUnit.Framework
open HW3

[<TestFixture>]
type LambdaInterpreterTests () =
    [<Test>]
    member this.Test_Variable_Substitution () =
        // Тест: замена переменной x на N в Var x
        let term = Var "x"
        let replacement = Abs ("y", Var "y")
        let result = Interpreter.subst term "x" replacement
        Assert.That(result, Is.EqualTo(replacement), "Var x должна заменяться на replacement")
    
    [<Test>]
    member this.Test_No_Substitution_When_Var_Names_Differ () =
        // Тест: замена x на N в Var y (y != x) не должна ничего менять
        let term = Var "y"
        let replacement = Abs ("x", Var "x")
        let result = Interpreter.subst term "x" replacement
        Assert.That(result, Is.EqualTo(Var "y"), "Var y не должна заменяться при замене x")
    
    [<Test>]
    member this.Test_No_Substitution_In_Abs_When_Parameter_Matches_X () =
        // Тест: λx. body не должна изменяться при замене x
        let term = Abs ("x", Var "x")
        let replacement = Abs ("y", Var "y")
        let result = Interpreter.subst term "x" replacement
        Assert.That(result, Is.EqualTo(term), "λx. ... не должна изменяться при замене x")
    
    [<Test>]
    member this.Test_Substitution_Inside_Abs_When_Parameter_Differs () =
        // Тест: замена x на N внутри λy. body, где y != x и нет конфликта
        let term = Abs ("y", Var "x")
        let replacement = Var "z"
        let expected = Abs ("y", Var "z")
        let result = Interpreter.subst term "x" replacement
        Assert.That(result, Is.EqualTo(expected), "Должна произойти замена внутри тела")
    
    [<Test>]
    member this.Test_Alpha_Conversion_When_Name_Conflict () =
        // Тест: замена x на y в λy. x должна дать λz. y (с новым именем)
        let term = Abs ("y", Var "x")
        let replacement = Var "y"
        let result = Interpreter.subst term "x" replacement
        
        match result with
        | Abs (z, body) ->
            Assert.That(z, Is.Not.EqualTo("y"), "Имя параметра должно быть изменено")
            match body with
            | Var v -> Assert.That(v, Is.EqualTo("y"), "Тело должно содержать replacement")
            | _ -> Assert.Fail("Ожидалась переменная в теле")
        | _ -> Assert.Fail("Ожидалась абстракция")
    
    [<Test>]
    member this.Test_Substitution_In_Application () =
        // Тест: замена в (M N)
        let term = App (Var "x", Var "y")
        let replacement = Var "z"
        let expected = App (Var "z", Var "y")
        let result = Interpreter.subst term "x" replacement
        Assert.That(result, Is.EqualTo(expected), "Замена должна применяться к обоим подтермам")
    
    [<Test>]
    member this.Test_Beta_Reduction_Simple () =
        // Тест: (λx. x) y → y
        let term = App (Abs ("x", Var "x"), Var "y")
        let expected = Var "y"
        let result = Interpreter.reduce term
        Assert.That(result, Is.EqualTo(expected), "Бета-редукция тождественной функции")
    
    [<Test>]
    member this.Test_Beta_Reduction_With_Substitution () =
        // Тест: (λx. λy. x) z → λy. z
        let term = App (Abs ("x", Abs ("y", Var "x")), Var "z")
        let expected = Abs ("y", Var "z")
        let result = Interpreter.reduce term
        Assert.That(result, Is.EqualTo(expected), "Бета-редукция с вложенной абстракцией")
    
    [<Test>]
    member this.Test_Reduce_Under_Abs () =
        // Тест: редукция внутри абстракции: λx. (λy. y) x → λx. x
        let term = Abs ("x", App (Abs ("y", Var "y"), Var "x"))
        let expected = Abs ("x", Var "x")
        let result = Interpreter.reduce term
        Assert.That(result, Is.EqualTo(expected), "Редукция должна происходить внутри абстракции")
    
    [<Test>]
    member this.Test_Reduce_Application_Left_First () =
        // Тест: редукция сначала левого терма в аппликации
        let term = App (App (Abs ("x", Abs ("y", Var "x")), Var "z"), Var "w")
        let expected = App (Abs ("y", Var "z"), Var "w")
        let result = Interpreter.reduce term
        Assert.That(result, Is.EqualTo(expected), "Сначала должен редуцироваться левый терм")
    
    [<Test>]
    member this.Test_Normalize_Returns_Some_When_Normal_Form_Reached () =
        // Тест: нормализация завершается и возвращает нормальную форму
        let term = App (Abs ("x", Var "x"), Var "y")
        let expected = Var "y"
        let result = Interpreter.normalize 100 term
        Assert.That(result, Is.EqualTo(Some expected), "Должна быть найдена нормальная форма")
    
    [<Test>]
    member this.Test_Normalize_With_Zero_MaxSteps () =
        // Тест: maxSteps = 0
        let term = Var "x"
        let result = Interpreter.normalize 0 term
        Assert.That(result, Is.EqualTo(None), "При maxSteps = 0 сразу возвращается None")
    
    [<Test>]
    member this.Test_Church_Numeral_Successor () =
        // Тест: вычисление функции следования для чисел Чёрча
        // succ = λn. λf. λx. f (n f x)
        let succ = Abs ("n", Abs ("f", Abs ("x", 
                    App (Var "f", App (App (Var "n", Var "f"), Var "x")))))
        let zero = Abs ("f", Abs ("x", Var "x"))
        let one = App (succ, zero)
        let result = Interpreter.normalize 100 one
        
        match result with
        | Some (Abs ("f", Abs ("x", App (Var "f", Var "x")))) -> 
            Assert.Pass("Функция следования работает")
        | Some _ -> 
            Assert.Fail("Результат не соответствует числу 1")
        | None -> 
            Assert.Fail("Нормализация не завершилась")
    
    [<Test>]
    member this.Test_FreeVars_Simple () =
        // Тест: свободные переменные в Var
        let term = Var "x"
        let result = Interpreter.freeVars term
        Assert.That(result, Is.EqualTo(Set.singleton "x"), "Var x должна содержать x")
    
    [<Test>]
    member this.Test_FreeVars_In_Abs () =
        // Тест: свободные переменные в λx. y
        let term = Abs ("x", Var "y")
        let result = Interpreter.freeVars term
        Assert.That(result, Is.EqualTo(Set.singleton "y"), "y должна быть свободна, x — связана")
    
    [<Test>]
    member this.Test_FreeVars_In_App () =
        // Тест: свободные переменные в (x y)
        let term = App (Var "x", Var "y")
        let result = Interpreter.freeVars term
        let expected = Set.ofList ["x"; "y"]
        Assert.That(result, Is.EqualTo(expected), "Обе переменные должны быть свободны")
    
    [<Test>]
    member this.Test_FreshName_Generates_Unique_Name () =
        // Тест: freshName генерирует имя, отсутствующее в used
        let used = Set.ofList ["x"; "x'"; "x''"]
        let result = Interpreter.freshName "x" used
        Assert.That(result, Is.EqualTo("x'''"), "Должно быть сгенерировано уникальное имя")
    
    [<Test>]
    member this.Test_FreshName_Returns_Original_If_Free () =
        // Тест: freshName возвращает hint, если он не используется
        let used = Set.ofList ["y"; "z"]
        let result = Interpreter.freshName "x" used
        Assert.That(result, Is.EqualTo("x"), "Hint должен быть возвращён, если он свободен")