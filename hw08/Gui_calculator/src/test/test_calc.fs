module test_calc
open Calculator
open NUnit.Framework

[<TestCase ("1 + 3", Result = 4)>]
[<TestCase ("7 + 2 * 8", Result = 23)>]
[<TestCase ("(5 - 1) / 2", Result = 2)>]
[<TestCase ("(1 + 1 + 1 + 1 + 1 + 1 + 1 + 2) * 0", Result = 0)>]
[<TestCase ("1 - 2 - 3", Result = -4)>]
[<TestCase ("3 ^ 1 ^ 2", Result = 3)>]
[<TestCase ("334 ^ 1 ^ 2", Result = 334)>]
[<TestCase ("65 ^ 0", Result = 1)>]
[<TestCase ("32 ^ 1", Result = 32)>]
[<TestCase ("11 * 0 - 7", Result = -7)>]
[<TestCase ("2 - 56", Result = -54)>]
let ``Tests for calculator`` in_string =
    calculator in_string