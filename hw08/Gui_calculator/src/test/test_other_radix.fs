module test_other_radix
open NUnit.Framework
open other_radix

// перевод в шестнадцатиричную
[<TestCase ("12", Result = "C")>] // проверка правильности перевода в буквы
[<TestCase ("32", Result = "20")>] // проверка правильности перевода в цифры
[<TestCase ("74", Result = "4A")>] // проверка правильности смешанного перевода
let ``Tests to hex`` in_str =
    convertTo_16 (in_str)
    
// перевод в двоичную и восмеричную
[<TestCase ("8", 2, Result = 1000)>] // перевод в двоичную
[<TestCase ("37", 2, Result = 100101)>] // перевод в двоичную
[<TestCase ("10", 8, Result = 12)>] // перевод в восьмиричную
[<TestCase ("99", 8, Result = 143)>] // перевод в восьмиричную
let ``Tests to bin and oct`` in_str bas =
    convertToBase (in_str) (bas)

// перевод из двоичной, восьмиричной м шестнадцатиричной систем
[<TestCase ("1010", 2, Result = 10)>] // перевод из двоичной
[<TestCase ("1010111", 2, Result = 87)>] // перевод из двоичной
[<TestCase ("11", 8, Result = 9)>] // перевод из восьмиричной
[<TestCase ("154", 8, Result = 108)>] // перевод из восьмиричной
[<TestCase ("16", 16, Result = 22)>] // перевод из шестнадцатиричной
[<TestCase ("F5", 16, Result = 245)>] // перевод из шестнадцатиричной
let ``Tests from other`` in_str bas =
    to10FromOther (in_str) (bas)

