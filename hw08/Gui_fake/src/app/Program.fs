// calculator and science calculator
// ex. 42 & 43
// Author: Kudryashova Anna

(* Калькулятор работает только с int-овыми числами 
   1. Считает выражения с использованием скобок. 
      для вычисления: ввести выражение и нажать "="
   2. Считает тригонометрические функции от чисел или выражений. 
      для того, чтобы посчитать тригонометрическую функцию от числа: ввести число и нажать знак функции
      для того, чтобы посчитать тригонометрическую функцию от выражения: ввести выражение в скобках и нажать знак функции
   3. Переводит из десятичной в двоичную, восьмиричную или шестнадцатиричную системы и обратно
      для того, чтобы осуществить перевод необходимо ввести начальное число и нажать кнопку необходимого преобразования
*)

module ex42_1
open System.Drawing
open System
open System.Windows.Forms
open Calculator
open other_radix


let programLabel =
  let lbl = new Label()
  lbl.Location <- System.Drawing.Point(40, 26)
  lbl.Height <- 20
  lbl.Width <- 450
  lbl.BorderStyle <- BorderStyle.Fixed3D
  lbl.BackColor <-System.Drawing.Color.GhostWhite
  lbl.AutoSize <- false
  lbl
let textLabel =
  let text = new Label()
  text.Location <-System.Drawing.Point(40, 6)
  text.Text <- "Result:"
  text

let expresionLabel =
  let lbl = new Label()
  lbl.Location <- System.Drawing.Point(40, 74)
  lbl.Height <- 20
  lbl.Width <- 450
  lbl.BorderStyle <- BorderStyle.Fixed3D
  lbl.BackColor <-System.Drawing.Color.GhostWhite
  lbl.AutoSize <- false
  lbl

let textLabel2 =
  let text = new Label()
  text.Location <-System.Drawing.Point(40, 51)
  text.Text <- "Expresion:"
  text



let exitButton =
  let but = new Button()
  but.Location <- System.Drawing.Point(415,265)
  but.Text <- "Exit"
  but.Click.Add (fun e -> Application.Exit())
  but

let mutable in_string = ""
let mutable temp = ""
let mutable flag = 0

let addTostring (exp:char) args =
  if exp = '(' || exp = ')'|| System.Char.IsLetterOrDigit(exp)
    then temp <- temp + exp.ToString()
  else in_string <- in_string + temp
       in_string <- in_string + " "
       in_string <- in_string + exp.ToString()
       in_string <- in_string + " "
       flag <- 1
       temp <- ""
  
let A_Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(290,145)
  but.Text <- "A"
  but.Width <- 66
  but.Click.Add (addTostring 'A')
  but

let B_Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(356,145)
  but.Text <- "B"
  but.Width <- 67
  but.Click.Add (addTostring 'B')
  but

let C_Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(423,145)
  but.Text <- "C"
  but.Width <- 66
  but.Click.Add (addTostring 'C')
  but

let D_Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(290,168)
  but.Text <- "D"
  but.Width <- 66
  but.Click.Add (addTostring 'D')
  but

let E_Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(356,168)
  but.Text <- "E"
  but.Width <- 67
  but.Click.Add (addTostring 'E')
  but

let F_Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(423,168)
  but.Text <- "F"
  but.Width <- 66
  but.Click.Add (addTostring 'F')
  but
 

let Number1Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(40,145)
  but.Text <- "1"
  but.Width <- 66
  but.Click.Add (addTostring '1')
  but

let Number2Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(107,145)
  but.Text <- "2"
  but.Width <- 67
  but.Click.Add (addTostring '2')
  but

let Number3Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(174,145)
  but.Text <- "3"
  but.Width <- 66
  but.Click.Add (addTostring '3')
  but

let Number4Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(40,168)
  but.Text <- "4"
  but.Width <- 66
  but.Click.Add (addTostring '4')
  but

let Number5Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(107,168)
  but.Text <- "5"
  but.Width <- 67
  but.Click.Add (addTostring '5')
  but

let Number6Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(174,168)
  but.Text <- "6"
  but.Width <- 66
  but.Click.Add (addTostring '6')
  but
  
let Number7Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(40,191)
  but.Text <- "7"
  but.Width <- 66
  but.Click.Add (addTostring '7')
  but

let Number8Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(107,191)
  but.Text <- "8"
  but.Width <- 67
  but.Click.Add (addTostring '8')
  but

let Number9Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(174,191)
  but.Text <- "9"
  but.Width <- 66
  but.Click.Add (addTostring '9')
  but
  
let Number0Button = 
  let but = new Button()
  but.Location <- System.Drawing.Point(107,214)
  but.Text <- "0"
  but.Width <- 67
  but.Click.Add (addTostring '0')
  but
  

let sumButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(40,110)
  but.Text <- "+"
  but.Width <- 50
  but.Click.Add (addTostring '+') 
  but


let leftPar = 
  let but = new Button()
  but.Location <- System.Drawing.Point(140,249)
  but.Text <- "("
  but.Width <- 50
  but.Click.Add (addTostring '(') 
  but

let rightPar = 
  let but = new Button()
  but.Location <- System.Drawing.Point(190,249)
  but.Text <- ")"
  but.Width <- 50
  but.Click.Add (addTostring ')') 
  but

let multiButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(140,110)
  but.Text <- "*"
  but.Width <- 50
  but.Click.Add (addTostring '*') 
  but

let subButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(90,110)
  but.Text <- "-"
  but.Width <- 50
  but.Click.Add (addTostring '-') 
  but

let divButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(190,110)
  but.Text <- "/"
  but.Width <- 50
  but.Click.Add (addTostring '/') 
  but

let powButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(40,249)
  but.Text <- "^"
  but.Width <- 50
  but.Click.Add (addTostring '^') 
  but

let modButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(90,249)
  but.Text <- "%"
  but.Width <- 50
  but.Click.Add (addTostring '%') 
  but

let resultAction args = 
  in_string <- in_string + " "
  in_string <- in_string + temp
  printf "%A\n" in_string
  let res = calculator(in_string)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let resultButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(250,145)
  but.Text <- "="
  but.Height <- 70
  but.Width <- 30
  but.Click.Add resultAction 
  but

let sinAction args = 
  if flag = 0 then in_string <- in_string + temp
                   let res = Math.Sin(Convert.ToDouble(in_string))
                   programLabel.Text <- sprintf "%A" res
  else in_string <- in_string + " "
       in_string <- in_string + temp
       let res1 = calculator(in_string)
       let res = Math.Sin(Convert.ToDouble(res1))
       programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string

let cosAction args = 
  if flag = 0 then in_string <- in_string + temp
                   let res = Math.Cos(Convert.ToDouble(in_string))
                   programLabel.Text <- sprintf "%A" res
  else in_string <- in_string + " "
       in_string <- in_string + temp
       let res1 = calculator(in_string)
       let res = Math.Cos(Convert.ToDouble(res1))
       programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string

let tgAction args = 
  if flag = 0 then in_string <- in_string + temp
                   let res = Math.Tan(Convert.ToDouble(in_string))
                   programLabel.Text <- sprintf "%A" res
  else in_string <- in_string + " "
       in_string <- in_string + temp
       let res1 = calculator(in_string)
       let res = Math.Tan(Convert.ToDouble(res1))
       programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string

let ctgAction args = 
  let mutable res = 0.0
  if flag = 1 then  in_string <- in_string + " "
                    in_string <- in_string + temp
                    res <- Convert.ToDouble(calculator(in_string))
  else in_string <- in_string + temp
       res <- Convert.ToDouble(in_string)
  if res = 0.0 then MessageBox.Show ("Division by zero!")
                    |> ignore
               elif flag = 0 then 
                                  let res = 1.0 / ( Math.Tan(Convert.ToDouble(in_string)))
                                  programLabel.Text <- sprintf "%A" res
              else 
                   let res1 = calculator(in_string)
                   let res =  1.0 /  Math.Tan(Convert.ToDouble(res1))
                   programLabel.Text <- sprintf "%A" res
                   expresionLabel.Text <- sprintf "%A" in_string

let sinButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(290,110)
  but.Text <- "sin"
  but.Width <- 50
  but.Click.Add (sinAction) 
  but

let cosButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(340,110)
  but.Text <- "cos"
  but.Width <- 50
  but.Click.Add (cosAction) 
  but

let tgButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(390,110)
  but.Text <- "tg"
  but.Width <- 50
  but.Click.Add (tgAction) 
  but

let ctgButton = 
  let but = new Button()
  but.Location <- System.Drawing.Point(440,110)
  but.Text <- "ctg"
  but.Width <- 50
  but.Click.Add (ctgAction) 
  but

let toOctAction args = 
  in_string <- in_string + temp
  printf "%A\n" in_string 
  let res = convertToBase (in_string)(8)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let To8Button =
  let but = new Button()
  but.Location <- System.Drawing.Point(356,200)
  but.Text <- "to oct"
  but.Width <- 67
  but.Click.Add (toOctAction) 
  but

let toBinAction args = 
  in_string <- in_string + temp 
  let res = convertToBase (in_string)(2)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let To2Button =
  let but = new Button()
  but.Location <- System.Drawing.Point(290,200)
  but.Text <- "to bin"
  but.Width <- 66
  but.Click.Add (toBinAction) 
  but

let toHexAction args = 
  in_string <- in_string + temp
  let res = convertTo_16 (in_string)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let To16Button =
  let but = new Button()
  but.Location <- System.Drawing.Point(423,200)
  but.Text <- "to hex"
  but.Width <- 66
  but.Click.Add (toHexAction) 
  but
  
let fromBinAction args = 
  in_string <- in_string + temp
  let res = to10FromOther (in_string)(2)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let From2Button =
  let but = new Button()
  but.Location <- System.Drawing.Point(290,223)
  but.Text <- "from bin"
  but.Width <- 66
  but.Click.Add (fromBinAction) 
  but

let fromOctAction args = 
  in_string <- in_string + temp
  let res = to10FromOther (in_string)(8)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let From8Button =
  let but = new Button()
  but.Location <- System.Drawing.Point(356,223)
  but.Text <- "from oct"
  but.Width <- 67
  but.Click.Add (fromOctAction) 
  but

let fromHexAction args = 
  in_string <- in_string + temp
  let res = to10FromOther (in_string)(16)
  programLabel.Text <- sprintf "%A" res
  expresionLabel.Text <- sprintf "%A" in_string
  

let From16Button =
  let but = new Button()
  but.Location <- System.Drawing.Point(423,223)
  but.Text <- "from hex"
  but.Width <- 66
  but.Click.Add (fromHexAction) 
  but


  





let mainForms =
  let form = new Form (Visible = false)
  form.BackColor <-System.Drawing.Color.LightSteelBlue
  
  form.Height <- 350
  form.Width <- 555
  form.FormBorderStyle <- FormBorderStyle.Fixed3D
  
  form.Controls.Add(programLabel)
  form.Controls.Add(textLabel)
  form.Controls.Add(textLabel2)
  form.Controls.Add(expresionLabel)
  form.Controls.Add(exitButton)

  form.Controls.Add(Number1Button)
  form.Controls.Add(Number2Button)
  form.Controls.Add(Number3Button)
  form.Controls.Add(Number4Button)
  form.Controls.Add(Number5Button)
  form.Controls.Add(Number6Button)
  form.Controls.Add(Number7Button)
  form.Controls.Add(Number8Button)
  form.Controls.Add(Number9Button)
  form.Controls.Add(Number0Button)

  form.Controls.Add(sumButton)
  form.Controls.Add(resultButton)
  form.Controls.Add(multiButton)
  form.Controls.Add(leftPar)
  form.Controls.Add(rightPar)
  form.Controls.Add(subButton)
  form.Controls.Add(divButton)
  form.Controls.Add(powButton)
  form.Controls.Add(modButton)

  form.Controls.Add(sinButton)
  form.Controls.Add(cosButton)
  form.Controls.Add(tgButton)
  form.Controls.Add(ctgButton)

  form.Controls.Add(A_Button)
  form.Controls.Add(B_Button)
  form.Controls.Add(C_Button)
  form.Controls.Add(D_Button)
  form.Controls.Add(E_Button)
  form.Controls.Add(F_Button)

  form.Controls.Add(To8Button)
  form.Controls.Add(To2Button)
  form.Controls.Add(To16Button)
  form.Controls.Add(From8Button)
  form.Controls.Add(From2Button)
  form.Controls.Add(From16Button)
  form



























[<EntryPoint>]
let main argv =
  mainForms.Visible <- true
  Application.Run()
  0
