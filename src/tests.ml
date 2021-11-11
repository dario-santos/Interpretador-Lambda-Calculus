open Lambda

module MachineFisherPlotkin = Machine.MakeMachine (Beta_redux.Eager) (Cps.FisherPlotkin)

let () = Printf.printf "\n\nMachineFisherPlotkin:\n\n" 

let t1 = Abs(App(Abs(Abs(Var 0)), Var 0))
let v1_obtained = MachineFisherPlotkin.exec v1
let v1_expected = Abs(Abs(Var 0))

let () = Printf.printf "Obtained: %s\nExpected: %s\n\n" (MachineFisherPlotkin.string_of_lambda v1_obtained) (MachineFisherPlotkin.string_of_lambda v1_expected)
