open! Base
open React_trace

let fuel = 100

let set_in_effect_step_two_times () =
  let prog =
    let open Syntax in
    Prog.(
      Comp
        ( {
            name = "C";
            param = "x";
            body =
              Expr.(
                Stt
                  {
                    label = 0;
                    stt = "s";
                    set = "setS";
                    init = Const (Int 42);
                    body =
                      Seq
                        ( Eff
                            (App
                               {
                                 fn = Var "setS";
                                 arg = Fn { param = "s"; body = Const (Int 43) };
                               }),
                          View [ Const Unit ] );
                  });
          },
          Expr Expr.(View [ App { fn = Var "C"; arg = Const Unit } ]) ))
  in
  let { Interp.steps } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step two times" ~expected:2 ~actual:steps

let set_in_effect_step_indefinitely () =
  let prog =
    let open Syntax in
    Prog.(
      Comp
        ( {
            name = "C";
            param = "x";
            body =
              Expr.(
                Stt
                  {
                    label = 0;
                    stt = "s";
                    set = "setS";
                    init = Const (Int 42);
                    body =
                      Seq
                        ( Eff
                            (App
                               {
                                 fn = Var "setS";
                                 arg =
                                   Fn
                                     {
                                       param = "s";
                                       body =
                                         Bin_op
                                           {
                                             op = Plus;
                                             left = Var "s";
                                             right = Const (Int 1);
                                           };
                                     };
                               }),
                          View [ Const Unit ] );
                  });
          },
          Expr Expr.(View [ App { fn = Var "C"; arg = Const Unit } ]) ))
  in
  let { Interp.steps } = Interp.run ~fuel prog in
  Alcotest.(check' int) ~msg:"step indefintely" ~expected:fuel ~actual:steps

let () =
  let open Alcotest in
  run "Interpreter"
    [
      ( "steps",
        [
          test_case "Set in effect should step two times" `Quick
            set_in_effect_step_two_times;
          test_case "Set in effect should step indefintely" `Quick
            set_in_effect_step_indefinitely;
        ] );
    ]
