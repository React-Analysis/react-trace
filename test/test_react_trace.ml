open! Base
open React_trace

let fuel = 100

let set_in_body_nonterminate () =
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
                        ( App
                            {
                              fn = Var "setS";
                              arg = Fn { param = "s"; body = Const (Int 43) };
                            },
                          View [ Const Unit ] );
                  });
          },
          Expr Expr.(View [ App { fn = Var "C"; arg = Const Unit } ]) ))
  in
  let run () =
    Interp.(re_render_limit_h (run ~fuel) prog ~re_render_limit:25) |> ignore
  in
  Alcotest.(check_raises) "retry indefintely" Interp.Too_many_re_renders run

let set_in_effect_step_three_times () =
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
  Alcotest.(check' int) ~msg:"step three times" ~expected:3 ~actual:steps

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
                                         Bop
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
          test_case "Set in body should not terminate" `Quick
            set_in_body_nonterminate;
          test_case "Set in effect should step three times" `Quick
            set_in_effect_step_three_times;
          test_case "Set in effect should step indefintely" `Quick
            set_in_effect_step_indefinitely;
        ] );
    ]
