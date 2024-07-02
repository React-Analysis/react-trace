open! Base
open React_trace

let test_prog =
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

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Sexp.pp_hum Stdlib.Format.std_formatter (Syntax.Prog.sexp_of_t test_prog);
  Interp.run test_prog;
  Stdlib.exit (if Logs.err_count () > 0 then 1 else 0)
