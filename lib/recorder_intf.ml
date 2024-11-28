open! Core
open Stdlib.Effect.Deep

module type Intf = sig
  type recording

  val emp_recording : recording
  val event_h : 'a. ('a, recording:recording -> 'a * recording) handler
end
