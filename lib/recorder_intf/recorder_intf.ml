open! Base

module type Intf = sig
  type recording

  val emp_recording : recording
  val event_h : ('a -> 'b) -> 'a -> recording:recording -> 'b * recording
end
