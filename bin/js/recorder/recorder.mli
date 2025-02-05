type tree = { path : string; name : string; children : tree list }
and entry = { msg : string; tree : tree } [@@deriving yojson_of]

and recording = { checkpoints : entry list; log : string }
[@@deriving yojson_of]

include Recorder_intf.Intf with type recording := recording
