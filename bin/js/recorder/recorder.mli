type entry = { msg : string }

and recording = { checkpoints : entry list; log : string }
[@@deriving yojson_of]

include Recorder_intf.Intf with type recording := recording
