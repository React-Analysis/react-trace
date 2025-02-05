type entry = { msg : string }
type recording = { checkpoints : entry list; log : string }

include Recorder_intf.Intf with type recording := recording
