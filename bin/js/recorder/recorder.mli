type recording = { checkpoints : string list; log : string }

include Recorder_intf.Intf with type recording := recording
