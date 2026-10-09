import std/strformat
import std/strutils
import std/sequtils

type
  Instrument = enum Guitar, Bass
  TabHeader = object
    song:       string
    artist:     string
    instrument: string
    tuning:     seq[char]

proc parseTuning(stun: string): seq[char] =
  let notes = stun.split()
  for note in notes:
    if note.len != 1:
      raise newException(ValueError, fmt"bad tuning, got {note}")
    result.add note[0]

proc parseHeader(fname: string): TabHeader =
  const delim = "::::"
  let f = fname.open()
  defer: f.close()

  let data  = f.readAll()
  let start = data.find(delim)
  let fin   = data.find(delim, start + delim.len)

  if start == -1 or fin == -1:
    raise newException(ValueError, "delimiter not found in tab file")

  let rhead = data[start + delim.len + 1..<fin].strip()
  let split = rhead.splitLines()

  if split.len != 4:
    raise newException(ValueError, fmt"bad tab header, got {split.len} entries")

  for line in split:
    let entry = line.split(":").mapIt(it.strip)
    if entry.len != 2: 
      raise newException(ValueError, fmt"bad tab header, got {line}")

    case entry[0]
    of "song":       result.song       = entry[1]
    of "artist":     result.artist     = entry[1]
    of "instrument": result.instrument = entry[1]
    of "tuning":     result.tuning     = entry[1].parseTuning
    else: 
      raise newException(
        ValueError,
        fmt"bad tab header, invalid element {entry[0]}"
      )

when is_main_module:
  echo parseHeader("tab.txt")
