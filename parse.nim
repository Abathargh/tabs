import std/enumerate
import std/strformat
import std/strutils
import std/sequtils
import std/tables

export tables

type
  TabHeader* = object
    song*:       string
    artist*:     string
    instrument*: string
    tuning*:     seq[char]

  StringNotation* = object
    note*:  int
    index*: int

  TabPart* = object
    repeats*: int
    tabs*: Table[int, seq[StringNotation]]

  TabSection* = object
    name*:    string
    parts*:   seq[TabPart]

  Tablature* = object
    header*:  TabHeader
    sections*: seq[TabSection]

proc parseTuning(stun: string): seq[char] =
  let notes = stun.split()
  for note in notes:
    if note.len != 1:
      raise newException(ValueError, fmt"bad tuning, got {note}")
    result.add note.toLower()[0]

proc parseHeader(data: string): TabHeader =
  let split = data.splitLines()
  if  split.len != 4:
    raise newException(ValueError, 
      fmt"bad tab header, got {split.len} entries")

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

proc parseNotations(data: string): seq[StringNotation] =
  let entries = data.strip.split(";")
  for entry in entries:
    let pairs = entry.split(",").mapIt(it.parseInt)
    
    if pairs.len != 2:
      raise newException(ValueError, fmt"bad tab entry: {entry}")

    let note = pairs[0]
    let idx  = pairs[1]

    if note < 0 or idx < 0:
      raise newException(ValueError, fmt"note ({note}) or idx ({idx}) < 0")

    result.add StringNotation(note: note, index: idx)

proc parsePart(data: string, tuning: seq[char]): TabPart =
  let split = data.splitLines()
  if  split.len < 2:
    raise newException(ValueError, fmt"bad part, got {split.len} entries")

  for line in split:
    let entry = line.split(":").mapIt(it.strip)
    if entry.len != 2: 
      raise newException(ValueError, fmt"bad part, got {line}")

    case entry[0]
    of "repeats": result.repeats = entry[1].parseInt
    else:
      let note = entry[0].parseInt()

      if note < 0 or note >= tuning.len:
        raise newException(ValueError, fmt"string {note} not in range for tuning")

      if note in result.tabs:
        raise newException(ValueError, fmt"string {note} duplicate in part")

      result.tabs[note] = parseNotations(entry[1])

proc parseName(data: string): string =
  let entries = data.splitLines
  if  entries.len != 1:
    raise newException(ValueError, fmt"bad section header, got {entries}")

  let elems = entries[0].split(":").mapIt(it.strip) 
  if  elems.len != 2 or elems[0] != "name":
    raise newException(ValueError, fmt"bad section header, got {entries}")
  elems[1]


proc parseSection(data: string, tuning: seq[char]): TabSection =
  const delim = "----"

  let split = data.split(delim).mapIt(it.strip)
  if  split.len < 1:
    raise newException(ValueError, fmt"bad section, got {split.len} entries")

  for i, section in enumerate(split):
    if i == 0:
      result.name = parseName(section)
      continue

    result.parts.add parsePart(section, tuning)

proc parseTablature*(fname: string): Tablature =
  const delim = "::::"
  let f = fname.open()
  defer: f.close()

  let data     = f.readAll()
  let sections = data.split(delim).mapIt(it.strip)

  if sections.len == 0:
    raise newException(ValueError, "invalid tablature: no header found")

  for i, section in enumerate(sections):
    if i == 0:
      result.header = parseHeader(section)
      continue
    result.sections.add parseSection(section, result.header.tuning)

