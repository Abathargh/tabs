import std/algorithm
import std/os
import std/sequtils
import std/strformat
import std/strutils
from std/unicode import Rune, toRunes, toUTF8, runeLen, runeSubStr, `$`

import pixie
import textalot
import parse

type
  Focus = enum
    focusSections, focusGrid

  Mode = enum
    modeNormal, modePrompt, modeConfirm

  Editor = ref object
    path:     string
    tab:      Tablature
    dirty:    bool
    focus:    Focus
    mode:     Mode
    section:  int
    part:     int
    row:      int
    col:      int
    scroll:   int
    vscroll:  int
    pending:  string
    status:   string
    label:    string
    input:    string
    onSubmit: proc(s: string)
    running:  bool

const
  cellWidth   = 3
  listWidth   = 26
  defaultCols = 16

# ---------------------------------------------------------------------------
# Model helpers
# ---------------------------------------------------------------------------

proc strings(t: Tablature): seq[char] =
  ## Every string in the tuning, highest first (as tabs are read).
  t.header.tuning.reversed()

proc slot(t: Tablature, row: int): int =
  ## Maps a grid row (highest string first) to its position in the tuning.
  t.header.tuning.len - 1 - row

proc trimStrings(p: var TabPart, count: int): int =
  ## Drops the lines of strings past the first count, e.g. after the tuning
  ## loses strings. Returns how many of them had notes.
  for i in toSeq(p.tabs.keys):
    if i >= count:
      if p.tabs[i].len > 0: inc result
      p.tabs.del i

proc fretAt(p: TabPart, s, idx: int): int =
  for n in p.tabs.getOrDefault(s):
    if n.index == idx: return n.note
  -1

proc clearFret(p: var TabPart, s, idx: int) =
  if s in p.tabs:
    p.tabs[s].keepItIf(it.index != idx)

proc setFret(p: var TabPart, s, idx, fret: int) =
  p.clearFret(s, idx)
  p.tabs.mgetOrPut(s, @[]).add StringNotation(note: fret, index: idx)
  p.tabs[s].sort(proc(a, b: StringNotation): int = cmp(a.index, b.index))

proc shiftColumns(p: var TabPart, start, delta: int) =
  ## Moves every note at index >= start by delta.
  for line in p.tabs.mvalues:
    for n in line.mitems:
      if n.index >= start: n.index += delta

proc deleteColumn(p: var TabPart, idx: int) =
  for line in p.tabs.mvalues:
    line.keepItIf(it.index != idx)
  p.shiftColumns(idx + 1, -1)

proc lastIndex(p: TabPart): int =
  result = -1
  for line in p.tabs.values:
    for n in line: result = max(result, n.index)

proc lastIndex(s: TabSection): int =
  result = -1
  for p in s.parts: result = max(result, p.lastIndex)

proc isEmpty(p: TabPart): bool =
  toSeq(p.tabs.values).allIt(it.len == 0)

proc unsavable(t: Tablature): string =
  ## The parser rejects a part with no string lines.
  for sec in t.sections:
    for k, p in sec.parts:
      if p.isEmpty:
        return &"'{sec.name}' part {k + 1} is empty, the format can't express it"
  ""

proc serialize(t: Tablature): string =
  ## Inverse of parseTablature. Strings are written by their index in the
  ## tuning, skipping the ones without notes.
  let tuning = t.header.tuning.mapIt(($it).toUpperAscii).join(" ")
  result = &"song: {t.header.song}\nartist: {t.header.artist}\n" &
           &"instrument: {t.header.instrument}\ntuning: {tuning}\n"
  for sec in t.sections:
    result.add &"::::\nname: {sec.name}\n"
    for p in sec.parts:
      result.add &"----\nrepeats: {p.repeats}\n"
      for i in 0 ..< t.header.tuning.len:
        let line = p.tabs.getOrDefault(i)
        if line.len == 0: continue
        let body = line.mapIt(&"{it.note},{it.index}").join(";")
        result.add &"{i}: {body}\n"

proc validField(s: string): bool =
  ## The format is line/colon based and splits parts on "----", so values
  ## cannot contain any of those.
  ':' notin s and '\n' notin s and "----" notin s

# ---------------------------------------------------------------------------
# Editor actions
# ---------------------------------------------------------------------------

proc current(ed: Editor): var TabSection = ed.tab.sections[ed.section]

proc currentPart(ed: Editor): var TabPart = ed.current.parts[ed.part]

proc hasSection(ed: Editor): bool =
  ed.section in 0 ..< ed.tab.sections.len

proc hasPart(ed: Editor): bool =
  ed.hasSection and ed.part in 0 ..< ed.current.parts.len

proc prompt(ed: Editor, label, initial: string, onSubmit: proc(s: string)) =
  ed.mode     = modePrompt
  ed.label    = label
  ed.input    = initial
  ed.onSubmit = onSubmit

proc confirm(ed: Editor, label: string, onYes: proc(s: string)) =
  ed.mode     = modeConfirm
  ed.label    = label & " [y/N]"
  ed.onSubmit = onYes

proc save(ed: Editor) =
  let problem = ed.tab.unsavable
  if problem.len > 0:
    ed.status = &"can't save, {problem}"
    return
  try:
    writeFile(ed.path, ed.tab.serialize)
    # Round-trip through the parser to catch anything it would reject.
    discard parseTablature(ed.path)
    ed.dirty  = false
    ed.status = &"saved {ed.path}"
  except CatchableError as e:
    ed.status = &"save failed: {e.msg}"

proc clampCursor(ed: Editor) =
  ed.section = clamp(ed.section, 0, max(ed.tab.sections.len - 1, 0))
  let parts  = if ed.hasSection: ed.current.parts.len else: 0
  ed.part    = clamp(ed.part, 0, max(parts - 1, 0))
  ed.row     = clamp(ed.row, 0, max(ed.tab.strings.len - 1, 0))
  ed.col     = max(ed.col, 0)

proc selectSection(ed: Editor, i: int) =
  if i == ed.section: return
  ed.section = i
  ed.part    = 0
  ed.vscroll = 0
  ed.clampCursor()

proc addSection(ed: Editor) =
  ed.prompt("new section name: ", "", proc(s: string) =
    if not s.validField:
      ed.status = "name cannot contain ':' or '----'"
      return
    let at = if ed.hasSection: ed.section + 1 else: 0
    let section = TabSection(name: s.strip,
                             parts: @[TabPart(repeats: 1)])
    ed.tab.sections.insert(section, at)
    ed.section = at
    ed.part    = 0
    ed.vscroll = 0
    ed.dirty   = true)

proc renameSection(ed: Editor) =
  if not ed.hasSection: return
  ed.prompt("rename section: ", ed.current.name, proc(s: string) =
    if not s.validField:
      ed.status = "name cannot contain ':' or '----'"
      return
    ed.current.name = s.strip
    ed.dirty = true)

proc deleteSection(ed: Editor) =
  if not ed.hasSection: return
  ed.confirm(&"delete section '{ed.current.name}'?", proc(s: string) =
    ed.tab.sections.delete ed.section
    ed.part    = 0
    ed.vscroll = 0
    ed.clampCursor()
    ed.dirty = true)

proc duplicateSection(ed: Editor) =
  if not ed.hasSection: return
  var copy = ed.current
  copy.name &= " (copy)"
  ed.tab.sections.insert(copy, ed.section + 1)
  ed.selectSection(ed.section + 1)
  ed.dirty = true

proc moveSection(ed: Editor, delta: int) =
  let target = ed.section + delta
  if not ed.hasSection or target notin 0 ..< ed.tab.sections.len: return
  swap(ed.tab.sections[ed.section], ed.tab.sections[target])
  ed.section = target
  ed.dirty   = true

proc addPart(ed: Editor) =
  ## Inserts an empty part below the current one and moves onto it.
  if not ed.hasSection: return
  let at = if ed.hasPart: ed.part + 1 else: 0
  ed.current.parts.insert(TabPart(repeats: 1), at)
  ed.part = at
  ed.row  = 0
  ed.dirty = true

proc duplicatePart(ed: Editor) =
  if not ed.hasPart: return
  ed.current.parts.insert(ed.currentPart, ed.part + 1)
  inc ed.part
  ed.dirty = true

proc deletePart(ed: Editor) =
  if not ed.hasPart: return
  ed.confirm(&"delete part {ed.part + 1} of '{ed.current.name}'?",
             proc(s: string) =
    ed.current.parts.delete ed.part
    ed.clampCursor()
    ed.dirty = true)

proc movePart(ed: Editor, delta: int) =
  let target = ed.part + delta
  if not ed.hasPart or target notin 0 ..< ed.current.parts.len: return
  swap(ed.current.parts[ed.part], ed.current.parts[target])
  ed.part  = target
  ed.dirty = true

proc setRepeats(ed: Editor) =
  if not ed.hasPart: return
  ed.prompt("repeats: ", $ed.currentPart.repeats, proc(s: string) =
    try:
      let n = s.strip.parseInt
      if n < 0: raise newException(ValueError, "negative")
      ed.currentPart.repeats = n
      ed.dirty = true
    except ValueError:
      ed.status = &"invalid repeats: {s}")

proc editTuning(ed: Editor) =
  let current = ed.tab.header.tuning.mapIt(($it).toUpperAscii).join(" ")
  ed.prompt("tuning (e.g. E A D G): ", current, proc(s: string) =
    let notes = s.splitWhitespace()
    if notes.len == 0 or notes.anyIt(it.len != 1 or not it[0].isAlphaAscii):
      ed.status = &"bad tuning: {s}"
      return
    ed.tab.header.tuning = notes.mapIt(it.toLowerAscii[0])
    var dropped = 0
    for sec in ed.tab.sections.mitems:
      for p in sec.parts.mitems:
        dropped += p.trimStrings(ed.tab.header.tuning.len)
    if dropped > 0:
      ed.status = &"dropped notes on {dropped} removed string(s)"
    ed.clampCursor()
    ed.dirty = true)

proc defaultTuning(instrument: string): seq[char] =
  ## Standard tuning, low to high, used to prefill the prompt on new files.
  let ins = instrument.toLowerAscii
  if "bass" in ins:      @['e', 'a', 'd', 'g']
  elif "ukulele" in ins: @['g', 'c', 'e', 'a']
  else:                  @['e', 'a', 'd', 'g', 'b', 'e']

proc editHeader(ed: Editor, fresh = false) =
  ## Chains one prompt per header field. On a fresh file the tuning prompt
  ## is prefilled with the standard tuning for the chosen instrument.
  ed.prompt("song: ", ed.tab.header.song, proc(song: string) =
    if song.validField: ed.tab.header.song = song.strip
    ed.prompt("artist: ", ed.tab.header.artist, proc(artist: string) =
      if artist.validField: ed.tab.header.artist = artist.strip
      ed.prompt("instrument: ", ed.tab.header.instrument, proc(ins: string) =
        if ins.validField: ed.tab.header.instrument = ins.strip
        if fresh: ed.tab.header.tuning = defaultTuning(ed.tab.header.instrument)
        ed.dirty = true
        ed.editTuning())))

proc quit(ed: Editor) =
  if ed.dirty:
    ed.confirm("unsaved changes, quit anyway?", proc(s: string) =
      ed.running = false)
  else:
    ed.running = false

proc reset(ed: Editor, path: string, tab: Tablature) =
  ed.path    = path
  ed.tab     = tab
  ed.dirty   = false
  ed.focus   = focusSections
  ed.section = 0
  ed.part    = 0
  ed.row     = 0
  ed.col     = 0
  ed.scroll  = 0
  ed.vscroll = 0

proc loadFile(ed: Editor, path: string) =
  ed.reset(path, parseTablature(path))
  ed.status = &"opened {path}"

proc startNew(ed: Editor, path: string) =
  ## Starts an empty tablature and walks through the header prompts.
  var tab: Tablature
  tab.header = TabHeader(song: "untitled", artist: "unknown",
                         instrument: "guitar", tuning: defaultTuning("guitar"))
  ed.reset(path, tab)
  ed.dirty = true
  ed.editHeader(fresh = true)

proc guardUnsaved(ed: Editor, action: proc()) =
  if ed.dirty:
    ed.confirm("unsaved changes, discard them?", proc(s: string) = action())
  else:
    action()

proc newFile(ed: Editor) =
  ed.guardUnsaved(proc() =
    ed.prompt("new file path: ", "", proc(path: string) =
      let path = path.strip
      if path.len == 0: return
      if fileExists(path):
        ed.status = &"{path} already exists, press o to open it"
        return
      ed.startNew(path)))

proc openFile(ed: Editor) =
  ed.guardUnsaved(proc() =
    ed.prompt("open file: ", "", proc(path: string) =
      let path = path.strip
      if not fileExists(path):
        ed.status = &"{path} does not exist, press N to create it"
        return
      try:
        ed.loadFile(path)
      except CatchableError as e:
        ed.status = &"open failed: {e.msg}"))

# ---------------------------------------------------------------------------
# PNG export
# ---------------------------------------------------------------------------

# JetBrains Mono (SIL OFL 1.1, see assets/fonts/OFL.txt), loaded at runtime
# from assets/fonts next to the binary. TABS_FONT overrides the path.
const fontFile = "JetBrainsMono-Regular.ttf"

proc fontPath(): string =
  result = getEnv("TABS_FONT", getAppDir() / "assets" / "fonts" / fontFile)
  if not fileExists(result):
    raise newException(IOError,
      &"font not found at {result}, set TABS_FONT to a .ttf/.otf file")

proc renderPng(t: Tablature, path: string) =
  ## Draws each section as a heading followed by a staff per part, wrapping
  ## long parts onto several staves.
  const
    margin   = 48'f32
    colW     = 24'f32
    rowH     = 20'f32
    labelW   = 28'f32
    headH    = 36'f32
    staffGap = 28'f32
    maxCols  = 40

  let strs   = t.strings
  let staffH = strs.len.float32 * rowH

  # Split every part into staff lines of at most maxCols columns. Parts of
  # a section share their width so their bars line up.
  var staves: seq[seq[seq[Slice[int]]]]
  for sec in t.sections:
    let cols = max(sec.lastIndex + 2, 8)
    var lines: seq[Slice[int]]
    var start = 0
    while start < cols:
      lines.add start .. min(start + maxCols, cols) - 1
      start += maxCols
    staves.add sec.parts.mapIt(lines)

  var height = margin * 2 + 88
  for parts in staves:
    height += headH
    for lines in parts:
      height += lines.len.float32 * (staffH + staffGap)
  let width = margin * 2 + labelW + (maxCols + 1).float32 * colW

  let image = newImage(width.int, height.int)
  image.fill(rgba(255, 255, 255, 255))
  let ctx      = newContext(image)
  let typeface = readTypeface(fontPath())
  let ink      = rgba(30, 30, 30, 255)
  let faint    = rgba(140, 140, 140, 255)

  proc font(size: float32, color = ink): Font =
    result       = newFont(typeface)
    result.size  = size
    result.paint = color

  proc put(s: string, x, y, size: float32, color = ink, center = false) =
    ## Draws s vertically centered on y, starting at x or centered on it.
    const box = 1000'f32
    let dx    = if center: box / 2 else: 0
    image.fillText(font(size, color), s, translate(vec2(x - dx, y - size)),
                   bounds = vec2(box, size * 2),
                   hAlign = (if center: CenterAlign else: LeftAlign),
                   vAlign = MiddleAlign)

  let tuning = t.header.tuning.mapIt(($it).toUpperAscii).join(" ")
  var y = margin
  put(t.header.song, margin, y + 16, 32, ink)
  put(&"{t.header.artist} · {t.header.instrument} · tuning {tuning}",
      margin, y + 54, 15, faint)
  y += 88

  let x0 = margin + labelW
  for i, sec in t.sections:
    put(sec.name, margin, y + headH / 2, 18, ink)
    y += headH

    for k, p in sec.parts:
      let lines = staves[i][k]
      for l, cols in lines:
        # One column of padding on both sides keeps notes off the bars.
        let x1 = x0 + (cols.len + 1).float32 * colW
        template lineY(r: int): float32 = y + r.float32 * rowH + rowH / 2

        ctx.lineWidth   = 1
        ctx.strokeStyle = faint
        for r, s in strs:
          put(($s).toUpperAscii, margin + labelW / 2, lineY(r), 14, ink, true)
          ctx.strokeSegment(x0, lineY(r), x1, lineY(r))

        if strs.len > 0:
          ctx.lineWidth   = 2
          ctx.strokeStyle = ink
          ctx.strokeSegment(x0, lineY(0), x0, lineY(strs.len - 1))
          ctx.strokeSegment(x1, lineY(0), x1, lineY(strs.len - 1))
          # Repeat count after the closing bar of the part's last staff.
          if l == lines.high and p.repeats > 1:
            put(&"x{p.repeats}", x1 + 8, (lineY(0) + lineY(strs.len - 1)) / 2,
                14, ink)

        for r in 0 ..< strs.len:
          for c in cols:
            let fret = p.fretAt(t.slot(r), c)
            if fret < 0: continue
            let cx = x0 + (c - cols.a + 1).float32 * colW
            # Blank the string behind the number so it stays readable.
            ctx.fillStyle = rgba(255, 255, 255, 255)
            let w = font(14).layoutBounds($fret).x + 6
            ctx.fillRect(cx - w / 2, lineY(r) - 8, w, 16)
            put($fret, cx, lineY(r), 14, ink, true)

        y += staffH + staffGap

  image.writeFile(path)

proc exportPng(ed: Editor) =
  ed.prompt("export png to: ", ed.path.changeFileExt("png"), proc(s: string) =
    var path = s.strip
    if path.len == 0: return
    if not path.toLowerAscii.endsWith(".png"): path &= ".png"
    try:
      ed.tab.renderPng(path)
      ed.status = &"exported {path}"
    except CatchableError as e:
      ed.status = &"export failed: {e.msg}")

# ---------------------------------------------------------------------------
# Input
# ---------------------------------------------------------------------------

template key(c: char): int = ord(c)

proc handlePrompt(ed: Editor, k: int) =
  case k
  of int(EVENT_KEY_ESC):
    ed.mode = modeNormal
  of int(EVENT_KEY_ENTER):
    ed.mode = modeNormal
    ed.onSubmit(ed.input)
  of int(EVENT_KEY_BACKSPACE), int(EVENT_KEY_BACKSPACE2):
    if ed.input.len > 0:
      ed.input = ed.input.toRunes[0 ..< ^1].`$`
  of int(EVENT_KEY_CTRL_U):
    ed.input = ""
  of int(EVENT_KEY_SPACE):
    ed.input.add ' '
  else:
    if k in 0x21 .. 0x7E or k > int(EVENT_KEY_F12):
      ed.input.add Rune(k).toUTF8

proc handleConfirm(ed: Editor, k: int) =
  ed.mode = modeNormal
  if k in [key('y'), key('Y')]:
    ed.onSubmit("y")

proc handleSections(ed: Editor, k: int) =
  case k
  of key('j'), int(EVENT_KEY_ARROW_DOWN):
    ed.selectSection(min(ed.section + 1, max(ed.tab.sections.len - 1, 0)))
  of key('k'), int(EVENT_KEY_ARROW_UP):
    ed.selectSection(max(ed.section - 1, 0))
  of key('J'): ed.moveSection(1)
  of key('K'): ed.moveSection(-1)
  of key('a'): ed.addSection()
  of key('r'): ed.renameSection()
  of key('d'): ed.deleteSection()
  of key('y'): ed.duplicateSection()
  of int(EVENT_KEY_ENTER), int(EVENT_KEY_TAB), key('l'),
     int(EVENT_KEY_ARROW_RIGHT):
    if ed.hasSection and ed.tab.strings.len > 0:
      ed.focus = focusGrid
    elif ed.tab.strings.len == 0:
      ed.status = "set a tuning first (t)"
  else: discard

proc handleGrid(ed: Editor, k: int) =
  let strs = ed.tab.strings
  if not ed.hasSection or strs.len == 0:
    ed.focus = focusSections
    return

  if k notin key('0') .. key('9'):
    ed.pending = ""

  # Part level keys, usable even when the section has no parts yet.
  case k
  of key('a'): ed.addPart(); return
  of int(EVENT_KEY_ESC), int(EVENT_KEY_TAB):
    ed.focus = focusSections
    return
  else: discard

  if not ed.hasPart: return
  let s = ed.tab.slot(ed.row)

  case k
  of key('h'), int(EVENT_KEY_ARROW_LEFT):  ed.col = max(ed.col - 1, 0)
  of key('l'), int(EVENT_KEY_ARROW_RIGHT): inc ed.col
  of key('k'), int(EVENT_KEY_ARROW_UP):
    # Moving past the top string continues into the part above.
    if ed.row > 0:
      dec ed.row
    elif ed.part > 0:
      dec ed.part
      ed.row = strs.len - 1
  of key('j'), int(EVENT_KEY_ARROW_DOWN):
    if ed.row < strs.len - 1:
      inc ed.row
    elif ed.part < ed.current.parts.len - 1:
      inc ed.part
      ed.row = 0
  of key('J'): ed.movePart(1)
  of key('K'): ed.movePart(-1)
  of key('y'): ed.duplicatePart()
  of key('X'): ed.deletePart()
  of key('n'): ed.setRepeats()
  of key('+'):
    inc ed.currentPart.repeats
    ed.dirty = true
  of key('-'):
    if ed.currentPart.repeats > 0:
      dec ed.currentPart.repeats
      ed.dirty = true
  of key('0') .. key('9'):
    # Up to two digits per cell, e.g. typing 1 then 2 gives fret 12.
    ed.pending = if ed.pending.len == 1: ed.pending & chr(k) else: $chr(k)
    ed.currentPart.setFret(s, ed.col, ed.pending.parseInt)
    ed.dirty = true
  of key('x'), int(EVENT_KEY_DELETE), int(EVENT_KEY_BACKSPACE),
     int(EVENT_KEY_BACKSPACE2):
    ed.currentPart.clearFret(s, ed.col)
    ed.dirty = true
  of key('i'):
    ed.currentPart.shiftColumns(ed.col, 1)
    ed.dirty = true
  of key('D'):
    ed.currentPart.deleteColumn(ed.col)
    ed.dirty = true
  of int(EVENT_KEY_HOME): ed.col = 0
  of int(EVENT_KEY_END):  ed.col = max(ed.currentPart.lastIndex, 0)
  else: discard

proc handleKey(ed: Editor, k: int) =
  ed.status = ""
  case ed.mode
  of modePrompt:  ed.handlePrompt(k); return
  of modeConfirm: ed.handleConfirm(k); return
  of modeNormal:  discard

  case k
  of int(EVENT_KEY_CTRL_S), key('s'): ed.save()
  of key('q'): ed.quit()
  of key('e'): ed.editHeader()
  of key('N'): ed.newFile()
  of key('o'): ed.openFile()
  of key('t'): ed.editTuning()
  of key('p'): ed.exportPng()
  else:
    case ed.focus
    of focusSections: ed.handleSections(k)
    of focusGrid:     ed.handleGrid(k)

# ---------------------------------------------------------------------------
# Rendering
# ---------------------------------------------------------------------------

const
  fg     = FG_COLOR_DEFAULT
  bg     = BG_COLOR_DEFAULT
  accent = FG_COLOR_CYAN
  dim    = FG_COLOR_BLACK_BRIGHT

proc drawHeader(ed: Editor, w: int) =
  let h      = ed.tab.header
  let tuning = h.tuning.mapIt(($it).toUpperAscii).join(" ")
  let mark   = if ed.dirty: " [+]" else: ""
  drawRectangle(0, 0, w, 1, BG_COLOR_BLUE, FG_COLOR_WHITE)
  drawText(&" {ed.path.extractFilename}{mark}", 0, 0,
           FG_COLOR_WHITE, BG_COLOR_BLUE)
  let info = &"{h.song} · {h.artist} · {h.instrument} · {tuning} "
  drawText(info, max(w - info.runeLen, 0), 0, FG_COLOR_WHITE, BG_COLOR_BLUE)

proc drawSections(ed: Editor, bottom: int) =
  let focused = ed.focus == focusSections
  drawBox(0, 1, listWidth, bottom, (if focused: accent else: dim), bg)
  drawText(" sections ", 2, 1, (if focused: accent else: dim), bg)

  if ed.tab.sections.len == 0:
    drawText("(empty, press a)", 2, 3, dim, bg)
    return

  for i, sec in ed.tab.sections:
    let y = 2 + i
    if y >= bottom - 1: break
    var line = &" {sec.name}"
    let count = &"{sec.parts.len}p "
    let room  = listWidth - 2 - count.len
    if line.runeLen > room: line = line.runeSubStr(0, room - 1) & "…"
    line = line.alignLeft(room) & count
    let (lfg, lbg) =
      if i != ed.section: (fg, bg)
      elif focused:       (FG_COLOR_BLACK, BG_COLOR_CYAN)
      else:               (FG_COLOR_BLACK, BG_COLOR_WHITE)
    drawText(line, 1, y, lfg, lbg)

proc drawGrid(ed: Editor, w, bottom: int) =
  ## Draws every part of the current section as a staff, stacked vertically
  ## and sharing the same columns.
  let focused = ed.focus == focusGrid
  let x0 = listWidth
  drawBox(x0, 1, w, bottom, (if focused: accent else: dim), bg)

  if not ed.hasSection:
    drawText("no section selected", x0 + 2, 3, dim, bg)
    return

  let sec = ed.current
  drawText(&" {sec.name} ", x0 + 2, 1, (if focused: accent else: dim), bg)

  let strs = ed.tab.strings
  if strs.len == 0:
    drawText("no tuning set, press t", x0 + 2, 3, dim, bg)
    return
  if sec.parts.len == 0:
    let hint = if focused: "press a" else: "press enter then a"
    drawText(&"no parts, {hint}", x0 + 2, 3, dim, bg)
    return

  # Horizontal scrolling so the cursor is always visible.
  let gridX   = x0 + 5
  let visible = max((w - gridX - 2) div cellWidth, 1)
  if ed.col < ed.scroll: ed.scroll = ed.col
  if ed.col >= ed.scroll + visible: ed.scroll = ed.col - visible + 1
  let cols = max(max(sec.lastIndex, ed.col) + 1 + 4, defaultCols)
  let last = min(ed.scroll + visible, cols)

  # Vertical scrolling by whole parts, each one is a label line, a line per
  # string and a blank line (which the last visible part can do without).
  let top    = 4
  let limit  = bottom - 3
  let blockH = strs.len + 2
  let fit    = max((limit - top + 2) div blockH, 1)
  if ed.part < ed.vscroll: ed.vscroll = ed.part
  if ed.part >= ed.vscroll + fit: ed.vscroll = ed.part - fit + 1
  ed.vscroll = clamp(ed.vscroll, 0, max(sec.parts.len - fit, 0))

  # Column ruler.
  for c in ed.scroll ..< last:
    if c mod 4 == 0:
      drawText($c, gridX + (c - ed.scroll) * cellWidth, 3, dim, bg)

  var y = top
  for k in ed.vscroll ..< sec.parts.len:
    if y > limit: break
    let p       = sec.parts[k]
    let current = k == ed.part
    let more    =
      if k == ed.vscroll and k > 0: "  ↑"
      elif k == ed.vscroll + fit - 1 and k < sec.parts.len - 1: "  ↓"
      else: ""
    drawText(&"part {k + 1}/{sec.parts.len}  x{p.repeats}{more}", x0 + 2, y,
             (if current and focused: accent else: dim), bg)
    inc y

    for r, s in strs:
      if y > limit: break
      drawText(&"{($s).toUpperAscii} |", x0 + 2, y, accent, bg)
      for c in ed.scroll ..< last:
        let fret = p.fretAt(ed.tab.slot(r), c)
        let cell = if fret < 0: "---" else: ($fret).alignLeft(cellWidth, '-')
        let x    = gridX + (c - ed.scroll) * cellWidth
        let onCursor = focused and current and r == ed.row and c == ed.col
        let (cfg, cbg) =
          if onCursor:   (FG_COLOR_BLACK, BG_COLOR_CYAN)
          elif fret < 0: (dim, bg)
          else:          (FG_COLOR_YELLOW_BRIGHT, bg)
        drawText(cell, x, y, cfg, cbg)
      drawText("|", gridX + (last - ed.scroll) * cellWidth, y, accent, bg)
      inc y
    inc y

  drawText(&"part {ed.part + 1}  string {($strs[ed.row]).toUpperAscii}  " &
           &"col {ed.col}", x0 + 2, bottom - 2, dim, bg)

proc helpItems(ed: Editor): seq[string] =
  case ed.mode
  of modePrompt:  @["enter confirm", "ctrl-u clear", "esc cancel"]
  of modeConfirm: @["y confirm", "any key cancel"]
  of modeNormal:
    case ed.focus
    of focusSections:
      @["j/k move", "a add", "r rename", "d delete", "y dup", "J/K reorder",
        "enter edit", "e header", "t tuning", "N new file", "o open",
        "p export png", "s save", "q quit"]
    of focusGrid:
      @["hjkl move", "0-9 fret", "x clear", "i insert col", "D delete col",
        "a add part", "y dup part", "X delete part", "J/K reorder parts",
        "+/-/n repeats", "esc back", "p export png", "s save", "q quit"]

proc wrapItems(items: seq[string], width: int): seq[string] =
  ## Packs items into as few lines as fit the width.
  const sep = " · "
  var line = ""
  for item in items:
    if line.len == 0:
      line = item
    elif line.runeLen + sep.runeLen + item.runeLen <= width:
      line &= sep & item
    else:
      result.add line
      line = item
  if line.len > 0: result.add line

proc drawFooter(ed: Editor, help: seq[string], top: int) =
  case ed.mode
  of modePrompt:
    drawText(ed.label & ed.input & "_", 1, top, FG_COLOR_WHITE, bg)
  of modeConfirm:
    drawText(ed.label, 1, top, FG_COLOR_YELLOW, bg)
  of modeNormal:
    drawText(ed.status, 1, top, FG_COLOR_GREEN, bg)

  for i, line in help:
    drawText(line, 1, top + 1 + i, dim, bg)

proc draw(ed: Editor) =
  let w      = getTerminalWidth()
  let h      = getTerminalHeight()
  let help   = ed.helpItems.wrapItems(w - 2)
  let bottom = h - 1 - help.len
  removeArea(0, 0, w, h)
  ed.drawHeader(w)
  ed.drawSections(bottom)
  ed.drawGrid(w, bottom)
  ed.drawFooter(help, bottom)

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

proc newEditor(path: string): Editor =
  result = Editor(running: true)
  if fileExists(path): result.loadFile(path)
  else:                result.startNew(path)

proc runEditor*(path: string) =
  let ed = newEditor(path)

  proc onExit() {.noconv.} =
    deinitTextalot()
    quit(0)
  setControlCHook(onExit)

  initTextalot()
  try:
    while ed.running:
      updateTextalot()
      if texalotEvent of KeyEvent:
        ed.handleKey(int(KeyEvent(texalotEvent).key))
      ed.draw()
      sleep(10)
  finally:
    deinitTextalot()

when isMainModule:
  try:
    if paramCount() >= 2 and paramStr(1) == "--png":
      # Headless export: tui --png file.txt [out.png]
      let ed  = Editor()
      ed.loadFile(paramStr(2))
      let png = if paramCount() >= 3: paramStr(3)
                else: paramStr(2).changeFileExt("png")
      ed.tab.renderPng(png)
      echo &"exported {png}"
    else:
      runEditor(if paramCount() >= 1: paramStr(1) else: "tab.txt")
  except CatchableError as e:
    stderr.writeLine &"error: {e.msg}"
    quit(1)
