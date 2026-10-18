import pixie 

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

const pageParts = 5 ## parts per image, longer tabs span several files

type PageEntry = tuple[sec: int, parts: Slice[int]]

proc paginate(t: Tablature): seq[seq[PageEntry]] =
  ## Groups the parts of all sections into pages of at most pageParts.
  ## Sections without parts stay with the page their heading falls on.
  result = @[newSeq[PageEntry]()]
  var used = 0
  for i, sec in t.sections:
    if sec.parts.len == 0:
      result[^1].add (i, 0 .. -1)
      continue
    var start = 0
    while start < sec.parts.len:
      if used == pageParts:
        result.add newSeq[PageEntry]()
        used = 0
      let stop = min(start + pageParts - used, sec.parts.len)
      result[^1].add (i, start .. stop - 1)
      used  += stop - start
      start  = stop

proc pagePath(path: string, page, pages: int): string =
  if pages == 1: path
  else: path.changeFileExt("") & &"-{page + 1}.png"

proc renderPng(t: Tablature, page: seq[PageEntry], label, path: string) =
  ## Draws each section as a heading followed by a staff per part, wrapping
  ## long parts onto several staves. Only the parts listed in page are drawn.
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
  for e in page:
    let sec  = t.sections[e.sec]
    let cols = max(sec.lastIndex + 2, 8)
    var lines: seq[Slice[int]]
    var start = 0
    while start < cols:
      lines.add start .. min(start + maxCols, cols) - 1
      start += maxCols
    staves.add sec.parts[e.parts].mapIt(lines)

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
  put(&"{t.header.artist} · {t.header.instrument} · tuning {tuning}{label}",
      margin, y + 54, 15, faint)
  y += 88

  let x0 = margin + labelW
  for i, e in page:
    let sec = t.sections[e.sec]
    put(sec.name, margin, y + headH / 2, 18, ink)
    y += headH

    for k, p in sec.parts[e.parts]:
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

proc exportPngs(t: Tablature, path: string): seq[string] =
  ## Renders t to path, or to path-1.png, path-2.png, ... when it has more
  ## than pageParts parts. Returns the written files.
  let pages = t.paginate
  for i, page in pages:
    let label = if pages.len > 1: &" · {i + 1}/{pages.len}" else: ""
    result.add pagePath(path, i, pages.len)
    t.renderPng(page, label, result[^1])

proc describe(files: seq[string]): string =
  if files.len == 1: files[0]
  else: &"{files[0]} .. {files[^1].extractFilename}"

proc exportPng*(ed: Editor) =
  ## Exports to the directory the tool was started from by default.
  let initial = getCurrentDir() / ed.path.extractFilename.changeFileExt("png")
  ed.prompt("export png to: ", initial, proc(s: string) =
    var path = s.strip
    if path.len == 0: return
    if not path.toLowerAscii.endsWith(".png"): path &= ".png"
    try:
      ed.status = &"exported {ed.tab.exportPngs(path).describe}"
    except CatchableError as e:
      ed.status = &"export failed: {e.msg}")


