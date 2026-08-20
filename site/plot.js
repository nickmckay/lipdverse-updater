// Timeseries plots.
//
// Drawn as inline SVG for the same reason the map is: no plotting library, no
// third-party host, and a page that keeps working on its own.
//
// The data comes out of values.parquet, which is 205 MB and holds all 48.6M
// values in the database. Fetching that to draw one record would be absurd, so
// the export's layout is exploited instead: every dataset's rows are
// contiguous, and so are every column's within it. A 340 KB index maps
// datasetId to its [start, end) row range, and hyparquet reads only that slice.

const SVG_NS = 'http://www.w3.org/2000/svg'
const num = v => {
  if (v === null || v === undefined) return null
  const n = Number(v)
  return Number.isFinite(n) ? n : null
}

// Downsample for drawing only. Above a few thousand points an SVG path costs
// more than it shows, so keep the extremes of each bucket: that preserves the
// visual envelope, which is what a reader is actually looking at.
function decimate (xs, ys, target = 3000) {
  const n = xs.length
  if (n <= target) return [xs, ys]
  const step = Math.ceil(n / (target / 2))
  const ox = [], oy = []
  for (let i = 0; i < n; i += step) {
    let lo = i, hi = i
    for (let j = i; j < Math.min(i + step, n); j++) {
      if (ys[j] < ys[lo]) lo = j
      if (ys[j] > ys[hi]) hi = j
    }
    const [a, b] = lo <= hi ? [lo, hi] : [hi, lo]
    ox.push(xs[a], xs[b]); oy.push(ys[a], ys[b])
  }
  return [ox, oy]
}

// Ticks are placed INSIDE the data's own range rather than the range being
// rounded outward to meet them. Rounding the domain is what left a record
// covering 1560-2005 drawn across an axis of 1500-2100: a quarter of the panel
// blank, and the shape of the series smaller for no reason.
const tickValues = (lo, hi, n) => {
  if (!(hi > lo)) return [lo]
  const raw = (hi - lo) / n
  const mag = Math.pow(10, Math.floor(Math.log10(raw)))
  const norm = raw / mag
  const step = (norm <= 1 ? 1 : norm <= 2 ? 2 : norm <= 2.5 ? 2.5 : norm <= 5 ? 5 : 10) * mag
  const out = []
  for (let t = Math.ceil(lo / step - 1e-9) * step; t <= hi + 1e-9; t += step) {
    out.push(Math.abs(t) < step * 1e-9 ? 0 : t)
  }
  return out
}

// A little breathing room so the extremes are not drawn on the frame itself,
// but proportional and small -- not a rounded-out axis.
const padded = (lo, hi, frac) => {
  if (!(hi > lo)) { const d = Math.abs(lo || 1) * 0.05; return [lo - d, hi + d] }
  const p = (hi - lo) * frac
  return [lo - p, hi + p]
}

const fmt = v => {
  const a = Math.abs(v)
  if (a >= 1e5 || (a > 0 && a < 1e-3)) return v.toExponential(1)
  return String(Math.round(v * 1000) / 1000)
}

/**
 * One chart: value against year.
 * @param {number[]} year  x values, years AD (negative = BCE)
 * @param {number[]} value y values
 */
export function plot (year, value, opts = {}) {
  const W = 900, H = 220, ML = 54, MR = 10, MT = 10, MB = 28

  const pairs = []
  for (let i = 0; i < year.length; i++) {
    const x = num(year[i]), y = num(value[i])
    if (x !== null && y !== null) pairs.push([x, y])
  }
  if (pairs.length < 2) {
    const p = document.createElement('p')
    p.className = 'muted'
    p.textContent = pairs.length ? 'Only one point with both a year and a value.' : 'No plottable points.'
    return p
  }
  pairs.sort((a, b) => a[0] - b[0])
  let xs = pairs.map(p => p[0]), ys = pairs.map(p => p[1])
  const shown = xs.length
  ;[xs, ys] = decimate(xs, ys)

  const [x0, x1] = padded(Math.min(...xs), Math.max(...xs), 0.012)
  const [y0, y1] = padded(Math.min(...ys), Math.max(...ys), 0.045)
  const sx = v => ML + (v - x0) / (x1 - x0) * (W - ML - MR)
  const sy = v => H - MB - (v - y0) / (y1 - y0) * (H - MT - MB)

  const svg = document.createElementNS(SVG_NS, 'svg')
  svg.setAttribute('viewBox', `0 0 ${W} ${H}`)
  svg.setAttribute('class', 'chart')
  svg.setAttribute('role', 'img')
  svg.setAttribute('aria-label', opts.label || 'timeseries')

  const add = (tag, attrs, text) => {
    const n = document.createElementNS(SVG_NS, tag)
    for (const [k, v] of Object.entries(attrs)) n.setAttribute(k, v)
    if (text !== undefined) n.textContent = text
    svg.append(n)
    return n
  }

  for (const t of tickValues(y0, y1, 4)) {
    add('line', { x1: ML, x2: W - MR, y1: sy(t), y2: sy(t), class: 'grid' })
    add('text', { x: ML - 8, y: sy(t) + 4, class: 'tick', 'text-anchor': 'end' }, fmt(t))
  }
  for (const t of tickValues(x0, x1, 6)) {
    add('line', { x1: sx(t), x2: sx(t), y1: MT, y2: H - MB, class: 'grid' })
    add('text', { x: sx(t), y: H - MB + 17, class: 'tick', 'text-anchor': 'middle' },
      t < 0 ? `${fmt(-t)} BCE` : fmt(t))
  }
  add('line', { x1: ML, x2: W - MR, y1: H - MB, y2: H - MB, class: 'axis' })
  add('line', { x1: ML, x2: ML, y1: MT, y2: H - MB, class: 'axis' })

  // the series
  let d = ''
  for (let i = 0; i < xs.length; i++) d += (i ? 'L' : 'M') + sx(xs[i]).toFixed(1) + ',' + sy(ys[i]).toFixed(1)
  add('path', { d, class: 'series' })
  // Points, but only when there are few enough for them to mean anything.
  if (xs.length <= 400) {
    for (let i = 0; i < xs.length; i++) {
      add('circle', { cx: sx(xs[i]).toFixed(1), cy: sy(ys[i]).toFixed(1), r: 1.9, class: 'dot' })
    }
  }
  if (opts.ylabel) {
    add('text', { x: 11, y: (MT + H - MB) / 2, class: 'axlabel',
                  transform: `rotate(-90 11 ${(MT + H - MB) / 2})`, 'text-anchor': 'middle' }, opts.ylabel)
  }

  const wrap = document.createElement('figure')
  wrap.className = 'chartwrap'
  wrap.append(svg)
  const cap = document.createElement('figcaption')
  cap.className = 'muted'
  cap.textContent = `${shown.toLocaleString()} points` +
    (shown > xs.length / 2 && shown > 3000 ? ' (drawn decimated)' : '')
  wrap.append(cap)
  return wrap
}


// The same rule as lv_axis_kind() in the pipeline, and for the same reason:
// units decide, the name is only the fallback. MaeHongSon.Buckley.2007 names
// its axis `age` and labels it `yr AD` over 1560-2005; deciding by name here
// would have plotted it as -56 to 390, reintroducing on the page the exact bug
// the export was fixed for.
function axisKind (units, name) {
  const tok = String(units || '').toLowerCase().replace(/[^a-z0-9]+/g, ' ').trim().split(/\s+/)
  const has = (...w) => w.some(x => tok.includes(x))
  if (has('ka', 'kyr', 'kyrs', 'myr', 'ma', '14c', 'c14')) return null
  if (has('bp', 'b2k', 'bce')) return 'age'
  if (has('ad', 'ce')) return 'year'
  return /^year/i.test(String(name || '')) ? 'year' : 'age'
}

/**
 * Pair each measurement column with its table's axis and draw it.
 *
 * The export does not record which table a column came from -- only its type
 * and kind -- so an axis is matched to a measurement by having the same value
 * count within the same block. That is right whenever a dataset has one
 * measurement table per block, which is the overwhelming majority, and is the
 * reason to add a table id to the export later.
 */
export function pairAxis (cols, byTsid) {
  const axes = cols.filter(c => c.isAxis)
  const out = []
  for (const c of cols) {
    if (c.isAxis) continue
    const vals = byTsid.get(c.TSid)
    if (!vals) continue
    const ax = axes.find(a => a.n_values === c.n_values && byTsid.has(a.TSid))
    if (!ax) continue
    const axVals = byTsid.get(ax.TSid)
    const kind = axisKind(ax.units, ax.variableName)
    if (kind === null) continue        // ka, radiocarbon: not a calendar axis
    const x = axVals.map(v => {
      const n = num(v)
      if (n === null) return null
      if (kind === 'year') return n
      const y = 1950 - n               // no year zero: 1 BCE runs into 1 CE
      return y <= 0 ? y - 1 : y
    })
    out.push({ col: c, axis: ax, x, y: vals.map(num) })
  }
  return out
}
