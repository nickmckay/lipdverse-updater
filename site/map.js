// The map.
//
// Self-hosted geography, no tile server and no map library. Natural Earth
// countries, decoded from TopoJSON to GeoJSON at build time and rounded to 2
// decimal places (~1 km, far finer than a world map needs) so the browser
// needs no decoder either -- 166 KB for the whole world.
//
// A site whose URLs are cited in papers should not stop drawing its map
// because a third-party tile host changed its terms or went away.

const W = 1000, H = 500          // equirectangular, 2:1
const GEO = '/dev/site/data/countries-110m.geojson'
const SVG_NS = 'http://www.w3.org/2000/svg'

const project = ([lon, lat]) => [(lon + 180) / 360 * W, (90 - lat) / 180 * H]

// GeoJSON ring -> SVG path, with the antimeridian handled.
//
// A ring that crosses 180 has consecutive points at +179 and -179, which
// project to opposite edges of the map: drawn naively that is a horizontal
// line straight across the world. Russia, Fiji and Antarctica all do it.
//
// The fix is to unwrap first -- walk the ring accumulating a +/-360 offset
// whenever a step jumps more than half the globe, so the ring stays continuous
// in a coordinate space that may now run outside [-180, 180] -- and then draw
// the ring up to three times, shifted by -360, 0 and +360. Whichever copies
// fall inside the viewBox are the ones you see; the rest are clipped. That is
// also what makes a country straddling the seam appear on both edges, which is
// correct rather than a duplicate.
function unwrap (ring) {
  const out = []
  let offset = 0
  for (let i = 0; i < ring.length; i++) {
    const [lon, lat] = ring[i]
    if (i > 0) {
      const d = lon - ring[i - 1][0]
      if (d > 180) offset -= 360
      else if (d < -180) offset += 360
    }
    out.push([lon + offset, lat])
  }
  return out
}

function ringPaths (ring) {
  const pts = unwrap(ring)
  let lo = Infinity, hi = -Infinity
  for (const [lon] of pts) { if (lon < lo) lo = lon; if (lon > hi) hi = lon }
  const paths = []
  for (const shift of [-360, 0, 360]) {
    // Skip a copy that cannot appear in the -180..180 window at all.
    if (lo + shift > 180 || hi + shift < -180) continue
    let d = ''
    for (let i = 0; i < pts.length; i++) {
      const [x, y] = project([pts[i][0] + shift, pts[i][1]])
      d += (i ? 'L' : 'M') + x.toFixed(1) + ',' + y.toFixed(1)
    }
    paths.push(d + 'Z')
  }
  return paths.join('')
}

function featurePath (f) {
  const g = f.geometry
  if (!g) return ''
  if (g.type === 'Polygon') return g.coordinates.map(ringPaths).join('')
  if (g.type === 'MultiPolygon') return g.coordinates.map(p => p.map(ringPaths).join('')).join('')
  return ''
}

const geoCache = new Map()
async function geo (url) {
  if (!geoCache.has(url)) geoCache.set(url, fetch(url).then(r => r.json()))
  return geoCache.get(url)
}

export function createMap (mount, opts = {}) {
  const svg = document.createElementNS(SVG_NS, 'svg')
  svg.setAttribute('viewBox', `0 0 ${W} ${H}`)
  svg.setAttribute('id', 'map')
  svg.setAttribute('role', 'img')
  svg.setAttribute('aria-label', 'Dataset locations')

  const ocean = document.createElementNS(SVG_NS, 'rect')
  ocean.setAttribute('width', W); ocean.setAttribute('height', H)
  ocean.setAttribute('class', 'ocean')

  const gGraticule = document.createElementNS(SVG_NS, 'path')
  gGraticule.setAttribute('class', 'graticule')
  {
    let d = ''
    for (let lon = -180; lon <= 180; lon += 30) { const [x] = project([lon, 0]); d += `M${x},0V${H}` }
    for (let lat = -60; lat <= 60; lat += 30) { const [, y] = project([0, lat]); d += `M0,${y}H${W}` }
    gGraticule.setAttribute('d', d)
  }

  const gLand = document.createElementNS(SVG_NS, 'g')
  gLand.setAttribute('class', 'land')
  const gPts = document.createElementNS(SVG_NS, 'g')
  gPts.setAttribute('class', 'points')

  svg.append(ocean, gLand, gGraticule, gPts)

  const wrap = document.createElement('div')
  wrap.className = 'mapwrap'
  const hint = document.createElement('div')
  hint.className = 'maphint'
  hint.textContent = 'scroll to zoom · drag to pan · double-click to reset'
  const reset = document.createElement('button')
  reset.className = 'chip mapreset'; reset.textContent = 'Reset'
  wrap.append(svg, hint, reset)
  mount.replaceChildren(wrap)

  // ---- view state ----------------------------------------------------------
  const view = { x: 0, y: 0, w: W, h: H }

  const applyView = () => {
    svg.setAttribute('viewBox', `${view.x} ${view.y} ${view.w} ${view.h}`)
    // Keep points and strokes the same apparent size however far we zoom.
    const k = view.w / W
    svg.style.setProperty('--k', k)
    opts.onView?.({ ...view })
  }

  const clamp = () => {
    view.w = Math.min(W, Math.max(W / 60, view.w))
    view.h = view.w / 2
    view.x = Math.min(W - view.w, Math.max(0, view.x))
    view.y = Math.min(H - view.h, Math.max(0, view.y))
  }

  const toSvg = (ev) => {
    const r = svg.getBoundingClientRect()
    return [view.x + (ev.clientX - r.left) / r.width * view.w,
            view.y + (ev.clientY - r.top) / r.height * view.h]
  }

  svg.addEventListener('wheel', (ev) => {
    ev.preventDefault()
    const [cx, cy] = toSvg(ev)
    const factor = Math.exp(ev.deltaY * 0.0015)
    const w0 = view.w
    view.w *= factor
    clamp()
    // Zoom about the cursor rather than the corner.
    const s = view.w / w0
    view.x = cx - (cx - view.x) * s
    view.y = cy - (cy - view.y) * s
    clamp(); applyView()
  }, { passive: false })

  let drag = null
  svg.addEventListener('pointerdown', (ev) => {
    if (ev.target.closest('a')) return
    drag = { x: ev.clientX, y: ev.clientY, vx: view.x, vy: view.y, moved: false }
    svg.setPointerCapture(ev.pointerId); svg.classList.add('grabbing')
  })
  svg.addEventListener('pointermove', (ev) => {
    if (!drag) return
    const r = svg.getBoundingClientRect()
    const dx = (ev.clientX - drag.x) / r.width * view.w
    const dy = (ev.clientY - drag.y) / r.height * view.h
    if (Math.abs(dx) > 1 || Math.abs(dy) > 1) drag.moved = true
    view.x = drag.vx - dx; view.y = drag.vy - dy
    clamp(); applyView()
  })
  const endDrag = (ev) => {
    if (!drag) return
    // A drag that moved should not also count as a click on a point.
    if (drag.moved) svg.classList.add('dragged')
    else svg.classList.remove('dragged')
    drag = null; svg.classList.remove('grabbing')
    try { svg.releasePointerCapture(ev.pointerId) } catch {}
  }
  svg.addEventListener('pointerup', endDrag)
  svg.addEventListener('pointercancel', endDrag)

  const resetView = () => {
    view.x = 0; view.y = 0; view.w = W; view.h = H; applyView()
  }
  svg.addEventListener('dblclick', resetView)
  reset.onclick = resetView

  async function drawLand (url) {
    const g = await geo(url)
    const frag = document.createDocumentFragment()
    for (const f of g.features) {
      const d = featurePath(f)
      if (!d) continue
      const p = document.createElementNS(SVG_NS, 'path')
      p.setAttribute('d', d)
      if (f.properties?.name) {
        const t = document.createElementNS(SVG_NS, 'title')
        t.textContent = f.properties.name
        p.append(t)
      }
      frag.append(p)
    }
    gLand.replaceChildren(frag)
  }

  let highlight = opts.highlight || null

  function setPoints (rows) {
    const frag = document.createDocumentFragment()
    for (const r of rows) {
      const lat = Number(r.geo_latitude), lon = Number(r.geo_longitude)
      if (!Number.isFinite(lat) || !Number.isFinite(lon)) continue
      if (Math.abs(lat) > 90 || Math.abs(lon) > 180) continue
      const [x, y] = project([lon, lat])
      const a = document.createElementNS(SVG_NS, 'a')
      const v = String(r.version || '').replace(/\./g, '_')
      a.setAttribute('href', `/dev/data/${encodeURIComponent(r.datasetId)}/${encodeURIComponent(v)}/`)
      const c = document.createElementNS(SVG_NS, 'circle')
      c.setAttribute('cx', x.toFixed(1)); c.setAttribute('cy', y.toFixed(1))
      c.setAttribute('r', 2.6)
      const t = document.createElementNS(SVG_NS, 'title')
      t.textContent = `${r.dataSetName}${r.archiveType ? ' · ' + r.archiveType : ''}`
      c.append(t); a.append(c); frag.append(a)
    }
    gPts.replaceChildren(frag)
  }

  // Centre on one point at a given span, for a dataset arrived at cold -- with
  // no context from a collection page to inherit.
  function focus (lon, lat, span = 60) {
    const [x, y] = project([lon, lat])
    view.w = Math.max(W / 60, Math.min(W, span / 360 * W))
    view.h = view.w / 2
    view.x = x - view.w / 2
    view.y = y - view.h / 2
    clamp(); applyView()
  }

  const setView = (v) => {
    if (!v) return
    view.x = v.x; view.y = v.y; view.w = v.w; view.h = v.h
    clamp(); applyView()
  }

  drawLand(GEO)
  applyView()
  return { setPoints, reset: resetView, focus, setView,
           getView: () => ({ ...view }),
           setHighlight: (id) => { highlight = id } }
}
