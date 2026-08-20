// Advanced search.
//
// The basic box searches what a dataset row already carries -- name, site,
// archive. Everything else people actually want to search by lives one level
// down, on the columns: proxy, variable, units, and the interpretations. So the
// advanced panel pulls timeseries and interpretations once, on first open, and
// builds a per-dataset index of the vocabulary its columns use.
//
// Cardinalities as of the 2026-08-18 export, which is what decides each
// control: archiveType 17, proxyGeneral 35, interpretation scope 7, direction
// 28, units 88, interpretation variable 118, seasonality 141, variableName 497,
// proxy 643. Small ones are plain checkbox lists; the long ones get a filter
// box. `standardName` is not offered because it is empty for every row in the
// database.
//
// Semantics: OR within a facet, AND across facets. That is the combination
// people expect from faceted search, and the only one where adding a tick can
// never widen the result set in a way that looks like a bug.

const FACETS = [
  { key: 'archiveType', label: 'Archive type', level: 'dataset' },
  { key: 'proxyGeneral', label: 'Proxy (general)', level: 'column' },
  { key: 'proxy', label: 'Proxy', level: 'column', long: true },
  { key: 'variableName', label: 'Variable', level: 'column', long: true },
  { key: 'units', label: 'Units', level: 'column', long: true },
  { key: 'interpVariable', label: 'Interpretation', level: 'interp' },
  { key: 'interpScope', label: 'Interpretation scope', level: 'interp' },
  { key: 'interpSeasonality', label: 'Seasonality', level: 'interp', long: true },
  { key: 'interpDirection', label: 'Direction', level: 'interp' }
]

const txt = v => (v === null || v === undefined) ? '' : String(v).trim()
const numOf = v => {
  if (v === null || v === undefined) return null
  const n = Number(v)
  return Number.isFinite(n) ? n : null
}

/**
 * Per-dataset sets of every vocabulary term its columns use, plus the numeric
 * spread needed by the range controls.
 */
export function buildIndex (datasets, ts, interps) {
  const byId = new Map()
  const get = (id) => {
    let e = byId.get(id)
    if (!e) {
      e = { archiveType: new Set(), proxyGeneral: new Set(), proxy: new Set(),
            variableName: new Set(), units: new Set(), interpVariable: new Set(),
            interpScope: new Set(), interpSeasonality: new Set(), interpDirection: new Set(),
            res: [], minYear: null, maxYear: null }
      byId.set(id, e)
    }
    return e
  }

  for (const d of datasets) {
    const e = get(d.datasetId)
    if (txt(d.archiveType)) e.archiveType.add(txt(d.archiveType))
    e.minYear = numOf(d.minYear)
    e.maxYear = numOf(d.maxYear)
  }

  const tsById = new Map()
  for (const t of ts) {
    if (t.tableType !== 'paleo' || t.tableKind !== 'measurement') continue
    const e = get(t.datasetId)
    for (const k of ['proxyGeneral', 'proxy', 'variableName', 'units']) {
      const v = txt(t[k])
      if (v) e[k].add(v)
    }
    const r = numOf(t.medianResolution)
    if (r !== null && r > 0) e.res.push(r)
    tsById.set(t.TSid, t.datasetId)
  }

  for (const i of interps) {
    const dsid = tsById.get(i.TSid)
    if (!dsid) continue
    const e = get(dsid)
    const map = { interpVariable: 'variable', interpScope: 'scope',
                  interpSeasonality: 'seasonality', interpDirection: 'direction' }
    for (const [k, src] of Object.entries(map)) {
      const v = txt(i[src])
      if (v) e[k].add(v)
    }
  }

  for (const e of byId.values()) {
    e.resMin = e.res.length ? Math.min(...e.res) : null
    e.resMax = e.res.length ? Math.max(...e.res) : null
    delete e.res
  }
  return byId
}

/** Option lists with counts, computed over whatever rows are in scope. */
export function facetOptions (rows, index) {
  const out = new Map()
  for (const f of FACETS) out.set(f.key, new Map())
  for (const r of rows) {
    const e = index.get(r.datasetId)
    if (!e) continue
    for (const f of FACETS) {
      const c = out.get(f.key)
      for (const v of e[f.key]) c.set(v, (c.get(v) || 0) + 1)
    }
  }
  return out
}

/** Does one dataset satisfy the current selection? */
export function matches (row, index, sel) {
  const e = index.get(row.datasetId)
  if (!e) return false
  for (const f of FACETS) {
    const want = sel.facets[f.key]
    if (!want || !want.size) continue
    let hit = false
    for (const v of want) if (e[f.key].has(v)) { hit = true; break }
    if (!hit) return false
  }
  // Coverage overlap, not containment: a record that spans part of the window
  // is the thing people are looking for.
  if (sel.from !== null || sel.to !== null) {
    if (e.minYear === null || e.maxYear === null) return false
    if (sel.to !== null && e.minYear > sel.to) return false
    if (sel.from !== null && e.maxYear < sel.from) return false
  }
  if (sel.resMax !== null) {
    if (e.resMin === null || e.resMin > sel.resMax) return false
  }
  return true
}

export function emptySelection () {
  const facets = {}
  for (const f of FACETS) facets[f.key] = new Set()
  return { facets, from: null, to: null, resMax: null }
}

export function selectionCount (sel) {
  let n = 0
  for (const f of FACETS) n += sel.facets[f.key].size
  if (sel.from !== null || sel.to !== null) n++
  if (sel.resMax !== null) n++
  return n
}

export { FACETS }
