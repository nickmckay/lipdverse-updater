// LiPDverse front end.
//
// Everything here reads the export directly: parquet over HTTP, byte ranges,
// no API and no server process. Apache sends Accept-Ranges, so hyparquet reads
// a file's footer and then only the column chunks it needs -- a 210,723-row
// count came back in 0.1s over the network during testing.
//
// Routing is done by .htaccess: a real file or directory always wins, so all
// 41,496 legacy pages keep serving themselves and this app only ever sees URLs
// that would otherwise 404. That is why there is no cutover.

import { asyncBufferFromUrl, parquetReadObjects } from '/dev/site/lib/hyparquet/index.js'
import { createMap } from '/dev/site/map.js'
import { plot, pairAxis } from '/dev/site/plot.js'
import { buildIndex, facetOptions, matches, emptySelection, selectionCount, FACETS }
  from '/dev/site/search.js'

const BASE = '/dev'
const EXPORT_INDEX = `${BASE}/export/current.json`

const $ = (s, r = document) => r.querySelector(s)
const el = (tag, attrs = {}, ...kids) => {
  const n = document.createElement(tag)
  for (const [k, v] of Object.entries(attrs)) {
    if (k === 'class') n.className = v
    else if (k === 'html') n.innerHTML = v
    else if (v !== null && v !== undefined) n.setAttribute(k, v)
  }
  for (const c of kids.flat()) if (c !== null && c !== undefined) n.append(c)
  return n
}
const txt = v => (v === null || v === undefined || v === '') ? '' : String(v)

// Parquet gives int64 as BigInt and R writes some numerics as doubles; the UI
// wants plain numbers or nothing, never "NaN" or "null" rendered as text.
const num = v => {
  if (v === null || v === undefined) return null
  const n = typeof v === 'bigint' ? Number(v) : Number(v)
  return Number.isFinite(n) ? n : null
}
const fmtYear = v => {
  const n = num(v)
  if (n === null) return ''
  const r = Math.round(n * 10) / 10
  return r < 0 ? `${Math.abs(r)} BCE` : `${r} CE`
}

let EXPORT_DIR = null
let MANIFEST = null
const cache = new Map()

async function loadParquet (name, columns) {
  const key = `${name}|${(columns || []).join(',')}`
  if (cache.has(key)) return cache.get(key)
  const p = (async () => {
    const url = `${EXPORT_DIR}/${name}.parquet`
    const file = await asyncBufferFromUrl({ url })
    return parquetReadObjects({ file, columns })
  })()
  cache.set(key, p)
  return p
}

async function boot () {
  // Which export to read is a served fact, not a constant compiled in here, so
  // a new export is published by writing one small file rather than by
  // redeploying the site.
  const idx = await fetch(EXPORT_INDEX).then(r => r.json())
  EXPORT_DIR = `${BASE}/export/${idx.dir}`
  MANIFEST = await fetch(`${EXPORT_DIR}/export_manifest.json`).then(r => r.json())
  renderFooter()
  route()
}

function renderFooter () {
  const m = MANIFEST
  $('#footer').replaceChildren(
    el('div', {},
      `Export ${m.version} · ${m.n_datasets.toLocaleString()} datasets · schema v${m.schema_version} · generated ${String(m.generated_at).slice(0, 10)}`),
    el('div', { class: 'muted' },
      el('code', {}, `db ${String(m.db_fingerprint).slice(0, 12)} · vocab ${String(m.vocab_pin).slice(0, 12)} · lipdR ${m.lipdr_version}`)))
}


// ---- carried context -------------------------------------------------------
//
// Clicking a point is a full page load, so the map state has to survive the
// navigation. It goes in sessionStorage rather than the URL: a dataset page's
// address is the thing people cite, and it should not carry somebody's zoom
// level and search box in it. The consequence -- a shared link opens without
// the context -- is right, because the context was never theirs.

const CTX_KEY = 'lv.mapctx'

function saveCtx (patch) {
  try {
    const prev = readCtx() || {}
    sessionStorage.setItem(CTX_KEY, JSON.stringify({ ...prev, ...patch, at: Date.now() }))
  } catch {}
}
function readCtx () {
  try {
    const raw = sessionStorage.getItem(CTX_KEY)
    if (!raw) return null
    const c = JSON.parse(raw)
    // Stale context from an hour ago is more confusing than none.
    if (!c || Date.now() - (c.at || 0) > 60 * 60 * 1000) return null
    return c
  } catch { return null }
}

// The dataset rows a saved scope refers to, recomputed rather than stored:
// keeping 7,293 rows in sessionStorage to redraw a map is the wrong trade.
async function rowsForScope (scope) {
  const all = await loadParquet('datasets')
  if (!scope || scope.kind === 'all') return all
  const members = await loadParquet('compilation_versions')
  const want = members.filter(r =>
    String(r.compilation).toLowerCase() === String(scope.name).toLowerCase() &&
    (!scope.version || String(r.version) === scope.version))
  const byId = new Map(all.map(r => [r.datasetId, r]))
  const byName = new Map(all.map(r => [r.dataSetName, r]))
  return want.map(r => byId.get(r.datasetId) || byName.get(r.dataset)).filter(Boolean)
}

function applyFilter (rows, f) {
  if (!f) return rows
  const q = String(f.q || '').toLowerCase()
  const arch = new Set(f.archives || [])
  return rows.filter(r => {
    if (arch.size && !arch.has(txt(r.archiveType))) return false
    if (!q) return true
    return txt(r.dataSetName).toLowerCase().includes(q) ||
           txt(r.geo_siteName).toLowerCase().includes(q) ||
           txt(r.archiveType).toLowerCase().includes(q)
  })
}


// ---- downloads -------------------------------------------------------------
//
// The exports have produced a zip of member .lpd files and a BibTeX file for
// every release since 0_6_2, and nothing ever linked to them. A compilation
// page that cannot hand you the compilation is not much of a compilation page.
//
// What exists is a served fact (downloads.json) rather than a guess, so a
// release whose zip has not been published shows no button instead of a 404.

let DOWNLOADS = null
async function downloads () {
  DOWNLOADS ??= await fetch('/dev/site/downloads.json').then(r => r.json()).catch(() => [])
  return DOWNLOADS
}
const humanBytes = n => {
  if (!n) return ''
  const u = ['B', 'KB', 'MB', 'GB']
  let i = 0, v = Number(n)
  while (v >= 1024 && i < u.length - 1) { v /= 1024; i++ }
  return `${v < 10 && i > 0 ? v.toFixed(1) : Math.round(v)} ${u[i]}`
}

async function renderCompilationDownloads (mount, name, version) {
  const all = await downloads()
  const d = all.find(x => String(x.compilation).toLowerCase() === String(name).toLowerCase() &&
                          String(x.version) === String(version))
  if (!d) {
    mount.replaceChildren(el('p', { class: 'muted small' },
      'No download has been published for this release.'))
    return
  }
  const base = `${BASE}/export/downloads/${encodeURIComponent(d.compilation)}/${encodeURIComponent(d.version)}`
  const kids = []
  if (d.zip) kids.push(el('a', { class: 'dlbtn', href: `${base}/${d.zip.file}`, download: '' },
    'Download all LiPD files', el('span', { class: 'dlsize' }, ` ${humanBytes(d.zip.bytes)}`)))
  if (d.bib) kids.push(el('a', { class: 'dlbtn secondary', href: `${base}/${d.bib.file}`, download: '' },
    'BibTeX', el('span', { class: 'dlsize' }, ` ${humanBytes(d.bib.bytes)}`)))
  kids.push(el('span', { class: 'muted small' },
    ` ${Number(d.datasets).toLocaleString()} datasets, as published in ${d.version}`))
  mount.replaceChildren(el('div', { class: 'downloads' }, ...kids))
}

// ---- routing ---------------------------------------------------------------
//
// Three shapes, read straight off the path:
//   /dev/lipdverse/                    everything, including datasets no
//                                      compilation reaches
//   /dev/<compilation>/<version>/      one release of one compilation
//   /dev/data/<datasetId>/<version>/   one dataset

function route () {
  const parts = location.pathname.replace(BASE, '').split('/').filter(Boolean)
  if (parts[0] === 'data' && parts[1]) return viewDataset(parts[1], parts[2])
  if (parts[0] && parts[0] !== 'lipdverse' && parts[0] !== 'site') {
    return viewCompilation(parts[0], parts[1])
  }
  return viewAll()
}

// ---- the everything view ---------------------------------------------------

async function viewAll () {
  $('#scope').textContent = 'the whole database'
  const rows = await loadParquet('datasets')
  saveCtx({ scope: { kind: 'all' }, label: 'all of LiPDverse', href: `${BASE}/lipdverse/` })
  renderCollection(rows, 'Every dataset in LiPDverse, including those no compilation contains.')
}

async function viewCompilation (name, version) {
  $('#scope').textContent = version ? `${name} · ${version}` : name
  const [members, all] = await Promise.all([
    loadParquet('compilation_versions'), loadParquet('datasets')
  ])
  const want = members.filter(r =>
    String(r.compilation).toLowerCase() === name.toLowerCase() &&
    (!version || String(r.version) === version))

  if (!want.length) {
    return $('#main').replaceChildren(
      el('p', {}, `No release named `, el('code', {}, `${name}${version ? '/' + version : ''}`), ` in this export.`),
      el('p', { class: 'muted' }, 'Older releases are served by their original pages; only releases the new pipeline produced appear here.'))
  }

  // A release records which dataset version it contained. Where that is known,
  // say so -- it is the difference between "these datasets" and "this release".
  const pinned = want.filter(r => r.datasetVersion).length
  const byId = new Map(all.map(r => [r.datasetId, r]))
  const byName = new Map(all.map(r => [r.dataSetName, r]))
  const rows = want.map(r => byId.get(r.datasetId) || byName.get(r.dataset)).filter(Boolean)

  saveCtx({ scope: { kind: 'compilation', name, version }, label: `${name} ${version || ''}`.trim(),
            href: location.pathname })
  const dlMount = el('div', {})
  if (version) renderCompilationDownloads(dlMount, name, version)
  const note = pinned
    ? `${rows.length.toLocaleString()} datasets, each pinned to the version this release contained.`
    : `${rows.length.toLocaleString()} datasets. This release recorded names only, so the data shown is current rather than as-published.`
  renderCollection(rows, note, !pinned, dlMount)
}

function renderCollection (rows, note, warn = false, dlMount = null) {
  const main = $('#main')
  const state = { q: '', archives: new Set(), limit: 200, sort: 'dataSetName', dir: 1,
                  sel: emptySelection(), index: null, loading: false }
  let map = null

  const archives = [...new Set(rows.map(r => txt(r.archiveType)).filter(Boolean))].sort()
  const search = el('input', { type: 'search', placeholder: 'Search name, site, archive…' })
  const chips = archives.map(a => el('button', { class: 'chip', 'aria-pressed': 'false' }, a))
  const advBtn = el('button', { class: 'chip advbtn', 'aria-expanded': 'false' }, 'Advanced \u25be')
  const clearBtn = el('button', { class: 'chip clearbtn hidden' }, 'Clear filters')
  const count = el('span', { class: 'count' })
  const advPanel = el('div', { class: 'adv hidden' })
  const svg = el('div')
  const tableWrap = el('div')
  const moreWrap = el('div', { class: 'more' })

  main.replaceChildren(
    el('p', { class: warn ? 'warn' : 'muted' }, note),
    dlMount || '',
    el('div', { class: 'controls' },
      el('div', { class: 'searchgroup' }, search, advBtn, clearBtn),
      el('div', { class: 'chipset' }, ...chips),
      count),
    advPanel, svg, tableWrap, moreWrap)

  const apply = () => {
    const q = state.q.toLowerCase()
    let out = rows.filter(r => {
      if (state.archives.size && !state.archives.has(txt(r.archiveType))) return false
      if (state.index && selectionCount(state.sel) && !matches(r, state.index, state.sel)) return false
      if (!q) return true
      return txt(r.dataSetName).toLowerCase().includes(q) ||
             txt(r.geo_siteName).toLowerCase().includes(q) ||
             txt(r.archiveType).toLowerCase().includes(q)
    })
    const k = state.sort
    out = out.slice().sort((a, b) => {
      const x = a[k], y = b[k]
      const nx = num(x), ny = num(y)
      if (nx !== null && ny !== null) return (nx - ny) * state.dir
      return String(txt(x)).localeCompare(String(txt(y))) * state.dir
    })
    count.textContent = `${out.length.toLocaleString()} of ${rows.length.toLocaleString()}`
    if (!map) {
      map = createMap(svg, { onView: (v) => saveCtx({ view: v }) })
      const c = readCtx()
      if (c?.view) map.setView(c.view)
    }
    map.setPoints(out)
    saveCtx({ filter: { q: state.q, archives: [...state.archives] } })
    drawTable(tableWrap, out.slice(0, state.limit))
    moreWrap.replaceChildren(out.length > state.limit
      ? el('button', { class: 'more' }, `Show more (${(out.length - state.limit).toLocaleString()} hidden)`)
      : '')
    const btn = $('button.more', moreWrap)
    if (btn) btn.onclick = () => { state.limit += 500; apply() }
  }

  const nActive = () => selectionCount(state.sel) + state.archives.size + (state.q ? 1 : 0)
  const syncClear = () => clearBtn.classList.toggle('hidden', nActive() === 0)

  const drawAdv = () => {
    if (!state.index) {
      advPanel.replaceChildren(el('p', { class: 'loading' },
        'Loading vocabulary from the timeseries and interpretations\u2026'))
      return
    }
    const opts = facetOptions(rows, state.index)
    const groups = FACETS.map(f => {
      const entries = [...(opts.get(f.key) || new Map())]
        .filter(([v, n]) => v && n)
        .sort((x, y) => y[1] - x[1] || String(x[0]).localeCompare(String(y[0])))
      if (!entries.length) return null
      const sel = state.sel.facets[f.key]
      const box = el('div', { class: 'facet' })
      const list = el('div', { class: 'facetlist' })
      const draw = (filterText = '') => {
        const ft = filterText.toLowerCase()
        const shown = entries.filter(([v]) => !ft || String(v).toLowerCase().includes(ft))
        list.replaceChildren(...shown.slice(0, 400).map(([v, n]) => {
          const cb = el('input', { type: 'checkbox' })
          cb.checked = sel.has(v)
          cb.onchange = () => {
            if (cb.checked) sel.add(v); else sel.delete(v)
            state.limit = 200; apply(); syncClear()
          }
          return el('label', { class: 'facetopt' }, cb,
            el('span', { class: 'facetval' }, String(v)),
            el('span', { class: 'facetn muted' }, String(n)))
        }))
        if (shown.length > 400) {
          list.append(el('p', { class: 'muted small' },
            `${(shown.length - 400).toLocaleString()} more \u2014 type to narrow`))
        }
      }
      const head = el('div', { class: 'facethead' },
        el('span', {}, f.label), el('span', { class: 'muted small' }, ` ${entries.length}`))
      if (f.long) {
        const fb = el('input', { type: 'search', class: 'facetfilter', placeholder: 'filter\u2026' })
        fb.oninput = () => draw(fb.value)
        box.append(head, fb, list)
      } else box.append(head, list)
      draw()
      return box
    }).filter(Boolean)

    const from = el('input', { type: 'number', class: 'yr', placeholder: 'from', value: state.sel.from ?? '' })
    const to = el('input', { type: 'number', class: 'yr', placeholder: 'to', value: state.sel.to ?? '' })
    const res = el('input', { type: 'number', class: 'yr', placeholder: 'years', value: state.sel.resMax ?? '', min: 0, step: 'any' })
    const onNum = () => {
      const g = (i) => i.value === '' ? null : Number(i.value)
      state.sel.from = g(from); state.sel.to = g(to); state.sel.resMax = g(res)
      state.limit = 200; apply(); syncClear()
    }
    for (const i of [from, to, res]) i.onchange = onNum

    advPanel.replaceChildren(
      el('div', { class: 'facetgrid' }, ...groups),
      el('div', { class: 'facetgrid ranges' },
        el('div', { class: 'facet' },
          el('div', { class: 'facethead' }, 'Covers years (CE, negative = BCE)'),
          el('div', { class: 'rangerow' }, from, el('span', { class: 'muted' }, 'to'), to),
          el('p', { class: 'muted small' }, 'Any overlap with this window.')),
        el('div', { class: 'facet' },
          el('div', { class: 'facethead' }, 'Resolution at least as fine as'),
          el('div', { class: 'rangerow' }, res, el('span', { class: 'muted' }, 'years')),
          el('p', { class: 'muted small' }, 'Median spacing of any one column.'))))
  }

  advBtn.onclick = async () => {
    const nowHidden = advPanel.classList.toggle('hidden')
    advBtn.setAttribute('aria-expanded', nowHidden ? 'false' : 'true')
    advBtn.textContent = nowHidden ? 'Advanced \u25be' : 'Advanced \u25b4'
    if (nowHidden) return
    drawAdv()
    if (!state.index && !state.loading) {
      state.loading = true
      try {
        const [ts, it] = await Promise.all([
          loadParquet('timeseries', ['TSid', 'datasetId', 'tableType', 'tableKind',
            'variableName', 'units', 'proxy', 'proxyGeneral', 'medianResolution']),
          loadParquet('interpretations')
        ])
        state.index = buildIndex(rows, ts, it)
      } catch (e) {
        advPanel.replaceChildren(el('p', { class: 'warn' }, 'Could not load the vocabulary.'))
        state.loading = false
        return
      }
      state.loading = false
      drawAdv()
    }
  }

  clearBtn.onclick = () => {
    state.q = ''; search.value = ''
    state.archives.clear(); chips.forEach(c => c.setAttribute('aria-pressed', 'false'))
    state.sel = emptySelection()
    state.limit = 200
    if (!advPanel.classList.contains('hidden')) drawAdv()
    apply(); syncClear()
  }

  search.oninput = () => { state.q = search.value; state.limit = 200; apply(); syncClear() }
  chips.forEach((c, i) => {
    c.onclick = () => {
      const a = archives[i]
      if (state.archives.has(a)) { state.archives.delete(a); c.setAttribute('aria-pressed', 'false') }
      else { state.archives.add(a); c.setAttribute('aria-pressed', 'true') }
      state.limit = 200; apply(); syncClear()
    }
  })
  tableWrap.addEventListener('click', e => {
    const th = e.target.closest('th[data-k]')
    if (!th) return
    const k = th.dataset.k
    state.dir = (state.sort === k) ? -state.dir : 1
    state.sort = k
    apply()
  })
  apply()
}

function drawTable (mount, rows) {
  const cols = [
    ['dataSetName', 'Dataset'], ['archiveType', 'Archive'], ['geo_siteName', 'Site'],
    ['geo_latitude', 'Lat'], ['geo_longitude', 'Lon'],
    ['minYear', 'From'], ['maxYear', 'To'], ['n_timeseries', 'Series'], ['version', 'Version']
  ]
  const head = el('tr', {}, cols.map(([k, label]) =>
    el('th', { 'data-k': k, class: ['geo_latitude', 'geo_longitude', 'minYear', 'maxYear', 'n_timeseries'].includes(k) ? 'num' : '' }, label)))
  const body = rows.map(r => {
    const v = txt(r.version).replace(/\./g, '_')
    return el('tr', {},
      el('td', {}, el('a', { href: `${BASE}/data/${encodeURIComponent(r.datasetId)}/${encodeURIComponent(v)}/` }, txt(r.dataSetName))),
      el('td', {}, txt(r.archiveType)),
      el('td', {}, txt(r.geo_siteName)),
      el('td', { class: 'num' }, num(r.geo_latitude)?.toFixed(2) ?? ''),
      el('td', { class: 'num' }, num(r.geo_longitude)?.toFixed(2) ?? ''),
      el('td', { class: 'num' }, fmtYear(r.minYear)),
      el('td', { class: 'num' }, fmtYear(r.maxYear)),
      el('td', { class: 'num' }, txt(num(r.n_timeseries))),
      el('td', { class: 'muted' }, txt(r.version)))
  })
  mount.replaceChildren(el('table', {}, el('thead', {}, head), el('tbody', {}, body)))
}


// ---- values, by row range --------------------------------------------------

let VALUES_INDEX = null
async function valuesFor (datasetId) {
  VALUES_INDEX ??= await fetch('/dev/site/data/values-index.json').then(r => r.json())
  const span = VALUES_INDEX[datasetId]
  if (!span) return null
  const [start, end] = span
  const file = await asyncBufferFromUrl({ url: `${EXPORT_DIR}/values.parquet` })
  const rows = await parquetReadObjects({
    file, columns: ['TSid', 'row_index', 'value_num'], rowStart: start, rowEnd: end
  })
  // Contiguous by TSid within the range, but sort by row_index anyway so the
  // series is in file order rather than whatever the read returns.
  const by = new Map()
  for (const r of rows) {
    if (!by.has(r.TSid)) by.set(r.TSid, [])
    by.get(r.TSid).push(r)
  }
  const out = new Map()
  for (const [tsid, rs] of by) {
    rs.sort((p, q) => Number(p.row_index) - Number(q.row_index))
    out.set(tsid, rs.map(r => r.value_num))
  }
  return { rows: rows.length, byTsid: out }
}


// ---- publications ----------------------------------------------------------
//
// 13,271 rows across the database; 5,495 datasets carry at least one with an
// author and a title, 82.9% of rows have a DOI, and 5,649 have no author at
// all. Plenty are DOI-only, so the renderer has to make something presentable
// out of a row with almost nothing in it.
//
// `authors` is list<string>, but inconsistently: sometimes each element is one
// author, sometimes a single element holds the whole list separated by
// semicolons (median 1 element, max 93). Both shapes are flattened before
// anything else looks at them.

function splitAuthors (authors) {
  const raw = Array.isArray(authors) ? authors : (authors ? [authors] : [])
  const out = []
  for (const a of raw) {
    for (const part of String(a ?? '').split(/\s*;\s*/)) {
      const t = part.trim()
      if (t) out.push(t)
    }
  }
  return out
}

// "Godad, Shital P."  -> "Godad, S. P."
// "Shital P. Godad"   -> "Godad, S. P."
// 93.2% of names carry a comma, so the first branch is the usual one.
function apaName (name) {
  const n = String(name).trim().replace(/\s+/g, ' ')
  if (/\bet al\.?$/i.test(n)) return n
  let last, rest
  if (n.includes(',')) {
    const bits = n.split(',')
    last = bits.shift().trim()
    rest = bits.join(' ').trim()
  } else {
    const toks = n.split(' ')
    last = toks.pop()
    rest = toks.join(' ')
  }
  const initials = rest.split(/[\s.]+/).filter(Boolean)
    .map(t => t[0].toUpperCase() + '.').join(' ')
  return initials ? `${last}, ${initials}` : last
}

// APA 7: up to 20 names in full, then an ellipsis and the final author.
function apaAuthors (names) {
  const n = names.map(apaName)
  if (!n.length) return ''
  if (n.length === 1) return n[0]
  if (n.length === 2) return `${n[0]}, & ${n[1]}`
  if (n.length <= 20) return `${n.slice(0, -1).join(', ')}, & ${n[n.length - 1]}`
  return `${n.slice(0, 19).join(', ')}, ... ${n[n.length - 1]}`
}

function citationParts (p) {
  const names = splitAuthors(p.authors)
  const year = num(p.year)
  const title = txt(p.title).replace(/\s*\.\s*$/, '')
  const journal = txt(p.journal)
  const doi = txt(p.doi).replace(/^https?:\/\/(dx\.)?doi\.org\//i, '').trim()
  return { authors: apaAuthors(names), year, title, journal, doi, nAuthors: names.length }
}

function citationText (p) {
  const c = citationParts(p)
  const bits = []
  if (c.authors) bits.push(c.authors)
  bits.push(`(${c.year ?? 'n.d.'}).`)
  if (c.title) bits.push(c.title + '.')
  if (c.journal) bits.push(c.journal + '.')
  if (c.doi) bits.push(`https://doi.org/${c.doi}`)
  return bits.join(' ').replace(/\s+/g, ' ').trim()
}

function renderPubs (pubs) {
  if (!pubs.length) {
    return el('p', { class: 'muted' }, 'No publication is recorded for this dataset.')
  }
  const ordered = pubs.slice().sort((x, y) => Number(x.pubIndex || 0) - Number(y.pubIndex || 0))
  return el('ol', { class: 'pubs' }, ordered.map(p => {
    const c = citationParts(p)
    // Nothing but a DOI is common enough to deserve its own presentation
    // rather than a citation made of blanks.
    if (!c.authors && !c.title && !c.journal) {
      return el('li', {}, c.doi
        ? el('span', {}, el('span', { class: 'muted' }, 'DOI only \u2014 no citation details recorded: '),
            el('a', { href: `https://doi.org/${c.doi}`, rel: 'noopener' }, c.doi))
        : el('span', { class: 'muted' }, 'Publication recorded with no details.'))
    }
    return el('li', {},
      el('span', { class: 'cite' },
        c.authors ? el('span', {}, c.authors, ' ') : '',
        el('span', {}, `(${c.year ?? 'n.d.'}). `),
        c.title ? el('span', {}, c.title, '. ') : '',
        c.journal ? el('em', {}, c.journal, '. ') : '',
        c.doi ? el('a', { href: `https://doi.org/${c.doi}`, rel: 'noopener' },
                   `https://doi.org/${c.doi}`) : ''),
      el('button', { class: 'chip copycite', title: 'Copy citation',
                     'data-cite': citationText(p) }, 'copy'))
  }))
}

// ---- one dataset -----------------------------------------------------------


// Which plot to open with.
//
// `primaryTimeseries` is the right answer when it is set, and it is set on only
// 7% of columns: 62.8% of datasets flag none at all. So the fallback matters
// more than the flag does. Taking simply the first column landed on something
// uninteresting -- uncertainty, notes, depth -- for 27.5% of those datasets;
// preferring a column that carries a proxy and skipping the obviously
// ancillary names brings that to 0.1%, measured across all 4,572 of them.
const ANCILLARY = /^(uncertainty|error|sd|std|stdev|notes?|core|section|sample|depth|thickness|hiatus|hasgap|hashiatus|mineralogy|correction|labid|material)/i

function pickDefault (cols, series) {
  const usable = cols.filter(c => series.has(c.TSid))
  if (!usable.length) return null
  const score = (c) => (
    (c.primaryTimeseries ? 0 : 100) +
    (ANCILLARY.test(String(c.variableName || '')) ? 10 : 0) +
    ((c.proxy && String(c.proxy).length) ? 0 : 1)
  )
  return usable.reduce((best, c) => score(c) < score(best) ? c : best, usable[0])
}



// Everything the export holds about one column, including its interpretations.
// Fields that are empty are omitted rather than printed blank: a panel of 19
// labels with 6 values fills the screen while saying less than a short list.
const TS_FIELDS = [
  ['variableName', 'Variable'], ['standardName', 'Standard name'], ['units', 'Units'],
  ['proxy', 'Proxy'], ['proxyGeneral', 'Proxy (general)'], ['description', 'Description'],
  ['TSid', 'TSid'], ['tableType', 'Block'], ['tableKind', 'Table kind'],
  ['isAxis', 'Is axis'], ['primaryTimeseries', 'Primary timeseries'],
  ['createdBy', 'Created by'], ['minYear', 'From'], ['maxYear', 'To'],
  ['medianResolution', 'Median resolution'], ['n_values', 'Values'],
  ['n_rows', 'Ensemble rows'], ['n_members', 'Ensemble members']
]

function renderColumnDetail (r, interps) {
  const rows = []
  for (const [k, label] of TS_FIELDS) {
    let v = r[k]
    if (typeof v === 'boolean') v = v ? 'yes' : 'no'
    else if (k === 'minYear' || k === 'maxYear') v = fmtYear(v)
    else if (k === 'medianResolution') v = num(v)?.toFixed(2) ?? ''
    else v = txt(num(v) ?? v)
    if (v === '' || v === null || v === undefined) continue
    rows.push(el('dt', {}, label), el('dd', {}, String(v)))
  }
  const kids = [el('dl', { class: 'meta detail' }, rows)]

  if (interps.length) {
    kids.push(el('h4', {}, `Interpretations (${interps.length})`))
    kids.push(el('table', { class: 'interp' },
      el('thead', {}, el('tr', {}, ['Scope', 'Rank', 'Variable', 'Detail', 'Direction', 'Seasonality', 'Annual', 'Basis']
        .map(h => el('th', {}, h)))),
      el('tbody', {}, interps
        .slice().sort((x, y) => Number(x.rank || 0) - Number(y.rank || 0))
        .map(i => el('tr', {},
          el('td', {}, txt(i.scope)), el('td', {}, txt(num(i.rank))),
          el('td', {}, txt(i.variable)), el('td', {}, txt(i.variableDetail)),
          el('td', {}, txt(i.direction)), el('td', {}, txt(i.seasonality)),
          el('td', {}, i.isAnnual === null || i.isAnnual === undefined ? '' : (i.isAnnual ? 'yes' : 'no')),
          el('td', {}, txt(i.basis)))))))
  } else {
    kids.push(el('p', { class: 'muted' }, 'No interpretation recorded for this column.'))
  }
  return el('div', { class: 'coldetail' }, kids)
}

async function drawDatasetMap (mount, backBar, d) {
  const ctx = readCtx()
  const lat = num(d.geo_latitude), lon = num(d.geo_longitude)
  const map = createMap(mount, { highlight: d.datasetId })

  if (ctx?.scope) {
    const rows = applyFilter(await rowsForScope(ctx.scope), ctx.filter)
    // The dataset itself may sit outside the filter that was active; show it
    // regardless, or the page would map everything except its own subject.
    if (!rows.some(r => r.datasetId === d.datasetId)) rows.push(d)
    map.setPoints(rows)
    if (ctx.view) map.setView(ctx.view)
    else if (lat !== null && lon !== null) map.focus(lon, lat, 60)

    const n = rows.length
    backBar.replaceChildren(
      el('a', { class: 'back', href: ctx.href || `${BASE}/lipdverse/` }, '\u2190 ', txt(ctx.label) || 'back'),
      el('span', { class: 'muted' },
        ` \u00b7 showing ${n.toLocaleString()} dataset${n === 1 ? '' : 's'} from there`),
      el('button', { class: 'chip', id: 'justthis' }, 'Just this one'))
    $('#justthis', backBar).onclick = () => {
      map.setPoints([d])
      if (lat !== null && lon !== null) map.focus(lon, lat, 40)
    }
  } else {
    map.setPoints([d])
    if (lat !== null && lon !== null) map.focus(lon, lat, 40)
    backBar.replaceChildren(
      el('a', { class: 'back', href: `${BASE}/lipdverse/` }, '\u2190 all of LiPDverse'))
  }
}

async function viewDataset (datasetId, version) {
  $('#scope').textContent = datasetId
  const rows = await loadParquet('datasets')
  const d = rows.find(r => r.datasetId === datasetId)
  if (!d) {
    return $('#main').replaceChildren(
      el('p', {}, 'No dataset with id ', el('code', {}, datasetId), ' in this export.'))
  }
  $('#scope').textContent = d.dataSetName

  const current = txt(d.version).replace(/\./g, '_')
  const asked = version || current
  const stale = asked !== current

  const meta = el('dl', { class: 'meta' },
    el('dt', {}, 'Dataset ID'), el('dd', {}, el('code', {}, txt(d.datasetId))),
    el('dt', {}, 'Version'), el('dd', {}, txt(d.version)),
    el('dt', {}, 'Archive'), el('dd', {}, txt(d.archiveType)),
    el('dt', {}, 'Site'), el('dd', {}, txt(d.geo_siteName)),
    el('dt', {}, 'Location'), el('dd', {}, `${num(d.geo_latitude)?.toFixed(3) ?? '?'}, ${num(d.geo_longitude)?.toFixed(3) ?? '?'}` +
      (num(d.geo_elevation) !== null ? ` \u00b7 ${num(d.geo_elevation)} m` : '')),
    el('dt', {}, 'Covers'), el('dd', {},
      (num(d.minYear) === null)
        ? el('span', { class: 'muted' }, 'not established from the measurements')
        : `${fmtYear(d.minYear)} to ${fmtYear(d.maxYear)}`),
    el('dt', {}, 'Timeseries'), el('dd', {}, txt(num(d.n_timeseries))),
    el('dt', {}, 'Chronology'), el('dd', {}, d.hasChron ? 'yes' : 'no',
      d.hasChronEnsemble ? ' \u00b7 with ensemble' : ''))

  // The dataset's own LiPD file. Every dataset in the database is published,
  // not only compilation members, so the 221 that no compilation reaches can
  // still be downloaded from their page. Named by dataSetName because that is
  // what the file is called; the link is built from the row we are rendering,
  // so a rename cannot leave it pointing at nothing.
  const lpd = `${BASE}/export/lpd/${encodeURIComponent(txt(d.dataSetName))}.lpd`
  const dlbar = el('div', { class: 'downloads' },
    el('a', { class: 'dlbtn', href: lpd, download: '' }, 'Download LiPD file'),
    el('span', { class: 'muted small' },
      ` ${txt(d.dataSetName)}.lpd \u00b7 version ${txt(d.version)}`))

  const tableWrap = el('div', {}, el('p', { class: 'loading' }, 'Loading timeseries\u2026'))
  const charts = el('div', {})
  const mapMount = el('div', {})
  const backBar = el('div', { class: 'backbar' })
  const pubsMount = el('div', {}, el('p', { class: 'loading' }, 'Loading publications\u2026'))
  // Two columns where there is room: identity and citations on the left, the
  // map beside them rather than pushed below the fold. Below ~900px it stacks,
  // with the map between the metadata and the publications -- the map is
  // orientation, and orientation belongs before the reading matter.
  $('#main').replaceChildren(
    stale ? el('p', { class: 'warn' },
      `This URL asks for version ${asked}; the current version is ${current}.`) : '',
    backBar,
    el('div', { class: 'dstop' },
      el('div', { class: 'dsmeta' }, meta, dlbar),
      el('div', { class: 'dsmap' }, mapMount),
      el('div', { class: 'dspubs' },
        el('h2', { class: 'h2 tight' }, 'Publications'),
        pubsMount)),
    el('h2', { class: 'h2' }, 'Measurements'),
    el('p', { class: 'muted hint' }, 'Click a row to show or hide its plot.'),
    tableWrap,
    charts)

  // The map you came from, as you left it: same set of points, same filter,
  // same zoom, with this dataset marked. Arriving cold -- a shared link, a
  // bookmark -- there is no context to inherit, so the map centres on the site
  // instead and says so.
  drawDatasetMap(mapMount, backBar, d)
  loadParquet('publications').then(all => {
    pubsMount.replaceChildren(renderPubs(all.filter(p => p.datasetId === datasetId)))
    pubsMount.addEventListener('click', async (ev) => {
      const b = ev.target.closest('.copycite')
      if (!b) return
      try { await navigator.clipboard.writeText(b.dataset.cite); b.textContent = 'copied' }
      catch { b.textContent = 'copy failed' }
      setTimeout(() => { b.textContent = 'copy' }, 1600)
    })
  }).catch(() => pubsMount.replaceChildren(el('p', { class: 'warn' }, 'Could not load publications.')))

  const ts = await loadParquet('timeseries')
  const paleo = ts.filter(r => r.datasetId === datasetId &&
    r.tableType === 'paleo' && r.tableKind === 'measurement')

  // The table is drawn before the values arrive, so the page is readable while
  // the range read is in flight. Rows become clickable once we know which
  // columns could be paired with a time axis.
  const state = { selected: new Set(), series: new Map(), ready: false,
                  expanded: new Set(), interps: new Map() }

  const drawRows = () => {
    const head = el('tr', {},
      el('th', { class: 'togglecol' }, ''),
      ...['Variable', 'Units', 'Proxy', 'From', 'To', 'Resolution', 'N']
        .map((h, i) => el('th', { class: i >= 3 ? 'num' : '' }, h)),
      el('th', { class: 'morecol' }, ''))
    const body = paleo.map(r => {
      const plottable = state.series.has(r.TSid)
      const on = state.selected.has(r.TSid)
      const open = state.expanded.has(r.TSid)
      const tr = el('tr', {
        class: [state.ready ? (plottable ? 'clickable' : 'noplot') : '', on ? 'on' : ''].filter(Boolean).join(' '),
        'data-tsid': r.TSid,
        title: state.ready && !plottable ? 'No time axis could be paired with this column' : null
      },
        el('td', { class: 'togglecol' },
          state.ready
            ? el('span', { class: 'box', 'aria-hidden': 'true' }, on ? '\u2713' : (plottable ? '' : '\u2014'))
            : ''),
        el('td', {}, txt(r.variableName)),
        el('td', {}, txt(r.units)),
        el('td', {}, txt(r.proxy)),
        el('td', { class: 'num' }, fmtYear(r.minYear)),
        el('td', { class: 'num' }, fmtYear(r.maxYear)),
        el('td', { class: 'num' }, num(r.medianResolution)?.toFixed(1) ?? ''),
        el('td', { class: 'num' }, txt(num(r.n_values))),
        el('td', { class: 'morecol' },
          el('button', { class: 'chip more-btn', 'data-more': r.TSid,
                         'aria-expanded': open ? 'true' : 'false' }, open ? 'less' : 'more')))
      if (!open) return [tr]
      const detail = el('tr', { class: 'detailrow' },
        el('td', { colspan: 9 },
          renderColumnDetail(r, (state.interps.get(r.TSid) || []))))
      return [tr, detail]
    }).flat()
    tableWrap.replaceChildren(el('table', { class: 'ts' }, el('thead', {}, head), el('tbody', {}, body)))
  }

  const drawCharts = () => {
    const picked = paleo.filter(r => state.selected.has(r.TSid))
    if (!picked.length) {
      charts.replaceChildren(el('p', { class: 'muted' }, 'No plots shown. Click a row above to add one.'))
      return
    }
    charts.replaceChildren(...picked.map(r => {
      const s = state.series.get(r.TSid)
      return el('section', { class: 'chartsec' },
        el('h3', {}, txt(r.variableName),
          r.units ? el('span', { class: 'muted' }, ` (${txt(r.units)})`) : '',
          el('span', { class: 'muted' }, ` \u00b7 against ${txt(s.axis.variableName)}`),
          el('button', { class: 'chip closechart', 'data-tsid': r.TSid, title: 'Hide' }, '\u00d7')),
        plot(s.x, s.y, { label: txt(r.variableName), ylabel: txt(r.units) }))
    }))
  }

  const toggle = (tsid) => {
    if (!state.series.has(tsid)) return
    if (state.selected.has(tsid)) state.selected.delete(tsid)
    else state.selected.add(tsid)
    drawRows(); drawCharts()
  }
  tableWrap.addEventListener('click', async (ev) => {
    const more = ev.target.closest('[data-more]')
    if (more) {
      ev.stopPropagation()
      const tsid = more.dataset.more
      if (state.expanded.has(tsid)) state.expanded.delete(tsid)
      else {
        state.expanded.add(tsid)
        // Interpretations are 88,099 rows for the whole database; fetch once,
        // the first time anyone actually opens a panel.
        if (!state.interpsLoaded) {
          state.interpsLoaded = true
          try {
            const all = await loadParquet('interpretations')
            for (const i of all) {
              if (!state.interps.has(i.TSid)) state.interps.set(i.TSid, [])
              state.interps.get(i.TSid).push(i)
            }
          } catch {}
        }
      }
      drawRows()
      return
    }
    const tr = ev.target.closest('tr[data-tsid]')
    if (tr) toggle(tr.dataset.tsid)
  })
  charts.addEventListener('click', (ev) => {
    const b = ev.target.closest('.closechart')
    if (b) toggle(b.dataset.tsid)
  })

  drawRows()
  charts.replaceChildren(el('p', { class: 'loading' }, 'Loading data\u2026'))

  try {
    const v = await valuesFor(datasetId)
    if (!v) {
      charts.replaceChildren(el('p', { class: 'muted' }, 'No values for this dataset in the export.'))
      state.ready = true; drawRows(); return
    }
    for (const s of pairAxis(paleo, v.byTsid)) state.series.set(s.col.TSid, s)
    state.ready = true

    const first = pickDefault(paleo, state.series)
    if (first) state.selected.add(first.TSid)
    drawRows(); drawCharts()
    if (!state.series.size) {
      charts.replaceChildren(el('p', { class: 'muted' },
        'Nothing plottable: no measurement column could be paired with a calendar time axis.'))
    }
  } catch (e) {
    state.ready = true; drawRows()
    charts.replaceChildren(el('p', { class: 'warn' }, 'Could not load the values.'),
                           el('pre', { class: 'muted' }, String(e && e.message || e)))
  }
}

boot().catch(e => {
  $('#main').replaceChildren(
    el('p', { class: 'warn' }, 'Could not load the export.'),
    el('pre', { class: 'muted' }, String(e && e.message || e)))
})
