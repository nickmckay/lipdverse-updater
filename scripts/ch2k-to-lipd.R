#!/usr/bin/env Rscript
#
# Build LiPD files from the CoralHydro2k MATLAB structure.
#
#   ./scripts/ch2k-extract.py ~/Downloads/hydro2kv2_0_3.mat /tmp/ch2k.json
#   ./scripts/ch2k-to-lipd.R /tmp/ch2k.json            # dry run, reports only
#   ./scripts/ch2k-to-lipd.R /tmp/ch2k.json --write    # write the files
#
# Output goes to ~/Dropbox/lipdverse/ch2k_26update/, staged for the regular
# ingest path rather than dropped into the database.
#
# ---------------------------------------------------------------------------
# IDENTITY IS THE WHOLE GAME HERE
#
# convert.m mints identifiers from the core code -- datasetId = ['ch2k' dsn],
# TSid = [dsn '_' var]. Those are not the identifiers the database holds. Used
# as-is, all 179 existing datasets would arrive as NEW datasets sitting beside
# the originals, and the compilation would double.
#
# So: match on dataSetName (Nick, 2026-08-19), reuse the existing datasetId and
# the existing TSid of each matching variableName, and mint only for what is
# genuinely new -- 48 cores, plus any new column on an existing core. datasetId
# never changes; that is what makes an update an update.
# ---------------------------------------------------------------------------
#
# The mapping is review/ch2k-field-mapping.csv, which is the reviewable record
# of every decision. Scopes: `sheet`/`global` write to the dataset root or geo
# or pub, `variable` writes to the column it belongs to, `csm` writes under
# inCompilation[CoralHydro2k]$csm, DROP is deliberate, MEASUREMENT becomes a
# column, STRUCTURAL is an uncertainty vector.

suppressPackageStartupMessages({library(dplyr); library(jsonlite)})
suppressMessages(devtools::load_all(quiet = TRUE))

args  <- commandArgs(trailingOnly = TRUE)
src   <- args[!grepl("^--", args)][1]
write <- "--write" %in% args
lim   <- suppressWarnings(as.integer(sub("^--limit=", "", grep("^--limit=", args, value = TRUE))))
lim   <- if (length(lim) && !is.na(lim)) lim else Inf
out   <- path.expand("~/Dropbox/lipdverse/ch2k_26update")
COMP  <- "CoralHydro2k"
if (is.na(src)) stop("usage: ch2k-to-lipd.R <extracted.json> [--write] [--limit=N]")

ch  <- jsonlite::fromJSON(src, simplifyVector = FALSE)
map <- readr::read_csv("review/ch2k-field-mapping.csv", show_col_types = FALSE)
cat(sprintf("source      : CoralHydro2k v%s, %d cores\n", ch$version, length(ch$cores)))

# ---- identity from the database -------------------------------------------

idx <- lv_db_index(lv_scan(lv_path("database")), cache = TRUE)
known_ds  <- unique(stats::na.omit(idx$datasets$datasetId))
known_ts  <- unique(stats::na.omit(idx$timeseries$TSid))
ds_id     <- stats::setNames(idx$datasets$datasetId, idx$datasets$fileDataSetName)
# (dataSetName, variableName) -> TSid, for reusing a column's identity
ts_key <- paste(idx$timeseries$dataSetName, idx$timeseries$variableName)
ts_id  <- stats::setNames(idx$timeseries$TSid, ts_key)

# CoralHydro2k's identifiers are legacy-style -- ch2k<coreCode> for a dataset,
# <coreCode>_<variable> for a column -- and the 179 existing records carry them.
# New records follow the same convention (Nick, 2026-08-19) so the compilation
# does not end up with two ID styles side by side. Both fall back to a random id
# if the conventional one is somehow taken, because uniqueness outranks tidiness.
new_dsid <- function(dsn) {
  i <- paste0("ch2k", dsn)
  if (i %in% known_ds) repeat { i <- lipdR::createDatasetId(); if (!i %in% known_ds) break }
  known_ds <<- c(known_ds, i); i
}
new_tsid <- function(dsn, varname) {
  i <- paste0(dsn, "_", gsub("/", "", varname))
  if (i %in% known_ts) repeat { i <- lipdR::createTSid(); if (!i %in% known_ts) break }
  # (known_ts grows as we go, so a name minted earlier in this run is taken too)
  known_ts <<- c(known_ts, i); i
}

# ---- the variables, and how each is expressed ------------------------------
#
# `_annual` variants are the same measurement at annual resolution, so they
# carry the parent's proxy and units and are distinguished by nominalResolution.
# The controlled vocabulary has no d18O_sw: seawater d18O is variableName
# "d18O" with the material carried separately, and it is INFERRED rather than
# measured -- the value is derived, not measured on seawater (Nick,
# 2026-08-19). The published files are inconsistent about this, using
# measurementMaterial=seawater on one column and inferredMaterial="sea water"
# on another of the same dataset; writing inferred everywhere settles it.
VARS <- list(
  d18O           = list(name = "d18O",  units = "permil", res = NULL,     material = NULL),
  SrCa           = list(name = "Sr/Ca", units = NULL,     res = NULL,     material = NULL),
  d18O_sw        = list(name = "d18O",  units = "permil", res = NULL,     material = "sea water"),
  d18O_annual    = list(name = "d18O",  units = "permil", res = "annual", material = NULL),
  SrCa_annual    = list(name = "Sr/Ca", units = NULL,     res = "annual", material = NULL),
  d18O_sw_annual = list(name = "d18O",  units = "permil", res = "annual", material = "sea water"))
UNC <- list(d18O_err = "d18O", SrCa_err = "SrCa")
# convert.m renamed the error fields BEFORE minting TSids -- [ti.d18OUncertainty]
# = ti.d18O_err -- so the published columns are <core>_d18OUncertainty, not
# <core>_d18O_err. Keying on the raw source name would mint a new id and orphan
# the existing column.
UNC_TSID <- list(d18O_err = "d18OUncertainty", SrCa_err = "SrCaUncertainty")

# ‰ and º survive in the source; the published files use permil and strip the
# degree sign. Same normalisation both places, or the merge sees a change on
# every run.
norm_units <- function(x) {
  if (is.null(x) || is.na(x)) return(NULL)
  x <- trimws(gsub("º|°", "", as.character(x)))
  if (grepl("^(‰|permil|per mil|per mille)$", x, ignore.case = TRUE)) "permil" else x
}
s1 <- function(x) { if (is.null(x)) return(NULL); v <- as.character(x)[1]
                    if (is.na(v) || !nzchar(trimws(v))) NULL else trimws(v) }
n1 <- function(x) { if (is.null(x)) return(NULL); v <- suppressWarnings(as.numeric(x)[1])
                    if (is.na(v)) NULL else v }


# unlist() DROPS NULLs, and a NaN in the source becomes a JSON null. On
# AB08MEN01 that gave year 1,677 long and d18O 1,670 -- every point after the
# first gap paired with the wrong year, and a table that cannot be made
# rectangular, so writeLipd emitted no CSV at all. Keep the gaps as NA.
num_vec <- function(x) {
  if (is.null(x)) return(numeric(0))
  vapply(x, function(z) if (is.null(z)) NA_real_ else suppressWarnings(as.numeric(z)[1]),
         numeric(1), USE.NAMES = FALSE)
}

rule <- function(field) {
  r <- map[map$source_field == field, , drop = FALSE]
  if (!nrow(r)) return(NULL)
  list(target = s1(r$target[1]), scope = s1(r$scope[1]))
}

# ---- build or update one dataset -------------------------------------------
#
# An existing dataset is UPDATED IN PLACE, not rebuilt. Building a replacement
# from the .mat alone drops everything the published file has accumulated:
# measured against the 179, that was 6,993 fields, 1,005 of them curator-owned
# (coralHydro2kGroup, jcpMeasured, coralExtensionRate, isComposite, notes).
# The .mat is the source for what CoralHydro2k measured; it is not the source
# for what the compilation has since curated. Only the fields it actually
# carries are touched.

# Everything the .mat has to say about a dataset, resolved through the mapping.
resolve <- function(dsn, rec) {
  m <- rec$meta
  root <- list(); geo <- list(); pubs <- list(); csm <- list(); colmeta <- list()
  for (f in names(m)) {
    r <- rule(f)
    if (is.null(r) || is.null(r$target) || r$scope %in% c("DROP", "MEASUREMENT", "STRUCTURAL")) next
    v <- m[[f]]; tgt <- r$target
    if (r$scope == "csm") { csm[[tgt]] <- v; next }
    if (grepl("^geo_", tgt)) { geo[[sub("^geo_", "", tgt)]] <- v; next }
    if (grepl("^pub[0-9]+_", tgt)) {
      i <- as.character(as.integer(sub("^pub([0-9]+)_.*$", "\\1", tgt)))
      k <- sub("^pub[0-9]+_", "", tgt)
      if (is.null(pubs[[i]])) pubs[[i]] <- list()
      pubs[[i]][[k]] <- v; next
    }
    if (grepl("^paleoData_|^calibration_", tgt)) { colmeta[[tgt]] <- v; next }
    root[[tgt]] <- v
  }
  if (!is.null(m$depth)) {
    d <- n1(m$depth)
    if (is.null(d)) bad_depth <<- c(bad_depth, sprintf("%s: depth = %s", dsn, s1(m$depth)))
    else geo$elevation <- -d
  }
  list(root = root, geo = geo, pubs = pubs, csm = csm, colmeta = colmeta)
}

# The measurement columns the .mat supplies, grouped by their time axis. A
# core's variables do NOT share one: BO14HTI01 has d18O at 333 points and Sr/Ca
# at 4,861. Each distinct axis is its own measurement table.
series_of <- function(dsn, rec, colmeta) {
  m <- rec$meta
  groups <- list()
  sig_of <- function(y) paste0(length(y), ":", paste(utils::head(y, 2), collapse = ","),
                               ":", paste(utils::tail(y, 2), collapse = ","))
  put <- function(sig, e) {
    if (is.null(groups[[sig]])) groups[[sig]] <<- list(year = e$year, items = list())
    groups[[sig]]$items[[length(groups[[sig]]$items) + 1L]] <<- e
  }
  for (v in names(VARS)) {
    d <- rec$data[[v]]
    if (is.null(d) || is.null(d$year)) next
    spec <- VARS[[v]]; base <- sub("_annual$", "", v)
    y <- num_vec(d$year); vv <- num_vec(d$value)
    if (length(vv) != length(y)) {
      ragged <<- c(ragged, sprintf("%s/%s: %d values against %d years", dsn, spec$name, length(vv), length(y)))
      next
    }
    extra <- list(units = if (!is.null(spec$units)) spec$units else norm_units(m[[paste0("units_", base)]]),
                  variableType = "measured", proxy = spec$name,
                  analyticalError = n1(m[[paste0(base, "_errAnalytic")]]),
                  analyticalErrorUnits = norm_units(m[[paste0("units_", base, "_errAnalytic")]]))
    for (k in names(colmeta)) if (grepl("^paleoData_", k)) {
      kk <- sub("^paleoData_", "", k)
      if (!kk %in% c("units", "analyticalErrorUnits")) extra[[kk]] <- colmeta[[k]]
    }
    extra$hasResolution <- spec$res
    extra$inferredMaterial <- spec$material
    put(sig_of(y), list(year = y, name = spec$name, values = vv, extra = extra,
                        base = base, srcvar = v))
  }
  for (u in names(UNC)) {
    d <- rec$data[[u]]; pd <- rec$data[[UNC[[u]]]]
    if (is.null(d) || is.null(pd) || is.null(pd$year)) next
    y <- num_vec(pd$year); vv <- num_vec(d$value)
    if (length(vv) != length(y)) next
    put(sig_of(y), list(year = y, name = "uncertainty", values = vv,
                        extra = list(units = norm_units(m[[paste0("units_", UNC[[u]])]]),
                                     variableType = "measured", uncertaintyFor = VARS[[UNC[[u]]]]$name),
                        base = UNC[[u]], srcvar = UNC_TSID[[u]]))
  }
  groups
}

csm_entry <- function(csm, base, m) {
  if (!length(csm)) return(NULL)
  src_res <- switch(base %||% "", "d18O" = m$res_d18O, "SrCa" = m$res_SrCa,
                    "d18O_sw" = m$res_d18O_sw, NULL)
  e <- list(compilationName = COMP, compilationVersion = "2_0_3", csm = list())
  for (k in names(csm)) if (k != "nominalResolution") e$csm[[k]] <- csm[[k]]
  if (!is.null(src_res)) e$csm$nominalResolution <- src_res
  if (length(e$csm)) list(e) else NULL
}

table_cols <- function(tb) tb[!names(tb) %in% c("tableName", "filename", "missingValue", "columns")]

build <- function(dsn, rec) {
  r <- resolve(dsn, rec)
  m <- rec$meta
  groups <- series_of(dsn, rec, r$colmeta)
  if (!length(groups)) return(NULL)
  existing <- !is.na(ds_id[dsn])

  if (existing) {
    L <- tryCatch(suppressWarnings(suppressMessages(
      lipdR::readLipd(idx$datasets$path[match(dsn, idx$datasets$fileDataSetName)]))),
      error = function(e) NULL)
    if (is.null(L)) { failed <<- c(failed, paste0(dsn, ": could not read the existing file")); return(NULL) }
  } else {
    L <- list(dataSetName = dsn, datasetId = new_dsid(dsn), archiveType = "Coral",
              lipdVersion = 1.3, createdBy = COMP)
  }

  # metadata the .mat supplies, over whatever is there; nothing else is touched
  for (k in names(r$root)) L[[k]] <- r$root[[k]]
  if (length(r$geo)) { if (is.null(L$geo)) L$geo <- list()
                       for (k in names(r$geo)) L$geo[[k]] <- r$geo[[k]] }
  if (length(r$pubs)) {
    if (is.null(L$pub)) L$pub <- list()
    for (i in names(r$pubs)) { ii <- as.integer(i)
      if (length(L$pub) < ii) L$pub[[ii]] <- list()
      for (k in names(r$pubs[[i]])) L$pub[[ii]][[k]] <- r$pubs[[i]][[k]] }
  }
  if (!existing) L$archiveType <- "Coral"

  # ---- columns ----
  # <core>_<matVariable> is the identity the published files already use, and
  # it maps 1:1 onto the source. Matching on variableName instead was wrong:
  # DE13HAI01 has FOUR columns called d18O (coral, seawater, and the annual
  # average of each), so name-matching overwrote whichever came first and made
  # a vocabulary decision look like the values had been swapped.
  ts_for <- function(srcvar) {
    id <- paste0(dsn, "_", srcvar)
    if (id %in% known_ts) return(id)               # the existing column
    known_ts <<- c(known_ts, id); id               # or the same name, minted
  }

  old_tabs <- if (!is.null(L$paleoData[[1]]$measurementTable)) L$paleoData[[1]]$measurementTable else list()
  # index the existing columns by variableName so their metadata survives
  # keyed by TSid, so a dataset with four d18O columns keeps them apart
  old_col <- list()
  for (tb in old_tabs) for (nm in names(table_cols(tb))) {
    c0 <- tb[[nm]]
    if (is.list(c0) && !is.null(c0$TSid)) old_col[[as.character(c0$TSid)[1]]] <- c0
  }

  tables <- list()
  for (gi in seq_along(groups)) {
    g <- groups[[gi]]
    tb <- list(tableName = sprintf("paleo1measurement%d", gi))
    n <- 0L
    mk <- function(varname, values, extra, csm_base = NULL, tsid = NULL) {
      n <<- n + 1L
      prev <- if (!is.null(tsid)) old_col[[tsid]] else NULL
      col <- if (!is.null(prev)) prev else list()
      # start from what the file already holds, then apply what the .mat says
      for (k in names(extra)) if (!is.null(extra[[k]])) col[[k]] <- extra[[k]]
      col$variableName <- varname
      col$values <- values
      col$number <- n
      col$TSid <- tsid %||% new_tsid(dsn, paste0(varname, "_", n))
      if (is.null(col$createdBy)) col$createdBy <- COMP
      ce <- csm_entry(r$csm, csm_base, m)
      if (!is.null(ce)) col$inCompilation <- ce
      col
    }
    # The year column's TSid is random in the published files, so it cannot be
    # derived from the source variable. Take it from whichever old table holds
    # this group's first measurement -- that is the axis this data already sat
    # on -- and only mint when the column is genuinely new.
    yr_ts <- NULL
    first_ts <- paste0(dsn, "_", g$items[[1]]$srcvar)
    for (tb0 in old_tabs) {
      cs <- table_cols(tb0)
      has <- any(vapply(cs, function(c) is.list(c) && identical(as.character(c$TSid)[1], first_ts), logical(1)))
      if (!has) next
      for (c in cs) if (is.list(c) && identical(as.character(c$variableName)[1], "year")) {
        yr_ts <- as.character(c$TSid)[1]; break
      }
      if (!is.null(yr_ts)) break
    }
    if (is.null(yr_ts)) yr_ts <- new_tsid(dsn, paste0(g$items[[1]]$srcvar, "_year"))
    tb[["year"]] <- mk("year", g$year,
                       list(units = "yr AD", variableType = "inferred",
                            inferredVariableType = "year"), tsid = yr_ts)
    for (it in g$items) {
      nm <- it$name
      # a table can hold two columns of the same name only if they are
      # distinguishable; the source variable already is, so it disambiguates.
      if (nm %in% names(tb)) nm <- paste0(nm, "_", it$srcvar)
      tb[[nm]] <- mk(it$name, it$values, it$extra, it$base, tsid = ts_for(it$srcvar))
    }
    tables[[gi]] <- tb
  }
  if (is.null(L$paleoData)) L$paleoData <- list(list())
  L$paleoData[[1]]$measurementTable <- tables
  L
}

# ---- run -------------------------------------------------------------------

names_all <- names(ch$cores)
if (is.finite(lim)) names_all <- utils::head(names_all, lim)
n_new <- sum(is.na(ds_id[names_all])); n_upd <- length(names_all) - n_new
cat(sprintf("identity    : %d update existing datasets, %d are new\n", n_upd, n_new))
if (!write) cat("mode        : DRY RUN (nothing written)\n")

fs::dir_create(out)
built <- 0L; failed <- character(); reused_ts <- 0L; minted_ts <- 0L
bad_depth <- character()
ragged <- character()
for (dsn in names_all) {
  L <- tryCatch(build(dsn, ch$cores[[dsn]]), error = function(e) {
    failed <<- c(failed, sprintf("%s: %s", dsn, conditionMessage(e))); NULL })
  if (is.null(L)) next
  tb <- L$paleoData[[1]]$measurementTable[[1]]
  for (cl in tb[!names(tb) %in% c("tableName", "filename", "missingValue")]) {
    if (!is.list(cl) || is.null(cl$TSid)) next
    if (paste(dsn, cl$variableName) %in% names(ts_id)) reused_ts <- reused_ts + 1L else minted_ts <- minted_ts + 1L
  }
  if (write) lipdR::writeLipd(L, path = out, removeNamesFromLists = TRUE)
  built <- built + 1L
}

cat(sprintf("\nbuilt       : %d dataset%s\n", built, if (built == 1) "" else "s"))
cat(sprintf("TSids       : %d reused, %d minted\n", reused_ts, minted_ts))
if (length(bad_depth)) {
  cat(sprintf("no elevation: %d record%s whose depth is not a number\n", length(bad_depth),
              if (length(bad_depth) == 1) "" else "s"))
  for (b in bad_depth) cat("   ", b, "\n")
}
if (length(ragged)) {
  cat(sprintf("ragged      : %d column%s whose length does not match the year axis (not written)\n",
              length(ragged), if (length(ragged) == 1) "" else "s"))
  for (r in utils::head(ragged, 8)) cat("   ", r, "\n")
}
if (length(failed)) {
  cat(sprintf("failed      : %d\n", length(failed)))
  for (f in utils::head(failed, 10)) cat("   ", f, "\n")
}
if (write) cat(sprintf("written to  : %s\n", out)) else
  cat("\nnothing written; re-run with --write\n")
