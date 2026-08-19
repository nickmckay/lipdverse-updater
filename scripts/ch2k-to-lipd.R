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
VARS <- list(
  d18O           = list(name = "d18O",   units = "permil",  res = NULL),
  SrCa           = list(name = "Sr/Ca",  units = NULL,      res = NULL),
  d18O_sw        = list(name = "d18O_sw", units = "permil", res = NULL),
  d18O_annual    = list(name = "d18O",   units = "permil",  res = "annual"),
  SrCa_annual    = list(name = "Sr/Ca",  units = NULL,      res = "annual"),
  d18O_sw_annual = list(name = "d18O_sw", units = "permil", res = "annual"))
UNC <- list(d18O_err = "d18O", SrCa_err = "SrCa")

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

# ---- build one dataset -----------------------------------------------------

build <- function(dsn, rec) {
  m <- rec$meta
  existing <- !is.na(ds_id[dsn])
  L <- list(dataSetName = dsn,
            datasetId   = if (existing) unname(ds_id[dsn]) else new_dsid(dsn),
            archiveType = "Coral",
            lipdVersion = 1.3,
            createdBy   = COMP)

  geo <- list(); pubs <- list(); csm <- list(); colmeta <- list()

  for (f in names(m)) {
    r <- rule(f)
    if (is.null(r) || is.null(r$target) || r$scope %in% c("DROP", "MEASUREMENT", "STRUCTURAL")) next
    v <- m[[f]]
    tgt <- r$target

    if (r$scope == "csm") { csm[[tgt]] <- v; next }
    if (grepl("^geo_", tgt)) { geo[[sub("^geo_", "", tgt)]] <- v; next }
    if (grepl("^pub[0-9]+_", tgt)) {
      i <- as.integer(sub("^pub([0-9]+)_.*$", "\\1", tgt))
      k <- sub("^pub[0-9]+_", "", tgt)
      if (is.null(pubs[[as.character(i)]])) pubs[[as.character(i)]] <- list()
      pubs[[as.character(i)]][[k]] <- v
      next
    }
    if (grepl("^paleoData_|^calibration_", tgt)) { colmeta[[tgt]] <- v; next }
    L[[tgt]] <- v
  }

  # depth is metres below sea level; the published files carry it negated as
  # elevation (verified on 146 records, no disagreement).
  #
  # n1() rather than a bare negation: ZI23NFL01 carries depth = "10/05/2024", a
  # date typed into a depth field. Refusing it here keeps a nonsense elevation
  # out of the database and puts the record on the report instead of aborting
  # the run over one bad cell.
  if (!is.null(m$depth)) {
    d <- n1(m$depth)
    if (is.null(d)) {
      bad_depth <<- c(bad_depth, sprintf("%s: depth = %s", dsn, s1(m$depth)))
    } else {
      geo$elevation <- -d
    }
  }
  if (length(geo)) L$geo <- geo
  if (length(pubs)) {
    L$pub <- lapply(sort(as.integer(names(pubs))), function(i) pubs[[as.character(i)]])
  }

  # ---- columns, grouped by their time axis ----------------------------------
  #
  # A core's variables do NOT share one axis. BO14HTI01 has d18O at 333 points
  # and Sr/Ca at 4,861; FE09OGA01 has monthly d18O at 737 and its annual average
  # at 122. Forcing them into one table refused 39 columns as ragged -- the data
  # was fine, the assumption was not. Each distinct axis gets its own
  # measurement table with its own year column, which is what convert.m was
  # doing with paleoData_tableNumber.

  # collect the variables that have data, keyed by the axis they sit on
  groups <- list()
  put <- function(sig, entry) {
    k <- sig
    if (is.null(groups[[k]])) groups[[k]] <<- list(year = entry$year, items = list())
    groups[[k]]$items[[length(groups[[k]]$items) + 1L]] <<- entry
  }
  sig_of <- function(y) paste0(length(y), ":", paste(utils::head(y, 2), collapse=","),
                               ":", paste(utils::tail(y, 2), collapse=","))

  for (v in names(VARS)) {
    d <- rec$data[[v]]
    if (is.null(d) || is.null(d$year)) next
    spec <- VARS[[v]]
    y <- num_vec(d$year); vv <- num_vec(d$value)
    if (length(vv) != length(y)) {
      ragged <<- c(ragged, sprintf("%s/%s: %d values against %d years", dsn, spec$name, length(vv), length(y)))
      next
    }
    base <- sub("_annual$", "", v)
    extra <- list(units = if (!is.null(spec$units)) spec$units else norm_units(m[[paste0("units_", base)]]),
                  variableType = "measured", proxy = spec$name,
                  analyticalError = n1(m[[paste0(base, "_errAnalytic")]]),
                  analyticalErrorUnits = norm_units(m[[paste0("units_", base, "_errAnalytic")]]))
    # Sheet-mapped column metadata first, so the normalised units above are not
    # overwritten by the raw per-mille sign that units_d18O carries.
    for (k in names(colmeta)) if (grepl("^paleoData_", k)) {
      kk <- sub("^paleoData_", "", k)
      if (!kk %in% c("units", "analyticalErrorUnits")) extra[[kk]] <- colmeta[[k]]
    }
    put(sig_of(y), list(year = y, name = spec$name, values = vv, extra = extra, base = base))
  }

  # uncertainty vectors ride with their parent, on the parent's axis
  for (u in names(UNC)) {
    d <- rec$data[[u]]; pd <- rec$data[[UNC[[u]]]]
    if (is.null(d) || is.null(pd) || is.null(pd$year)) next
    y <- num_vec(pd$year); vv <- num_vec(d$value)
    if (length(vv) != length(y)) {
      ragged <<- c(ragged, sprintf("%s/%s uncertainty: %d values against %d years", dsn, UNC[[u]], length(vv), length(y)))
      next
    }
    parent <- VARS[[UNC[[u]]]]$name
    put(sig_of(y), list(year = y, name = paste0(parent, "_uncertainty"), values = vv,
                        extra = list(units = norm_units(m[[paste0("units_", UNC[[u]])]]),
                                     variableType = "measured", uncertaintyFor = parent),
                        base = UNC[[u]]))
  }
  if (!length(groups)) return(NULL)

  # A TSid may be reused ONCE. A core can hold the same variableName on two
  # axes -- FE09OGA01 has monthly d18O and its annual average, both called
  # "d18O" -- and keying the lookup on (dataSetName, variableName) alone handed
  # both columns the same id. 22 of 227 datasets came out with duplicate TSids,
  # which would corrupt a merge that is keyed on nothing else. The first column
  # to claim a name keeps the existing id; any later one is minted.
  used <- character()
  mk_col <- function(varname, values, n, extra = list()) {
    key <- paste(dsn, varname)
    tsid <- if (!is.na(ts_id[key]) && !ts_id[key] %in% used) unname(ts_id[key])
            else new_tsid(dsn, paste0(varname, "_", n))
    used <<- c(used, tsid)
    col <- c(list(number = n, variableName = varname, TSid = tsid,
                  values = values, createdBy = COMP), extra)
    col[!vapply(col, is.null, logical(1))]
  }

  tables <- list()
  for (gi in seq_along(groups)) {
    g <- groups[[gi]]
    tb <- list(tableName = sprintf("paleo1measurement%d", gi))
    n <- 0L
    n <- n + 1L
    tb[["year"]] <- mk_col("year", g$year, n,
                           list(units = "yr AD", variableType = "inferred",
                                inferredVariableType = "year"))
    for (it in g$items) {
      n <- n + 1L
      col <- mk_col(it$name, it$values, n, it$extra)
      # csm on the measurements, never on the axis. nominalResolution is per
      # variable in the source (res_d18O, res_SrCa, res_d18O_sw).
      if (length(csm)) {
        src_res <- switch(it$base, "d18O" = m$res_d18O, "SrCa" = m$res_SrCa,
                          "d18O_sw" = m$res_d18O_sw, NULL)
        entry <- list(compilationName = COMP, compilationVersion = "2_0_3", csm = list())
        for (k in names(csm)) if (k != "nominalResolution") entry$csm[[k]] <- csm[[k]]
        if (!is.null(src_res)) entry$csm$nominalResolution <- src_res
        if (length(entry$csm)) col$inCompilation <- list(entry)
      }
      tb[[it$name]] <- col
    }
    tables[[gi]] <- tb
  }

  # Columns are NAMED MEMBERS of the table, not a `columns` list. That is the
  # layout every published CoralHydro2k file uses, and it is not cosmetic:
  # writing them under `columns` produced a .lpd with no CSV member and a single
  # column on read -- metadata intact, all 1,677 values gone.
  L$paleoData <- list(list(measurementTable = tables))
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
