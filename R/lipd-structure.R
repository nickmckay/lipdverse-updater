#' How a LiPD file is organised, and why
#'
#' A convention rather than a rule, but follow it wherever the data allows.
#' Stated by Nick 2026-08-19 while converting CoralHydro2k v2.0.3, after a
#' conversion had to decide whether to preserve an existing layout or rebuild
#' it.
#'
#' @section One physical object, one data object:
#' Every physical thing that was measured gets its own `paleoData` object (or
#' `chronData` for a chronology). Two cores from the same reef are two objects;
#' one core is one object however many things were measured on it. The question
#' to ask is not "is this a different variable" but "is this a different piece
#' of material".
#'
#' @section One sampling, one table:
#' Within an object, variables measured on the *same samples* belong in the
#' *same table*, because they share an axis: one row is one sample, and the
#' columns are what was measured on it. Variables measured at a different
#' resolution -- a coarser sub-sampling, an annual average of a monthly series
#' -- get their own table, because their rows are different samples.
#'
#' So the axis is the test. Columns that share a time or depth axis share a
#' table; columns that do not, cannot, because a table has one row set.
#'
#' @section What this rules out:
#' A table per variable. It is a common shape in files converted from formats
#' that had no concept of a table, and it is wrong in a specific way: it
#' duplicates the axis. `DE13HAI01` arrived with six measurement tables whose
#' `year` columns were byte-identical -- 159 values, same sum, six TSids for one
#' sampling. Nothing is gained and the redundant axes have to be kept
#' consistent forever.
#'
#' Consolidating such a file drops the duplicate axis columns and their TSids.
#' That is correct: they identified nothing that the surviving axis does not.
#' It is also the one case where an update legitimately removes a TSid the
#' database holds, so expect the write gate to report it.
#'
#' @section Applied to CoralHydro2k:
#' One core is one object. `d18O` and `Sr/Ca` measured on the same monthly
#' samples share a table; the annual averages of both share a second table,
#' because annual means are a different sampling. Seawater d18O is derived from
#' the same samples as the coral d18O, so it sits with it -- distinguished by
#' `inferredMaterial`, not by living somewhere else. The result is 195 datasets
#' with one table and 32 with two, and no dataset where one sampling was split
#' across tables.
#'
#' @name lipd_structure
#' @keywords internal
NULL
