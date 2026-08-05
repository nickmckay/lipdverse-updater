#' Review vocabulary decisions in a browser
#'
#' A local Shiny app over a review file from [lv_vocab_review()]. It exists
#' because the review is badly served by a spreadsheet: the context needed to
#' rule on a value is a paper, a PaST definition and a corner of the vocabulary,
#' none of which fit in a cell, and because the rows are far more repetitive than
#' they look. The hydroclimate2k batch is 87 rows but only nine distinct
#' decisions, 45 of them one seasonal-precipitation pattern applied over and
#' over.
#'
#' So values are grouped by the decision they would take, and a group is ruled on
#' once. Rows an agent left undecided are grouped by field and shown with
#' whatever it did find.
#'
#' Writes only the `decision` side of the file, the same columns you would fill
#' in by hand, and never applies anything. [lv_vocab_apply_review()] is still a
#' separate, deliberate step.
#'
#' @param path A review file from [lv_vocab_review()].
#' @param vocab From [lv_vocab()]; used to validate a hand-entered `map_to`.
#' @param launch Open a browser.
#' @param port Port to serve on.
#' @return With `launch = TRUE`, invisibly the path, after the browser session
#'   ends. With `launch = FALSE`, the Shiny app object, so the decision logic can
#'   be exercised with [shiny::testServer()].
#' @export
lv_vocab_review_app <- function(path, vocab = lv_vocab(), launch = TRUE, port = NULL) {
  for (p in c("shiny", "bslib", "DT")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      cli::cli_abort("{.pkg {p}} is needed for the reviewer. {.code install.packages(\"{p}\")}")
    }
  }
  path <- path.expand(path)
  if (!fs::file_exists(path)) cli::cli_abort("Review file not found: {.path {path}}")

  read_review <- function() {
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
                    na = "", progress = FALSE)
  }
  r0 <- read_review()
  need <- c("field", "value", "n", "proposed_decision", "decision")
  miss <- setdiff(need, names(r0))
  if (length(miss)) cli::cli_abort("Review file is missing {.field {miss}}.")

  DECISIONS <- c("synonym", "new_term", "decompose", "leave")

  # A group is one decision applied to many values. Undecided rows still group,
  # by field, so they can be worked through together rather than scattered.
  group_key <- function(r) {
    ifelse(is.na(r$proposed_decision) | !nzchar(r$proposed_decision),
           paste0("undecided · ", r$field),
           paste0(r$proposed_decision, " · ",
                  ifelse(is.na(r$proposed_map_to), "", r$proposed_map_to),
                  ifelse(is.na(r$proposed_also_field) | !nzchar(r$proposed_also_field), "",
                         paste0(" + ", r$proposed_also_field)),
                  " · ", r$field))
  }

  ui <- bslib::page_sidebar(
    title = paste0("Vocabulary review · ", fs::path_file(path)),
    theme = bslib::bs_theme(version = 5, preset = "shiny"),
    sidebar = bslib::sidebar(
      width = 340,
      shiny::uiOutput("progress"),
      shiny::hr(),
      shiny::uiOutput("grouplist"),
      shiny::hr(),
      shiny::actionButton("save", "Save to file", class = "btn-primary w-100"),
      shiny::div(class = "form-text mt-2",
                 "Writes the decision columns only. Applying is still a separate step."),
      shiny::verbatimTextOutput("saved", placeholder = FALSE)
    ),
    bslib::card(
      bslib::card_header(shiny::uiOutput("ghead")),
      bslib::card_body(
        shiny::uiOutput("evidence"),
        shiny::hr(),
        shiny::h6("Values in this group"),
        DT::DTOutput("members"),
        shiny::hr(),
        shiny::h6("Decision"),
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput("dec", "decision",
                                              c("(leave blank)" = "", DECISIONS))),
          shiny::column(3, shiny::textInput("map", "map_to")),
          shiny::column(3, shiny::textInput("af", "also_field")),
          shiny::column(3, shiny::textInput("av", "also_value"))),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput("pn", "past_name")),
          shiny::column(2, shiny::textInput("pid", "past_id")),
          shiny::column(6, shiny::textInput("note", "note"))),
        shiny::uiOutput("validity"),
        shiny::div(
          class = "d-flex gap-2 mt-2",
          shiny::actionButton("accept", "Accept group", class = "btn-success"),
          shiny::actionButton("accept_sel", "Accept selected rows"),
          shiny::actionButton("leave", "Mark all `leave`"),
          shiny::actionButton("clear", "Clear group"),
          shiny::actionButton("nxt", "Next →", class = "ms-auto"))
      )
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(r = r0, gi = 1L, msg = "")

    groups <- shiny::reactive({
      g <- group_key(rv$r)
      u <- unique(g)
      # Biggest first: the repetitive families are where the time goes.
      u[order(-vapply(u, function(k) sum(as.integer(rv$r$n[g == k])), numeric(1)))]
    })
    cur_key <- shiny::reactive({
      gs <- groups(); gs[max(1L, min(rv$gi, length(gs)))]
    })
    cur_idx <- shiny::reactive(which(group_key(rv$r) == cur_key()))

    decided <- function(r) !is.na(r$decision) & nzchar(r$decision)

    output$progress <- shiny::renderUI({
      r <- rv$r
      shiny::tagList(
        shiny::div(sprintf("%d of %d values decided", sum(decided(r)), nrow(r))),
        shiny::div(class = "text-muted small",
                   sprintf("%d of %d occurrences",
                           sum(as.integer(r$n[decided(r)])), sum(as.integer(r$n)))),
        shiny::div(class = "progress mt-2", style = "height:6px",
                   shiny::div(class = "progress-bar", role = "progressbar",
                              style = sprintf("width:%.0f%%", 100 * mean(decided(r))))))
    })

    output$grouplist <- shiny::renderUI({
      gs <- groups(); g <- group_key(rv$r)
      shiny::tagList(lapply(seq_along(gs), function(i) {
        idx <- which(g == gs[i])
        done <- sum(decided(rv$r)[idx]); tot <- length(idx)
        shiny::actionLink(
          paste0("go_", i), class = "d-block py-1 text-decoration-none",
          shiny::span(
            shiny::span(class = if (done == tot) "text-success" else "text-muted",
                        if (done == tot) "● " else "○ "),
            shiny::span(style = if (i == rv$gi) "font-weight:600" else "", gs[i]),
            shiny::span(class = "text-muted small", sprintf(" (%d/%d)", done, tot))))
      }))
    })
    shiny::observe({
      lapply(seq_along(groups()), function(i) {
        shiny::observeEvent(input[[paste0("go_", i)]], { rv$gi <- i }, ignoreInit = TRUE)
      })
    })

    output$ghead <- shiny::renderUI({
      idx <- cur_idx()
      shiny::tagList(
        shiny::strong(cur_key()),
        shiny::span(class = "text-muted",
                    sprintf("  — %d value%s, %d occurrence%s", length(idx),
                            if (length(idx) == 1) "" else "s",
                            sum(as.integer(rv$r$n[idx])),
                            if (sum(as.integer(rv$r$n[idx])) == 1) "" else "s")))
    })

    output$evidence <- shiny::renderUI({
      idx <- cur_idx(); r <- rv$r[idx, , drop = FALSE]
      fld <- function(nm) if (nm %in% names(r)) unique(stats::na.omit(r[[nm]])) else character()
      item <- function(lab, x, pre = FALSE) {
        if (!length(x) || !any(nzchar(x))) return(NULL)
        shiny::div(class = "mb-2",
                   shiny::div(class = "text-muted small text-uppercase", lab),
                   if (pre) shiny::tags$code(paste(x, collapse = "  |  "))
                   else shiny::div(paste(utils::head(x, 6), collapse = "  |  ")))
      }
      pdfs <- unique(unlist(strsplit(stats::na.omit(r$source_pdf), " \\| ")))
      shiny::tagList(
        item("rationale", fld("rationale")),
        item("vocabulary candidates", fld("candidates"), pre = TRUE),
        item("PaST candidates", fld("past_candidates"), pre = TRUE),
        item("datasets", unique(unlist(strsplit(stats::na.omit(r$datasets), " \\| ")))),
        if (length(pdfs)) shiny::div(
          class = "mb-2",
          shiny::div(class = "text-muted small text-uppercase", "papers"),
          shiny::div(class = "d-flex flex-wrap gap-2",
                     lapply(seq_along(pdfs), function(i) {
                       shiny::actionButton(paste0("pdf_", i), fs::path_file(pdfs[i]),
                                           class = "btn-sm btn-outline-secondary")
                     })))
      )
    })
    # Opening the PDF is a server-side shell call, which is fine and only fine
    # because this app is local by construction.
    shiny::observe({
      idx <- cur_idx()
      pdfs <- unique(unlist(strsplit(stats::na.omit(rv$r$source_pdf[idx]), " \\| ")))
      lapply(seq_along(pdfs), function(i) {
        shiny::observeEvent(input[[paste0("pdf_", i)]], {
          try(system2(if (Sys.info()[["sysname"]] == "Darwin") "open" else "xdg-open",
                      shQuote(pdfs[i])), silent = TRUE)
        }, ignoreInit = TRUE)
      })
    })

    output$members <- DT::renderDT({
      idx <- cur_idx()
      cols <- intersect(c("value", "n", "proposed_also_value", "decision", "map_to",
                          "also_value", "example"), names(rv$r))
      DT::datatable(rv$r[idx, cols, drop = FALSE], rownames = FALSE,
                    selection = "multiple",
                    options = list(pageLength = 12, dom = "tp", scrollX = TRUE))
    }, server = FALSE)

    # Seed the inputs from the group's proposal whenever the group changes.
    shiny::observeEvent(cur_key(), {
      r <- rv$r[cur_idx(), , drop = FALSE]
      one <- function(nm) {
        x <- if (nm %in% names(r)) unique(stats::na.omit(r[[nm]])) else character()
        if (length(x) == 1) x else ""
      }
      shiny::updateSelectInput(session, "dec", selected = one("proposed_decision"))
      shiny::updateTextInput(session, "map", value = one("proposed_map_to"))
      shiny::updateTextInput(session, "af", value = one("proposed_also_field"))
      shiny::updateTextInput(session, "av", value = one("proposed_also_value"))
      shiny::updateTextInput(session, "pn", value = one("proposed_past_name"))
      shiny::updateTextInput(session, "pid", value = one("proposed_past_id"))
      shiny::updateTextInput(session, "note", value = "")
    })

    # The same checks lv_vocab_apply_review() will run, shown before you commit
    # rather than as an error afterwards.
    problems <- shiny::reactive({
      d <- input$dec; if (!nzchar(d)) return(character())
      p <- character()
      fld <- unique(rv$r$field[cur_idx()])[1]
      if (d %in% c("synonym", "decompose") && !nzchar(input$map))
        p <- c(p, "map_to is required")
      if (nzchar(input$map) && !is.null(vocab[[fld]]) &&
          !input$map %in% vocab[[fld]]$lipdName)
        p <- c(p, sprintf("map_to '%s' is not a lipdName in %s", input$map, fld))
      if (d == "decompose" && (!nzchar(input$af) || !nzchar(input$av)))
        p <- c(p, "decompose needs also_field and also_value")
      if (d == "decompose" && nzchar(input$af) && grepl("^interpretation_", input$af)) {
        key <- input$af
        if (!is.null(vocab[[key]]) && nzchar(input$av) &&
            !vocab_standardize(input$av, key, vocab)$matched)
          p <- c(p, sprintf("also_value '%s' is not in the %s vocabulary", input$av, key))
      }
      p
    })
    output$validity <- shiny::renderUI({
      p <- problems()
      if (!length(p)) return(NULL)
      shiny::div(class = "alert alert-warning py-2 my-2",
                 shiny::tags$ul(class = "mb-0", lapply(p, shiny::tags$li)))
    })

    write_rows <- function(idx, per_row_also = FALSE) {
      if (!length(idx)) return()
      r <- rv$r
      r$decision[idx] <- if (nzchar(input$dec)) input$dec else NA_character_
      r$map_to[idx]   <- if (nzchar(input$map)) input$map else NA_character_
      r$also_field[idx] <- if (nzchar(input$af)) input$af else NA_character_
      # The season differs per value inside one group, so take each row's own
      # proposed_also_value unless the box was edited to something specific.
      r$also_value[idx] <- if (per_row_also && "proposed_also_value" %in% names(r))
        r$proposed_also_value[idx] else if (nzchar(input$av)) input$av else NA_character_
      if ("past_name" %in% names(r)) r$past_name[idx] <- if (nzchar(input$pn)) input$pn else NA_character_
      if ("past_id" %in% names(r))   r$past_id[idx]   <- if (nzchar(input$pid)) input$pid else NA_character_
      if ("note" %in% names(r) && nzchar(input$note)) r$note[idx] <- input$note
      rv$r <- r
    }

    shiny::observeEvent(input$accept, {
      idx <- cur_idx()
      # If the group's rows carry differing also_values, keep each row's own.
      pv <- if ("proposed_also_value" %in% names(rv$r)) rv$r$proposed_also_value[idx] else NA
      write_rows(idx, per_row_also = length(unique(stats::na.omit(pv))) > 1)
      rv$msg <- sprintf("%d value%s set", length(idx), if (length(idx) == 1) "" else "s")
    })
    shiny::observeEvent(input$accept_sel, {
      sel <- input$members_rows_selected
      if (!length(sel)) { rv$msg <- "no rows selected"; return() }
      idx <- cur_idx()[sel]
      pv <- if ("proposed_also_value" %in% names(rv$r)) rv$r$proposed_also_value[idx] else NA
      write_rows(idx, per_row_also = length(unique(stats::na.omit(pv))) > 1)
      rv$msg <- sprintf("%d selected row%s set", length(idx), if (length(idx) == 1) "" else "s")
    })
    shiny::observeEvent(input$leave, {
      idx <- cur_idx(); r <- rv$r
      r$decision[idx] <- "leave"
      r$map_to[idx] <- NA_character_; r$also_field[idx] <- NA_character_
      r$also_value[idx] <- NA_character_
      rv$r <- r; rv$msg <- sprintf("%d marked leave", length(idx))
    })
    shiny::observeEvent(input$clear, {
      idx <- cur_idx(); r <- rv$r
      for (nm in c("decision", "map_to", "also_field", "also_value", "past_name", "past_id"))
        if (nm %in% names(r)) r[[nm]][idx] <- NA_character_
      rv$r <- r; rv$msg <- "cleared"
    })
    shiny::observeEvent(input$nxt, {
      rv$gi <- if (rv$gi >= length(groups())) 1L else rv$gi + 1L
    })

    shiny::observeEvent(input$save, {
      readr::write_csv(rv$r, path, na = "")
      rv$msg <- sprintf("saved %s", format(Sys.time(), "%H:%M:%S"))
    })
    output$saved <- shiny::renderText(rv$msg)

    session$onSessionEnded(function() shiny::stopApp())
  }

  cli::cli_alert_info("Reviewing {.path {path}}")
  cli::cli_alert_info("Decisions are written only when you press {.strong Save to file}.")
  app <- shiny::shinyApp(ui, server)
  if (!launch) return(app)
  shiny::runApp(app, launch.browser = TRUE, port = port, quiet = TRUE)
  invisible(path)
}
