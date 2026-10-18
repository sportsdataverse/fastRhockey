# The vectorised table extraction (.vhl_table_parts / .vhl_cell_columns) must return exactly what the
# per-row rvest implementation it replaced returned. REFERENCE below is that implementation, kept
# verbatim (renamed); the parity CSV tests in test-vhl.R cover the parsers built on top.

fx_dir <- testthat::test_path("fixtures", "vhl")
fx_html <- function(name) {
  paste(readLines(file.path(fx_dir, name), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

.ref_cell_column <- function(rows, j) {
  cj <- rvest::html_element(rows, xpath = sprintf("(td|th)[%d]", j))
  present <- !is.na(rvest::html_name(cj))
  n <- length(rows)
  blank <- function(x, fill) {
    out <- rep(fill, n)
    out[present] <- x
    out
  }
  cp <- cj[present]
  first_a <- rvest::html_element(cp, xpath = ".//a")
  linked <- !is.na(rvest::html_name(first_a))
  phref <- rep("", length(cp))
  if (any(linked)) {
    phref[linked] <- .vhl_attr(rvest::html_element(cp[linked], xpath = ".//a[contains(@href, '/players/')]"), "href")
  }
  style <- gsub(" ", "", tolower(.vhl_attr(cp, "style")), fixed = TRUE)
  colspan <- .vhl_attr(cp, "colspan")
  span <- suppressWarnings(as.integer(colspan))
  span <- pmax(ifelse(is.na(span), 1L, span), 1L)
  list(
    present = present,
    hidden = blank(grepl("display:none", style, fixed = TRUE), FALSE),
    tag = blank(rvest::html_name(cp), NA_character_),
    colspan = blank(colspan, ""),
    rowspan = blank(.vhl_attr(cp, "rowspan"), ""),
    span = blank(span, 1L),
    text = blank(.vhl_text(cp), NA_character_),
    href = blank(.vhl_attr(first_a, "href"), ""),
    phref = blank(phref, ""),
    has_link = blank(linked, FALSE)
  )
}

# (header cells, body rows) of a table: the header is the LAST <thead> row (else a leading
# all-<th> body row), the body the <tbody> rows. A cell is a record
# list(tag, colspan, rowspan, span, text, href (first link), phref (first /players/ link),
# has_link); display:none cells are dropped.
.ref_table_parts <- function(table) {
  head <- list()
  body <- list()
  trs <- rvest::html_elements(table, xpath = ".//tr")
  n <- length(trs)
  if (!n) return(list(head = head, body = body))
  has_body <- length(rvest::html_elements(table, xpath = ".//tbody")) > 0L
  sect <- rvest::html_name(rvest::html_element(trs, xpath = "parent::*"))
  columns <- list()
  repeat {
    col <- .ref_cell_column(trs, length(columns) + 1L)
    if (!any(col$present) || length(columns) >= 400L) break
    columns[[length(columns) + 1L]] <- col
  }
  for (i in seq_len(n)) {
    cells <- list()
    for (col in columns) {
      if (!col$present[[i]]) break
      if (col$hidden[[i]]) next
      cells[[length(cells) + 1L]] <- list(
        tag = col$tag[[i]], colspan = col$colspan[[i]], rowspan = col$rowspan[[i]],
        span = col$span[[i]], text = col$text[[i]],
        href = col$href[[i]], phref = col$phref[[i]], has_link = col$has_link[[i]]
      )
    }
    if (identical(sect[[i]], "thead")) {
      head <- cells
    } else if (identical(sect[[i]], "tbody") || (!has_body && !identical(sect[[i]], "tfoot"))) {
      if (!length(head) && length(cells) && all(vapply(cells, function(c) c$tag == "th", NA))) {
        head <- cells
      } else {
        body[length(body) + 1L] <- list(cells)
      }
    }
  }
  list(head = head, body = body)
}


all_tables <- function(html) .vhl_fa(.vhl_html(html), ".//table")

test_that("table parts equal the per-row reference on every table of every fixture page", {
  skip_on_cran()
  pages <- c("calendar_1374.html", "online_899223.html", "player_34418.html", "full/report_899223.html",
             "standings_1374.html", "stats_players_1374.html", "team_460279.html", "tournaments.html")
  seen <- 0L
  for (pg in pages) {
    for (tb in all_tables(fx_html(pg))) {
      expect_identical(.vhl_table_parts(tb), .ref_table_parts(tb), label = pg)
      seen <- seen + 1L
    }
  }
  expect_gt(seen, 10L)
})

test_that("table parts equal the reference on ragged, hidden, linked, block and nested cells", {
  skip_on_cran()
  html <- paste0(
    "<table><thead><tr><th>A</th><th colspan=2>B</th></tr></thead><tbody>",
    "<tr><td>1</td><td style='display: none'>x</td><td rowspan=2><a href='/players/7'>p</a></td></tr>",
    "<tr><td>2<br>two</td></tr><tr></tr>",
    "<tr><td><a href='/x'>q</a> <a href='/players/9'>r</a></td><td><div>d</div></td><td>z</td><td>extra</td></tr>",
    "</tbody><tfoot><tr><td>foot</td></tr></tfoot></table>"
  )
  nested <- "<table><tr><td>o1</td><td><table><tr><td>i1</td><td>i2</td></tr></table></td><td>o3</td></tr><tr><td>o4</td></tr></table>"
  for (h in c(html, nested)) {
    tb <- all_tables(h)[[1]]
    expect_identical(.vhl_table_parts(tb), .ref_table_parts(tb))
  }
  expect_identical(.vhl_table_parts(all_tables("<table></table>")[[1]]), list(head = list(), body = list()))
})
