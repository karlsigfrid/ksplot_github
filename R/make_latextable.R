#' Convert a data frame to a LaTeX table
#'
#' Formats a data frame as a simple LaTeX table using booktabs rules
#' (\code{\\toprule}, \code{\\midrule}, \code{\\bottomrule}), wrapped in a
#' centered \code{table} environment. Numeric columns are right-aligned and
#' all other columns are left-aligned. LaTeX special characters in text cells
#' and column names are escaped, missing values become empty cells, and row
#' names are dropped.
#'
#' @param df A data frame.
#' @param digits Number of decimals to round non-integer numeric columns to.
#'   Numbers are shown with exactly this many decimals, so trailing zeros are
#'   kept. Integer columns are not affected. Use \code{-1} (the default) for
#'   no rounding.
#' @param filename Path of a file to write the table to. If \code{NULL} (the
#'   default), no file is written.
#' @param silent If \code{TRUE}, the function returns nothing (invisible
#'   \code{NULL}), which is useful when only the file output is needed.
#'   Defaults to \code{FALSE}.
#'
#' @return A character vector with one element per line of LaTeX code,
#'   suitable for \code{writeLines()}, or invisible \code{NULL} if
#'   \code{silent = TRUE}.
#'
#' @note The LaTeX document needs \code{\\usepackage{booktabs}} in its
#'   preamble.
#'
#' @examples
#' df <- data.frame(group = c("A", "B"), mean = c(1.2345, 2.5), n = c(10L, 12L))
#' writeLines(make_latextable(df, digits = 2))
#'
#' \dontrun{
#' make_latextable(df, digits = 2, filename = "table.tex", silent = TRUE)
#' }
#'
#' @export
make_latextable <- function(df, digits = -1, filename = NULL, silent = FALSE) {
  stopifnot(is.data.frame(df))

  # Escape LaTeX special characters in text cells and column names
  escape_latex <- function(x) {
    x <- gsub("\\", "\001", x, fixed = TRUE)           # placeholder for backslash
    x <- gsub("([&%$#_{}])", "\\\\\\1", x)
    x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
    gsub("\001", "\\textbackslash{}", x, fixed = TRUE)
  }

  # Format one column as character
  format_col <- function(x) {
    if (is.numeric(x) && !is.integer(x) && digits >= 0) {
      out <- formatC(x, format = "f", digits = digits)
    } else {
      out <- as.character(x)
    }
    out[is.na(x)] <- ""
    if (is.numeric(x)) out else escape_latex(out)
  }

  # Numeric columns right-aligned, everything else left-aligned
  align <- paste(ifelse(vapply(df, is.numeric, logical(1)), "r", "l"),
                 collapse = "")

  header <- paste(escape_latex(names(df)), collapse = " & ")
  cells  <- unname(lapply(df, format_col))
  body   <- do.call(paste, c(cells, sep = " & "))

  lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\begin{tabular}{", align, "}"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    if (length(body) > 0) paste0(body, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )

  if (!is.null(filename)) writeLines(lines, filename)

  if (silent) invisible(NULL) else lines
}
