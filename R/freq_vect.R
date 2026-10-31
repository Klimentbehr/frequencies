#' freq_vect
#'
#' freq_vect returns a frequency table with counts and percentages of values
#'   from an atomic vector of type character, integer, double, or logical
#'
#' @param data_vector   an atomic vector of type character, integer, double, or logical
#' @param sort_by_count   boolean value that determines if output will be sorted by count or name
#' @param total_row   a boolean value that determines if the output will have a summary row appended.
#'   The summary row's first column is the character string "Total"; the Count, Percentage and
#'   Cum. columns remain numeric.
#' @param include_na   a boolean value that determines if missing values (NA) are shown as their
#'   own row (the default) or dropped before percentages are computed.
#'
#' @return a data.frame containing the counts and percentages of each value from the provided data.
#'   Cum. is calculated from the unrounded counts so it always ends at exactly 100.
#' @export
#'
#' @examples
#' # Sample vector and data frame to demo the freq_vect function.
#' numbers <- sample(1:10, 200, replace = TRUE)
#' tbl     <- data.frame(numbers = sample(1:10, 200, replace = TRUE),
#'   letters = sample(letters, 200, replace = TRUE),
#'   dates = sample(seq(as.Date('1999/10/01'), as.Date('2000/01/01'), by="day"),
#'                  200, replace = TRUE),
#'   logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
#'   stringsAsFactors = FALSE)
#'
#' freq_vect(numbers)
#' freq_vect(tbl$numbers)
#' freq_vect(tbl$letters, sort_by_count = TRUE, total_row = FALSE)
#' freq_vect(mtcars$cyl)

freq_vect <- function(data_vector, sort_by_count = FALSE, total_row = TRUE, include_na = TRUE) {

  if (!is.atomic(data_vector)) stop('freq_vect requires an atomic vector.')

  if (!(typeof(data_vector) %in% c('logical', 'integer', 'double', 'character'))) {
    stop('Vector not of acceptable data type. Needs to be of type logical, integer, double, or character.')
  }

  if (length(data_vector) < 1) stop('Vector needs a length of at least 1.')

  check_flag(sort_by_count, 'sort_by_count')
  check_flag(total_row, 'total_row')
  check_flag(include_na, 'include_na')

  if (!include_na) {
    data_vector <- data_vector[!is.na(data_vector)]
    if (length(data_vector) < 1) stop('Vector contains only missing values.')
  }

  # Single pass over the data: unique values, then a tabulate of their positions.
  # This is O(n), keeps the original type (dates, ints, ...) and never builds a
  # cross-tab matrix like xtabs/table would. match() does not treat two NAs as
  # equal, so missing values need a dedicated index or they disappear.
  values <- unique(data_vector)
  n      <- tabulate(match_incl_na(data_vector, values), nbins = length(values))
  total  <- sum(n)

  ord <- if (sort_by_count) order(-n, values) else order(values)
  values <- values[ord]
  n      <- n[ord]

  result <- data.frame(data       = values,
                       Count      = n,
                       Percentage = round(n * 100 / total, 1),
                       Cum.       = round(cumsum(n) * 100 / total, 1),
                       stringsAsFactors = FALSE)

  if (total_row) {
    result$data <- as.character(result$data)
    result <- rbind(result,
                    data.frame(data = 'Total', Count = total, Percentage = 100, Cum. = 100,
                               stringsAsFactors = FALSE))
  }

  rownames(result) <- NULL
  result
}

# Shared argument validation: a single non-missing TRUE/FALSE.
check_flag <- function(x, name) {
  if (!(is.logical(x) && length(x) == 1L && !is.na(x))) {
    stop(name, ' was not logical. Need TRUE or FALSE', call. = FALSE)
  }
  invisible(TRUE)
}

# match() returns NA for NA, so tabulate() would drop missing values. Point those
# rows at the NA slot in `table` instead.
match_incl_na <- function(x, table) {
  idx <- match(x, table)
  na_level <- which(is.na(table))[1L]
  if (!is.na(na_level)) idx[is.na(x)] <- na_level
  idx
}
