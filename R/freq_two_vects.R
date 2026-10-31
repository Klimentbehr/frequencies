#' freq_two_vects
#'
#' freq_two_vects takes two columns from a data frame and returns a data frame containing frequency
#'    tables with counts and percentages of col2 within col1.
#'    It answers the question, what percent of col1 is col2.
#'
#'   As an example, if col1 was gender and col2 was ethnicity you would get the count and rate of
#'   males that are asian, african american, hispanic, etc.
#'
#'   If col1 was ethnicity and col2 was gender you would get the count and rate of asians that are
#'   male and female.
#'
#'   Only combinations that occur in the data are returned (no zero-count rows), and missing values
#'   are treated as their own category.
#'
#' @param df   a data frame
#' @param col1   a column from the data frame to be aggregated at the higher level.
#' @param col2   a column from the data frame to be aggregated within col1.
#' @param separate_tables a boolean that decides if you want all aggregations in one data frame
#'   or split apart so each element of col1 gets it's own table; the default is FALSE
#'
#' @return returns a data frame (or a list of data frames if separate_tables is TRUE) split by
#'   col1 with counts and rates of col2.
#' @export
#'
#' @examples
#' # Sample data frame to demo the freq_two_vects function.
#' df <- data.frame(gender = sample(c('m','f'), 200, replace = TRUE),
#'                  ethnicity = sample(c('african american', 'asian', 'caucasian',
#'                                    'hispanic', 'other'),
#'                                    200, replace = TRUE),
#'                  stringsAsFactors = FALSE)
#'
#' freq_two_vects(df, gender, ethnicity, FALSE)
#' gender_by_ethnicity <- freq_two_vects(df, gender, ethnicity, TRUE)
#' gender_by_ethnicity$m
#' freq_two_vects(df, gender, ethnicity, TRUE)$m
#' freq_two_vects(df, ethnicity, gender, FALSE)
#' ethnicity_by_gender <- freq_two_vects(df, ethnicity, gender, TRUE)
#' ethnicity_by_gender$asian

freq_two_vects <- function(df, col1, col2, separate_tables = FALSE){

  if (!is.data.frame(df)) {
    stop('df must be a data frame, not an object of class "', class(df)[1], '".', call. = FALSE)
  }

  # Column names may be given bare (gender) or quoted ("gender").
  col1 <- gsub('^["\']|["\']$', '', deparse(substitute(col1)))
  col2 <- gsub('^["\']|["\']$', '', deparse(substitute(col2)))
  if (!(col1 %in% names(df))) stop(col1, ' not in df.', call. = FALSE)
  if (!(col2 %in% names(df))) stop(col2, ' not in df.', call. = FALSE)
  if (identical(col1, col2)) stop('col1 and col2 must be different columns.', call. = FALSE)

  a <- df[[col1]]
  b <- df[[col2]]
  if (!is.atomic(a)) stop(col1, ' is not an atomic vector.', call. = FALSE)
  if (!is.atomic(b)) stop(col2, ' is not an atomic vector.', call. = FALSE)

  check_flag(separate_tables, 'separate_tables')

  # Encode each value as its position among the uniques, combine the two codes into one key,
  # and tabulate the keys. Work is O(n) and memory is proportional to the number of
  # combinations that actually occur (xtabs allocated every possible cell).
  ua <- unique(a)
  ub <- unique(b)
  ia <- match_incl_na(a, ua)
  ib <- match_incl_na(b, ub)
  key <- ia + (ib - 1) * length(ua)            # double, so no integer overflow

  ukey  <- unique(key)
  first <- match(ukey, key)                    # a representative row for each combination
  n     <- tabulate(match(key, ukey), nbins = length(ukey))

  v1 <- a[first]
  v2 <- b[first]
  group_total <- tabulate(ia, nbins = length(ua))[ia[first]]

  result <- data.frame(v1, v2, n, round(n * 100 / group_total, 1), stringsAsFactors = FALSE)
  colnames(result) <- c(col1, col2, 'Count', 'Percentage')

  # Order by col1, then largest Count first, then col2 to break ties deterministically.
  result <- result[order(v1, -n, v2), , drop = FALSE]
  rownames(result) <- NULL

  if (separate_tables) {
    tables <- split(result, result[[col1]])
    return(lapply(tables, function(d) { rownames(d) <- NULL; d }))
  }
  result
}
