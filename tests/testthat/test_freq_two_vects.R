library(frequencies)
# Golden values were generated with the pre-R-3.6 sampler; pin it so they stay reproducible.
suppressWarnings(set.seed(1, sample.kind = 'Rounding'))
tbl <<- data.frame(numbers = sample(1:3, 200, replace = TRUE),
                   letters = sample(letters[1:3], 200, replace = TRUE),
                   dates = sample(seq(as.Date('1999/10/01'), as.Date('1999/10/03'), by = "day"),
                                  200, replace = TRUE),
                   logicals = sample(c(TRUE, FALSE), 200, replace = TRUE),
                   stringsAsFactors = FALSE)

test_that("invalid input is rejected with clear errors", {
  expect_error(freq_two_vects(nonexistant, nonexistant), "nonexistant")
  expect_error(freq_two_vects(tbl, nonexistant, numbers), "nonexistant not in df.")
  expect_error(freq_two_vects(tbl, numbers, nonexistant), "nonexistant not in df.")
  expect_error(freq_two_vects('tbl', numbers, letters), 'df must be a data frame, not an object of class "character"')
  expect_error(freq_two_vects(data.frame, numbers, letters), 'df must be a data frame')
  expect_error(freq_two_vects(list(a = 1, b = 2), a, b), 'df must be a data frame')
  expect_error(freq_two_vects(tbl, numbers, numbers), 'must be different')
  expect_error(freq_two_vects(tbl, numbers, letters, separate_tables = 'yes'), 'separate_tables was not logical')
  expect_error(freq_two_vects(tbl, numbers, letters, separate_tables = NA), 'separate_tables was not logical')
})

test_that("quoted column names work and df can be an expression", {
  expect_equal(freq_two_vects(tbl, "numbers", "letters"), freq_two_vects(tbl, numbers, letters))
  # The old exists() check rejected anything that was not a bare object name.
  expect_equal(nrow(freq_two_vects(tbl[tbl$numbers == 1, ], numbers, letters)), 3)
  expect_silent(freq_two_vects(head(tbl, 50), numbers, letters))
})

test_that("percentages within each col1 value sum to 100 and counts sum to nrow", {
  res <- freq_two_vects(tbl, letters, dates)
  expect_equal(sum(res$Count), nrow(tbl))
  sums <- tapply(res$Percentage, res$letters, sum)
  expect_true(all(abs(sums - 100) < 0.5))   # rounding to 1 dp per row
})

test_that("missing values are their own category", {
  d <- data.frame(g = c('a', 'a', 'a', NA, NA), s = c('x', NA, 'x', 'y', 'y'), stringsAsFactors = FALSE)
  res <- freq_two_vects(d, g, s)
  expect_equal(sum(res$Count), 5)
  expect_equal(res$Count[is.na(res$g)], 2)
  expect_equal(res$Percentage[is.na(res$g)], 100)
  expect_equal(res$Count[res$g %in% 'a' & is.na(res$s)], 1)
})

test_that("factors and sparse combinations are handled without zero rows", {
  d <- data.frame(a = factor(c('lo', 'hi', 'hi', 'lo'), levels = c('lo', 'hi')),
                  b = c('p', 'q', 'q', 'p'), stringsAsFactors = FALSE)
  res <- freq_two_vects(d, a, b)
  expect_equal(nrow(res), 2)                # only observed combinations
  expect_equal(as.character(res$a), c('lo', 'hi'))  # factor level order respected
})

test_that("large input runs quickly", {
  set.seed(2)
  big <- data.frame(school = sample(1:500, 5e5, replace = TRUE),
                    ethnicity = sample(letters[1:7], 5e5, replace = TRUE))
  elapsed <- system.time(res <- freq_two_vects(big, school, ethnicity))[['elapsed']]
  expect_equal(sum(res$Count), 5e5)
  expect_lt(elapsed, 5)
})
check_numbers_letters_f <- data.frame(numbers = c(1L, 1L, 1L, 2L, 2L),
                                      letters = c('a', 'b', 'c', 'a', 'c'),
                                      Count   = c(25L, 21L, 10L, 28L, 27L),
                                      Percentage = c(44.6, 37.5, 17.9, 35.9, 34.6),
                                      stringsAsFactors = FALSE)

check_numbers_letters_t1 <- data.frame(numbers = c(1L, 1L, 1L),
                                       letters = c('a', 'b', 'c'),
                                       Count   = c(25L, 21L, 10L),
                                       Percentage = c(44.6, 37.5, 17.9),
                                       stringsAsFactors = FALSE)

check_numbers_letters_t2 <- data.frame(numbers = c(2L, 2L, 2L),
                                       letters = c('a', 'c', 'b'),
                                       Count   = c(28L, 27L, 23L),
                                       Percentage = c(35.9, 34.6, 29.5),
                                       stringsAsFactors = FALSE)

check_letters_numbers_f <- data.frame(letters    = c('a', 'a', 'a', 'b', 'b'),
                                      numbers    = c(2L, 1L, 3L, 2L, 1L),
                                      Count      = c(28L, 25L, 25L, 23L, 21L),
                                      Percentage = c(35.9, 32.1, 32.1, 35.4, 32.3),
                                      stringsAsFactors = FALSE)

test_that('numbers and letters aggregations are correct', {
  expect_equal(freq_two_vects(tbl, numbers, letters, FALSE)[1:5,], check_numbers_letters_f)
  expect_equal(freq_two_vects(tbl, numbers, letters, TRUE)[[1]],   check_numbers_letters_t1)
  expect_equal(freq_two_vects(tbl, numbers, letters, TRUE)[[2]],   check_numbers_letters_t2)
  expect_equal(freq_two_vects(tbl, letters, numbers, FALSE)[1:5,],  check_letters_numbers_f)
})

check_dates_logicals_f <- data.frame(dates = as.Date(c('1999-10-01', '1999-10-01', '1999-10-02',
                                                       '1999-10-02', '1999-10-03')),
                                     logicals = c(TRUE, FALSE, FALSE, TRUE, FALSE),
                                     Count = c(32L, 21L, 41L, 36L, 41L),
                                     Percentage = c(60.4, 39.6, 53.2, 46.8, 58.6),
                                     stringsAsFactors = FALSE)

check_dates_logicals_t1 <- data.frame(dates = as.Date(c('1999-10-01', '1999-10-01')),
                                      logicals = c(TRUE, FALSE),
                                      Count = c(32L, 21L),
                                      Percentage = c(60.4, 39.6),
                                      stringsAsFactors = FALSE)

check_dates_logicals_t2 <- data.frame(dates = as.Date(c('1999-10-02', '1999-10-02')),
                                     logicals = c(FALSE, TRUE),
                                     Count = c(41L, 36L),
                                     Percentage = c(53.2, 46.8),
                                     stringsAsFactors = FALSE)

check_logicals_dates_f <- data.frame(logicals = c(FALSE, FALSE, FALSE, TRUE, TRUE),
                                     dates = as.Date(c('1999-10-02', '1999-10-03', '1999-10-01',
                                                       '1999-10-02', '1999-10-01')),
                                     Count = c(41L, 41L, 21L, 36L, 32L),
                                     Percentage = c(39.8, 39.8, 20.4, 37.1, 33.0),
                                     stringsAsFactors = FALSE)

test_that('dates and logicals aggregations are correct', {
  expect_equal(freq_two_vects(tbl, dates, logicals, FALSE)[1:5,], check_dates_logicals_f)
  expect_equal(freq_two_vects(tbl, dates, logicals, TRUE)[[1]], check_dates_logicals_t1)
  expect_equal(freq_two_vects(tbl, dates, logicals, TRUE)[[2]], check_dates_logicals_t2)
  expect_equal(freq_two_vects(tbl, logicals, dates, FALSE)[1:5,], check_logicals_dates_f)
})


