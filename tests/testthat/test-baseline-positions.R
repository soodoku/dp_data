testthat::test_that("baseline recodes retain nonanswers and third categories", {
  testthat::expect_identical(
    baseline_position_recode(c(1, 2, 3, 4, 5, NA, 8), "euro_preference"),
    c(
      "supports", "opposes", "blank_ballot", "will_not_vote", "undecided",
      "missing", "unresolved_code"
    )
  )
  testthat::expect_identical(
    baseline_position_recode(c(1, 2, 3, 4, 77, 98, 99), "party_identity"),
    c(
      "democrat", "republican", "independent", "none", "dont_know",
      "skipped", "refused"
    )
  )
  testthat::expect_identical(
    baseline_position_recode(c(1, 2, 3, 6, NA), "religious_affiliation"),
    c("catholic", "protestant", "muslim", "dont_know", "missing")
  )
  testthat::expect_error(baseline_position_recode(1, "invented"), "Unknown")
})

testthat::test_that("baseline linkage respects IDs and immutable file rows", {
  raw <- data.frame(id = c(42, 51, NA), source_row = 1:3)
  selected <- data.frame(respondent_id = c("51", "42"), source_row = c(2L, 1L))
  testthat::expect_identical(
    match_baseline_rows(selected, raw, "id", TRUE),
    c(2L, 1L)
  )
  testthat::expect_error(match_baseline_rows(selected, raw, "source_row", TRUE))
  row_ids <- data.frame(respondent_id = c("2", "1"), source_row = c(2L, 1L))
  testthat::expect_identical(
    match_baseline_rows(row_ids, raw, "source_row", TRUE), c(2L, 1L)
  )
  selected$source_row <- c(1L, 2L)
  testthat::expect_error(match_baseline_rows(selected, raw, "id", TRUE))
  testthat::expect_identical(
    match_baseline_rows(selected, raw, "id", FALSE),
    c(2L, 1L)
  )
  raw$id[3] <- 42
  testthat::expect_error(match_baseline_rows(selected, raw, "id", FALSE))
  raw$id[3] <- NA
  selected$respondent_id[1] <- "absent"
  testthat::expect_error(match_baseline_rows(selected, raw, "id", FALSE))
  selected$respondent_id[1] <- NA
  testthat::expect_error(match_baseline_rows(selected, raw, "id", FALSE))
})

testthat::test_that("baseline source checks reject altered input files", {
  root <- tempfile()
  dir.create(file.path(root, "metadata"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  path <- file.path(root, "input.csv")
  writeLines("original", path)
  checksum <- digest::digest(path, file = TRUE, algo = "sha256")
  readr::write_csv(
    data.frame(path = "input.csv", sha256 = checksum),
    file.path(root, "metadata", "source_files.csv")
  )
  testthat::expect_identical(baseline_source_hash(root, "input.csv"), checksum)
  writeLines("changed", path)
  testthat::expect_error(baseline_source_hash(root, "input.csv"), "checksum")
})

testthat::test_that("religion screening nonanswers retain their meaning", {
  testthat::expect_identical(
    baseline_religion_recode(c(1, NA, NA, NA, NA), c(1, 2, 3, 4, NA)),
    c("catholic", "no_religion", "refused_or_na", "no_answer", "missing")
  )
  testthat::expect_identical(
    baseline_religion_recode(c(2, 5), c(NA, NA)),
    c("protestant", "other")
  )
})
