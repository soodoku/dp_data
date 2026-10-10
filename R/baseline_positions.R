baseline_position_recode <- function(value, measure) {
  value <- as.numeric(value)
  map <- switch(measure,
    euro_preference = c(
      "1" = "supports", "2" = "opposes", "3" = "blank_ballot",
      "4" = "will_not_vote", "5" = "undecided"
    ),
    party_identity = c(
      "1" = "democrat", "2" = "republican", "3" = "independent",
      "4" = "none", "77" = "dont_know", "98" = "skipped",
      "99" = "refused"
    ),
    religious_affiliation = c(
      "1" = "catholic", "2" = "protestant", "3" = "muslim",
      "4" = "jewish", "5" = "other", "6" = "dont_know",
      "7" = "no_answer"
    ),
    stop("Unknown baseline measure: ", measure)
  )
  result <- unname(map[as.character(value)])
  result[is.na(value)] <- "missing"
  result[is.na(result)] <- "unresolved_code"
  result
}

baseline_source_hash <- function(root, path) {
  manifest <- readr::read_csv(
    file.path(root, "metadata", "source_files.csv"),
    col_types = readr::cols(.default = readr::col_character())
  )
  expected <- unique(manifest$sha256[manifest$path == path])
  actual <- digest::digest(file.path(root, path), file = TRUE, algo = "sha256")
  if (length(expected) != 1L || !identical(expected, actual)) {
    stop("Baseline source checksum mismatch: ", path)
  }
  actual
}

baseline_religion_recode <- function(affiliation, religious) {
  stopifnot(length(affiliation) == length(religious))
  result <- baseline_position_recode(affiliation, "religious_affiliation")
  missing_affiliation <- is.na(affiliation)
  result[which(missing_affiliation & religious == 2)] <- "no_religion"
  result[which(missing_affiliation & religious == 3)] <- "refused_or_na"
  result[which(missing_affiliation & religious == 4)] <- "no_answer"
  result
}

match_baseline_rows <- function(selected, raw, id, check_source_row) {
  ids <- as.character(raw[[id]])
  stopifnot(!anyNA(selected$respondent_id))
  stopifnot(!anyDuplicated(ids[!is.na(ids)]))
  index <- match(selected$respondent_id, ids)
  stopifnot(!anyNA(index))
  if (check_source_row) {
    stopifnot(all(raw$source_row[index] == selected$source_row))
  }
  index
}

build_baseline_positions <- function(root, participants) {
  keys <- c("poll_id", "source_dataset", "respondent_id")
  stopifnot(!anyNA(participants[keys]))
  stopifnot(!anyDuplicated(participants[keys]))
  specifications <- list(
    list(
      poll = "denmark-euro-2000", measure = "euro_preference",
      path = "data/denmark-euro-2000/survey.parquet",
      id = "delnr", column = "s_15", source_wave = "t0",
      dictionary = "data/denmark-euro-2000/value-labels.csv",
      evidence = paste(
        "data/denmark-euro-2000/value-labels.csv s_15;",
        "metadata/analysis_phase_roles.csv Denmark t0 recruitment"
      )
    ),
    list(
      poll = "northern-ireland-2007", measure = "religious_affiliation",
      path = "data/northern-ireland-2007/survey.parquet",
      id = "cserial", column = "t1q31", source_wave = "T1",
      dictionary = "data/northern-ireland-2007/value-labels.csv",
      evidence = paste(
        "data/northern-ireland-2007/value-labels.csv t1q30/t1q31;",
        "metadata/analysis_phase_roles.csv Northern Ireland T1 recruitment"
      )
    ),
    list(
      poll = "america-in-one-room-2019", measure = "party_identity",
      path = "data/america-in-one-room-2019/participants.tab",
      id = "source_row", column = "D1", source_wave = "PRE",
      dictionary = paste0(
        "data/america-in-one-room-2019/codebooks/a1r_codebook.tab"
      ),
      evidence = paste(
        "data/america-in-one-room-2019/codebooks/a1r_codebook.tab D1;",
        "metadata/analysis_phase_roles.csv A1R PRE"
      )
    )
  )
  rows <- lapply(specifications, function(spec) {
    checksum <- baseline_source_hash(root, spec$path)
    dictionary_hash <- baseline_source_hash(root, spec$dictionary)
    selected <- participants[participants$poll_id == spec$poll, ]
    path <- file.path(root, spec$path)
    if (grepl("\\.parquet$", path)) {
      fields <- c("source_row", spec$id, spec$column)
      if (spec$measure == "religious_affiliation") {
        fields <- c(fields, "t1q30")
      }
      raw <- arrow::read_parquet(path, col_select = unique(fields))
    } else {
      raw <- readr::read_tsv(path,
        col_select = "D1",
        col_types = readr::cols(.default = readr::col_double()),
        na = c("", "NA"), show_col_types = FALSE
      )
      raw$source_row <- seq_len(nrow(raw))
    }
    index <- match_baseline_rows(selected, raw, spec$id,
      check_source_row = spec$poll != "northern-ireland-2007"
    )
    source_row <- raw$source_row[index]
    value <- as.numeric(raw[[spec$column]][index])
    group <- baseline_position_recode(value, spec$measure)
    auxiliary_value <- rep(NA_real_, length(index))
    auxiliary_column <- NA_character_
    if (spec$measure == "religious_affiliation") {
      auxiliary_column <- "t1q30"
      auxiliary_value <- as.numeric(raw$t1q30[index])
      group <- baseline_religion_recode(value, auxiliary_value)
    }
    tibble::tibble(
      poll_id = selected$poll_id,
      source_dataset = selected$source_dataset,
      respondent_id = selected$respondent_id,
      measure_id = spec$measure,
      baseline_group = group,
      raw_value = value,
      auxiliary_raw_value = auxiliary_value,
      auxiliary_source_column = auxiliary_column,
      source_column = spec$column,
      source_row = as.integer(source_row),
      source_wave = spec$source_wave,
      wave_role = "pre_arrival",
      source_path = spec$path,
      source_sha256 = checksum,
      dictionary_path = spec$dictionary,
      dictionary_sha256 = dictionary_hash,
      evidence = spec$evidence
    )
  })
  result <- dplyr::bind_rows(rows)
  stopifnot(!anyDuplicated(result[c(keys, "measure_id")]))
  result
}
