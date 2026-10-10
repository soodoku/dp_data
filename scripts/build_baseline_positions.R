source("R/paths.R")
load_project()

root <- project_path()
path <- project_path(
  "output", "analysis", "analysis_phase_participants.parquet"
)
manifest <- readr::read_csv(project_path("output", "analysis", "manifest.csv"),
  show_col_types = FALSE
)
expected <- manifest$sha256[manifest$table == "analysis_phase_participants"]
actual <- digest::digest(path, file = TRUE, algo = "sha256")
stopifnot(identical(expected, actual))
participants <- arrow::read_parquet(path)
positions <- build_baseline_positions(root, participants)
directory <- project_path("output", "baseline_positions")
dir.create(directory, recursive = TRUE, showWarnings = FALSE)
target <- file.path(directory, "baseline_positions.parquet")
arrow::write_parquet(positions, target)
stopifnot(identical(positions, arrow::read_parquet(target)))
readr::write_csv(
  dplyr::count(positions, poll_id, source_dataset, measure_id, baseline_group),
  file.path(directory, "coverage.csv")
)
jsonlite::write_json(list(
  participant_sha256 = expected,
  output_sha256 = digest::digest(target, file = TRUE, algo = "sha256"),
  rows = nrow(positions),
  source_hashes = unique(positions[c("source_path", "source_sha256")]),
  dictionary_hashes = unique(positions[c(
    "dictionary_path", "dictionary_sha256"
  )]),
  recode_sha256 = digest::digest(
    "R/baseline_positions.R",
    file = TRUE, algo = "sha256"
  )
), file.path(directory, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)
message("Wrote ", nrow(positions), " baseline-position records")
