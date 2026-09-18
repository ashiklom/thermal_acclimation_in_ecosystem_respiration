# The declared column contracts at the two file-reading boundaries, and the
# tibble conversion that makes `site_info`-driven column lookups self-checking.

use_project_root()

# A FLUXNET-format table in miniature: 12-digit timestamps, one column that is
# sentinel for its entire length (these files are full of them), one that is
# not. `TS_F_MDS_1` is the all-sentinel one because a `TS >= TS_MIN_VALID`
# filter downstream is exactly what a mistyped column would corrupt.
write_fluxnet_fixture <- function(path) {
  writeLines(
    c(
      "TIMESTAMP_START,TIMESTAMP_END,TS_F_MDS_1,NEE_VUT_REF,SWC_F_MDS_1",
      "199601010000,199601010030,-9999,-1.53,22.4",
      "199601010030,199601010100,-9999,-1.21,22.5"
    ),
    path
  )
  path
}

test_that("FLUXNET_COL_TYPES keeps timestamps character and everything else double", {
  path <- write_fluxnet_fixture(withr::local_tempfile(fileext = ".csv"))
  dat <- readr::read_csv(path, col_types = FLUXNET_COL_TYPES, progress = FALSE)

  expect_equal(nrow(readr::problems(dat)), 0)
  # The splice orders and compares these as strings, so the type is load-bearing.
  expect_type(dat$TIMESTAMP_START, "character")
  expect_type(dat$TIMESTAMP_END, "character")
  expect_identical(dat$TIMESTAMP_START[[1]], "199601010000")
  # Double even though every value is the sentinel.
  expect_type(dat$TS_F_MDS_1, "double")
  expect_type(dat$NEE_VUT_REF, "double")

  # And the numeric sentinel sweep still empties it.
  dat[dat == -9999] <- NA
  expect_true(all(is.na(dat$TS_F_MDS_1)))
  expect_equal(dat$NEE_VUT_REF, c(-1.53, -1.21))
})

test_that("type guessing gets the all-sentinel column wrong", {
  # Why the contract is declared rather than inferred. Guessing with -9999
  # treated as missing types the column `logical`, and `TS >= TS_MIN_VALID`
  # then compares against a logical NA instead of filtering numbers. This test
  # fails if someone replaces the spec with readr's `na` argument.
  path <- write_fluxnet_fixture(withr::local_tempfile(fileext = ".csv"))
  guessed <- readr::read_csv(
    path,
    na = c("", "NA", "-9999"),
    col_types = readr::cols(),
    progress = FALSE
  )
  expect_type(guessed$TS_F_MDS_1, "logical")
  expect_type(guessed$TIMESTAMP_START, "double")
})

test_that("the ERA5 contract turns a non-numeric SWC column into an error", {
  # col_double() makes the bad value NA rather than silently typing the column
  # character, and the unit guard then refuses it by name.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c("time,site,SWC", "1990-01-01,X-Tst,0.25 m3/m3", "1990-01-02,X-Tst,0.31 m3/m3"),
    path
  )
  expect_error(
    suppressWarnings(read_era5_swc("X-Tst", path)),
    "not a volumetric fraction"
  )
})

test_that("every amf_read_base() result is converted to a tibble", {
  # The AmeriFlux path reaches into the base table with column names taken from
  # site_info (`a[[site_info$SW_IN]]` and friends). On the data.frame that
  # `amf_read_base()` returns, an NA or absent name yields NULL silently and the
  # complaint surfaces much later as a missing column; on a tibble it raises at
  # the point of use. Keep the conversion adjacent to every read.
  lines <- readLines(file.path("R", "ameriflux.R"), warn = FALSE)
  hits <- grep("amf_read_base\\(", lines)
  hits <- hits[!grepl("^\\s*#", lines[hits])]
  expect_gt(length(hits), 0)
  unconverted <- hits[!vapply(
    hits,
    function(i) any(grepl("as_tibble", lines[i:min(i + 6L, length(lines))])),
    logical(1)
  )]
  expect_equal(unconverted, integer())
})

test_that("a tibble is what makes an NA column name fail loudly", {
  # Pins the semantics the conversion above is bought with, so that the reason
  # survives even if tibble's message changes.
  base <- data.frame(SWC_1_1_1 = 1.5, TA = 20)
  expect_null(base[[NA_character_]])
  expect_error(tibble::as_tibble(base)[[NA_character_]], "NA_character_")
  # Partial matching is the other silent path this closes.
  expect_equal(base$SWC_1_1, 1.5)
  expect_warning(tibble::as_tibble(base)$SWC_1_1, "Unknown or uninitialised column")
})
