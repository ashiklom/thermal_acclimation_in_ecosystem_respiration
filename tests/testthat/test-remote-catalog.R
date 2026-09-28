# Version-aware downloads: when `download_site()` re-fetches, what it does with
# the old copy, and what it leaves behind when the provider fails. Everything
# runs in a scratch project directory -- DIR_RAWDATA is relative -- with the
# real downloaders swapped for fakes that write a release of our choosing.

# Replace a function the pipeline looks up in the global environment for the
# rest of the calling test.
mock_global <- function(name, value, envir = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, value, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = envir)
}

# Lay down one ICOS release for X-Tst, the way `download-icos.py` leaves it:
# the archive, and the half-hourly table extracted beside it.
write_icos_release <- function(span) {
  dir <- file.path(DIR_RAWDATA, "ICOS", "X-Tst")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  stem <- sprintf("ICOSETC_X-Tst_FLUXNET_FLUXMET_HH_%s_v1.3_r1", span)
  writeLines(span, file.path(dir, paste0(stem, ".csv")))
  writeLines(span, file.path(dir, paste0(stem, ".zip")))
  paste0(stem, ".zip")
}

icos_site <- list(site_ID = "X-Tst", source = "ICOS")

local_scratch_project <- function(envir = parent.frame()) {
  withr::local_dir(withr::local_tempdir(.local_envir = envir), .local_envir = envir)
}

test_that("an unchanged release is not fetched again", {
  local_scratch_project()
  old <- write_icos_release("2020-2025")
  mock_global("download_icos", function(...) stop("should not be called"))
  got <- suppressMessages(download_site(icos_site, remote = c(ICOS = old)))
  expect_match(basename(got), "2020-2025")
})

test_that("a new release supersedes the old copy and records its id", {
  local_scratch_project()
  old <- write_icos_release("2020-2025")
  new <- "ICOSETC_X-Tst_FLUXNET_FLUXMET_HH_2020-2026_v1.3_r1.zip"
  mock_global("download_icos", function(...) write_icos_release("2020-2026"))

  got <- suppressMessages(download_site(icos_site, remote = c(ICOS = new)))

  # exactly one table now, the new one -- `product_file()` stops on two
  expect_match(basename(got), "2020-2026")
  expect_equal(local_remote_id("X-Tst", "ICOS"), new)
  kept <- list.files(file.path(DIR_RAWDATA, "_superseded"), recursive = TRUE)
  expect_true(any(grepl(old, kept, fixed = TRUE)))
})

test_that("a failed update keeps the old copy and records nothing", {
  local_scratch_project()
  old <- write_icos_release("2020-2025")
  new <- "ICOSETC_X-Tst_FLUXNET_FLUXMET_HH_2020-2026_v1.3_r1.zip"
  mock_global("download_icos", function(...) {
    # half a download, then the provider goes away
    dir.create(file.path(DIR_RAWDATA, "ICOS", "X-Tst"), recursive = TRUE, showWarnings = FALSE)
    writeLines("partial", file.path(DIR_RAWDATA, "ICOS", "X-Tst", "partial.zip"))
    stop("connection reset")
  })

  expect_warning(
    got <- suppressMessages(download_site(icos_site, remote = c(ICOS = new))),
    "keeping"
  )
  expect_match(basename(got), "2020-2025")
  expect_equal(local_remote_id("X-Tst", "ICOS"), old)
  expect_false(file.exists(file.path(DIR_RAWDATA, "ICOS", "X-Tst", "partial.zip")))
  expect_length(list.files(file.path(DIR_RAWDATA, "_superseded"), recursive = TRUE), 0)
})

test_that("an unknown remote id keeps what is on disk", {
  local_scratch_project()
  write_icos_release("2020-2025")
  mock_global("download_icos", function(...) invisible(NULL)) # present: skips
  got <- suppressMessages(download_site(icos_site, remote = c(ICOS = NA)))
  expect_match(basename(got), "2020-2025")
  expect_length(list.files(file.path(DIR_RAWDATA, "_superseded"), recursive = TRUE), 0)
})

test_that("a first download records the id it was fetched for", {
  local_scratch_project()
  mock_global("download_icos", function(...) write_icos_release("2020-2026"))
  suppressMessages(download_site(icos_site, remote = c(ICOS = "some-id")))
  expect_equal(local_remote_id("X-Tst", "ICOS"), "some-id")
})

test_that("FLUXNET's flat archive is superseded with its directory", {
  local_scratch_project()
  dir <- file.path(DIR_RAWDATA, "FLUXNET", "X-Tst")
  dir.create(dir, recursive = TRUE)
  writeLines("x", file.path(dir, "ICOS_X-Tst_FLUXNET_FLUXMET_HH_1996-2025_v1.3_r1.csv"))
  writeLines("x", file.path(DIR_RAWDATA, "FLUXNET", "ICOS_X-Tst_FLUXNET_1996-2025_v1.3_r1.zip"))
  # a neighbour whose ID shares a prefix must not be swept up with it
  writeLines("x", file.path(DIR_RAWDATA, "FLUXNET", "ICOS_X-Tst2_FLUXNET_1996-2025_v1.3_r1.zip"))

  expect_equal(local_remote_id("X-Tst", "FLUXNET"), "ICOS_X-Tst_FLUXNET_1996-2025_v1.3_r1.zip")
  supersede_product("X-Tst", "FLUXNET")
  left <- list.files(file.path(DIR_RAWDATA, "FLUXNET"))
  expect_equal(left, "ICOS_X-Tst2_FLUXNET_1996-2025_v1.3_r1.zip")
})

test_that("a site's catalogue slice is a named product -> id vector", {
  catalog <- tibble::tibble(
    site_ID = c("A", "A", "B"),
    product = c("FLUXNET", "ICOS", "ICOS"),
    remote_id = c("f1", NA, "i2")
  )
  expect_equal(remote_for_site(catalog, "A"), c(FLUXNET = "f1", ICOS = NA))
  expect_length(remote_for_site(catalog, "C"), 0)
})
