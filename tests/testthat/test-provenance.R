# Multi-product provenance: parsing a site's source list, locating products, and
# the splice rule that joins them.

use_project_root()

test_that("site_sources parses single and multi-product strings", {
  expect_equal(site_sources(list(site_ID = "x", source = "ICOS")), "ICOS")
  expect_equal(
    site_sources(list(site_ID = "x", source = "FLUXNET+ICOS")),
    c("FLUXNET", "ICOS")
  )
  expect_equal(
    site_sources(list(site_ID = "x", source = "WW2020+FLUXNET+ICOS")),
    c("WW2020", "FLUXNET", "ICOS")
  )
  # tolerate incidental whitespace, since this column is hand-maintained upstream
  expect_equal(
    site_sources(list(site_ID = "x", source = "FLUXNET + ICOS")),
    c("FLUXNET", "ICOS")
  )
})

test_that("an unknown product is rejected by name", {
  expect_error(
    site_sources(list(site_ID = "XX-Bad", source = "FLUXNET+NOPE")),
    "NOPE"
  )
})

test_that("site_reader sends only AmeriFlux BASE down the AmeriFlux path", {
  expect_equal(site_reader(list(site_ID = "x", source = "AmeriFlux_BASE")), "ameriflux")
  expect_equal(site_reader(list(site_ID = "x", source = "FLUXNET+ICOS")), "fluxnet_family")
  expect_equal(site_reader(list(site_ID = "x", source = "TERN")), "fluxnet_family")
})

test_that("every site in site_info declares products we know how to read", {
  # Guards the generated column against typos: this is the check that would
  # catch a hand-edit of site_info.csv introducing an unreadable source.
  site_info <- get_site_info()
  for (i in seq_len(nrow(site_info))) {
    expect_true(length(site_sources(site_info[i, ])) >= 1)
  }
})

test_that("product_file reports absence rather than guessing", {
  expect_true(is.na(product_file("XX-None", "ICOS")))
  expect_error(product_file("XX-None", "NoSuchProduct"), "Unknown data product")
})

# ---------------------------------------------------------------- splice rule

two_products <- function() {
  # Overlapping records: the old one covers 2010-2011, the new one 2011-2012.
  old <- data.frame(
    TIMESTAMP_START = c("201001010000", "201006010000", "201101010000", "201106010000"),
    NEE_VUT_REF = 1:4, src = "old"
  )
  new <- data.frame(
    TIMESTAMP_START = c("201101010000", "201106010000", "201201010000", "201206010000"),
    NEE_VUT_REF = 11:14, src = "new"
  )
  list(new = new, old = old) # deliberately in the wrong order
}

test_that("splice_products orders by timestamp, not by the caller's order", {
  out <- splice_products(two_products())
  expect_equal(out$TIMESTAMP_START, sort(out$TIMESTAMP_START))
  expect_equal(out$TIMESTAMP_START[1], "201001010000")
})

test_that("the earlier product wins where two overlap", {
  out <- splice_products(two_products())
  # 2011 exists in both; it must come from the older product
  overlap <- out[substr(out$TIMESTAMP_START, 1, 4) == "2011", ]
  expect_equal(unique(overlap$src), "old")
  # and the newer product contributes only its non-overlapping tail
  expect_equal(out$src[substr(out$TIMESTAMP_START, 1, 4) == "2012"], c("new", "new"))
})

test_that("splicing extends the record rather than duplicating it", {
  out <- splice_products(two_products())
  expect_equal(nrow(out), 6L)
  expect_equal(anyDuplicated(out$TIMESTAMP_START), 0L)
  expect_equal(range(substr(out$TIMESTAMP_START, 1, 4)), c("2010", "2012"))
})

test_that("a single product passes through untouched", {
  one <- two_products()["old"]
  expect_equal(splice_products(one), one$old)
})

test_that("splicing nothing is an error, not an empty frame", {
  expect_error(splice_products(list()), "Nothing to splice")
})

test_that("no R source compares site_info$source to a bare product name", {
  # `source` is a `+`-separated list, so `== "ICOS"` silently matches nothing
  # once a site gains a second product. Three call sites were missed when the
  # column became compound -- fix_soil_temp(), the _targets.R site filter and
  # the coverage audit -- and each failed late and confusingly. Compare via
  # site_reader()/site_sources(), or grepl(..., fixed = TRUE).
  offenders <- character()
  for (f in c(list.files("R", pattern = "[.]R$", full.names = TRUE), "_targets.R")) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep("(\\$source|\\[\\[\"source\"\\]\\])\\s*(==|%in%)", lines)
    hits <- hits[!grepl("^\\s*#", lines[hits])]
    if (length(hits)) offenders <- c(offenders, sprintf("%s:%d", f, hits))
  }
  expect_equal(offenders, character())
})

# ------------------------------------------------------- FLUXNET2015

test_that("FLUXNET2015 has its own directory, distinct from the shuttle's", {
  # The two products share a site's provenance list at FI-Sod, and their
  # filename patterns are different, but keeping them in one directory invited
  # confusion about which one a file belonged to -- and FLUXNET2015 is
  # acquired by hand, so it is not the downloader's to manage.
  expect_equal(FLUX_PRODUCTS[["FLUXNET2015"]]$dir, "FLUXNET2015")
  expect_equal(FLUX_PRODUCTS[["FLUXNET"]]$dir, "FLUXNET")
  # The shuttle's FLUXMET pattern must not match a FLUXNET2015 FULLSET table,
  # or a site declaring both would resolve the same file twice.
  fullset <- "FLX_FI-Sod_FLUXNET2015_FULLSET_HH_2001-2014_1-4.csv"
  fluxmet <- "ICOSETC_FI-Sod_FLUXNET_FLUXMET_HH_2023-2025_v1.3_r1.csv"
  expect_true(grepl(FLUX_PRODUCTS[["FLUXNET2015"]]$pattern, fullset))
  expect_false(grepl(FLUX_PRODUCTS[["FLUXNET"]]$pattern, fullset))
  expect_true(grepl(FLUX_PRODUCTS[["FLUXNET"]]$pattern, fluxmet))
  expect_false(grepl(FLUX_PRODUCTS[["FLUXNET2015"]]$pattern, fluxmet))
})

test_that("FI-Sod declares the three products its record actually needs", {
  # 2001-2014 from FLUXNET2015, 2023-2024 from the shuttle, 2023-2025 from
  # ICOS. Nothing public covers 2015-2022; see docs/data-provenance.md.
  expect_equal(
    site_sources(get_site_info("FI-Sod")),
    c("FLUXNET2015", "FLUXNET", "ICOS")
  )
})

test_that("an absent FLUXNET2015 archive explains how to obtain it", {
  # There is no programmatic route, so the downloader's whole job is to say so
  # precisely rather than fail obscurely or silently skip.
  err <- expect_error(download_fluxnet2015("XX-None"), "by hand")
  msg <- conditionMessage(err)
  expect_match(msg, "XX-None")
  expect_match(msg, "fluxnet.org")
  expect_match(msg, "Data Policy")
  expect_match(msg, file.path("data-raw", "FLUXNET2015", "XX-None"), fixed = TRUE)
})

test_that("a present FLUXNET2015 archive is left alone", {
  skip_if(is.na(product_file("FI-Sod", "FLUXNET2015")), "FI-Sod FLUXNET2015 not downloaded")
  expect_message(download_fluxnet2015("FI-Sod"), "already present")
})
