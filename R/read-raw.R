# Readers for the raw site data (original-data/SiteData), one per product.
#
# Each product keeps one file per site, named after the site:
#   AmeriFlux_BASE  AmeriFlux_BASE/AMF_<site>_BASE-BADM_<version>.zip
#   FLUXNET2015     FLUXNET2015/unzip/FLX_<site>_FLUXNET2015_FULLSET_(HH|HR)_*.csv
#   FLUXNET2020     FLUXNET2020/unzip/FLX_<site>_FLUXNET2015_FULLSET_(HH|HR)_*.csv
#   FLUXNET2025     FLUXNET07202025/unzip/FLX_<site>_*.csv
#   ICOS2025        Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1/unzip/ICOSETC_<site>_*.csv
#   ICOS2020        ICOS_after2020/*<site>*.csv (listed in the original scripts; no site uses it)

raw_product_dirs <- list(
  AmeriFlux_BASE = "AmeriFlux_BASE",
  FLUXNET2015 = file.path("FLUXNET2015", "unzip"),
  FLUXNET2020 = file.path("FLUXNET2020", "unzip"),
  FLUXNET2025 = file.path("FLUXNET07202025", "unzip"),
  ICOS2025 = file.path("Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1", "unzip"),
  ICOS2020 = "ICOS_after2020"
)

# Path of a site's file of one product.
raw_product_file <- function(site_id, product, base_dir = path_original("SiteData")) {
  site_re <- gsub("-", "\\\\-", site_id)
  pattern <- switch(product,
    AmeriFlux_BASE = paste0("^AMF_", site_re, "_BASE-BADM_.*\\.zip$"),
    FLUXNET2015 = ,
    FLUXNET2020 = paste0("^FLX_", site_re, "_FLUXNET2015_FULLSET_(HH|HR)_.*\\.csv$"),
    FLUXNET2025 = paste0("^FLX_", site_re, "_.*\\.csv$"),
    ICOS2025 = paste0("^ICOSETC_", site_re, "_.*\\.csv$"),
    ICOS2020 = paste0(site_re, ".*\\.csv$"),
    stop("unknown product: ", product)
  )
  files <- list.files(file.path(base_dir, raw_product_dirs[[product]]), pattern = pattern, full.names = TRUE)
  if (length(files) != 1) {
    stop("expected one ", product, " file for ", site_id, ", found ", length(files), ": ", paste(basename(files), collapse = ", "))
  }
  files
}

# A site's AmeriFlux BASE half-hourly table, with parsed timestamps
# (TIMESTAMP, YEAR, MONTH, DAY, DOY, HOUR, MINUTE; local standard time).
read_ameriflux_base <- function(site_id, base_dir = path_original("SiteData")) {
  amerifluxr::amf_read_base(raw_product_file(site_id, "AmeriFlux_BASE", base_dir), parse_timestamp = TRUE, unzip = TRUE)
}

# A site's table from one FLUXNET-format product (FLUXNET2015/2020/2025,
# ICOS2025, ICOS2020), as read.csv() reads it.
read_fluxnet <- function(site_id, product, base_dir = path_original("SiteData")) {
  utils::read.csv(raw_product_file(site_id, product, base_dir))
}

# A site's FLUXNET-format record from the products in `sources` (in order,
# e.g. c("FLUXNET2025", "ICOS2025")). Each later product only adds the
# records after the end of the record so far: those after its row for the
# record's last TIMESTAMP_START, or all of its rows if it has no such row.
read_fluxnet_sources <- function(site_id, sources, base_dir = path_original("SiteData")) {
  stitch_products(lapply(sources, function(product) read_fluxnet(site_id, product, base_dir)))
}

stitch_products <- function(tables) {
  by_time <- function(x) x[order(x$TIMESTAMP_START), , drop = FALSE]
  a <- by_time(tables[[1]])
  for (next_table in tables[-1]) {
    next_table <- by_time(next_table)
    last <- max(a$TIMESTAMP_START)
    after <- if (any(next_table$TIMESTAMP_START == last)) next_table$TIMESTAMP_START > last else rep(TRUE, nrow(next_table))
    a <- dplyr::bind_rows(a, next_table[after, , drop = FALSE])
  }
  a
}
