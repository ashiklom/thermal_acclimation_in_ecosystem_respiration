# Readers for the raw site data (original-data/SiteData), one per product.
#
# Each product keeps one file per site, named after the site:
#   AmeriFlux_BASE  AmeriFlux_BASE/AMF_<site>_BASE-BADM_<version>.zip
#   FLUXNET2015     FLUXNET2015/unzip/FLX_<site>_FLUXNET2015_FULLSET_(HH|HR)_*.csv
#   FLUXNET2020     FLUXNET2020/unzip/FLX_<site>_FLUXNET2015_FULLSET_(HH|HR)_*.csv
#   FLUXNET2025     FLUXNET07202025/unzip/FLX_<site>_*.csv
#   ICOS2025        Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1/unzip/ICOSETC_<site>_*.csv
#   ICOS2020        ICOS_after2020/*<site>*.csv (listed in the original scripts; no site uses it)
#
# and, downloaded for the data update (R/download-flux.R), in path_raw("flux")
# whatever `base_dir`:
#   SHUTTLE         SHUTTLE/unzip/<hub>_<site>_FLUXNET_FLUXMET_(HH|HR)_*.csv
#
# A site's `source` names its products in one of two ways: joined by "_"
# (stitched: each later product only adds the records after the end of the
# record so far, as the original scripts did) or by "+" (spliced: the first
# product wins wherever it has records, the others only add records before
# its start or after its end; used for the data update, where the newest
# release comes first).

raw_product_dirs <- list(
  AmeriFlux_BASE = "AmeriFlux_BASE",
  FLUXNET2015 = file.path("FLUXNET2015", "unzip"),
  FLUXNET2020 = file.path("FLUXNET2020", "unzip"),
  FLUXNET2025 = file.path("FLUXNET07202025", "unzip"),
  ICOS2025 = file.path("Ecosystem final quality (L2) product in ETC-Archive format - release 2025-1", "unzip"),
  ICOS2020 = "ICOS_after2020",
  SHUTTLE = file.path("SHUTTLE", "unzip")
)

# Products downloaded for the data update, read from path_raw("flux").
update_products <- c("SHUTTLE")

# The products named by a site's source.
source_products <- function(source) {
  if (source == "AmeriFlux_BASE") "AmeriFlux_BASE" else strsplit(source, "[_+]")[[1]]
}

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
    SHUTTLE = paste0("_", site_re, "_.*FLUXMET_(HH|HR)_.*\\.csv$"),
    stop("unknown product: ", product)
  )
  root <- if (product %in% update_products) path_raw("flux") else base_dir
  files <- list.files(file.path(root, raw_product_dirs[[product]]), pattern = pattern, full.names = TRUE)
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

# A site's FLUXNET-format record from its source (see above).
read_fluxnet_record <- function(site_id, source, base_dir = path_original("SiteData")) {
  tables <- lapply(source_products(source), function(product) read_fluxnet(site_id, product, base_dir))
  if (grepl("+", source, fixed = TRUE)) splice_products(tables) else stitch_products(tables)
}

# The first table's records, plus each later table's records before the
# start or after the end of the record so far; sorted by time.
splice_products <- function(tables) {
  a <- tables[[1]]
  for (next_table in tables[-1]) {
    outside <- next_table$TIMESTAMP_START < min(a$TIMESTAMP_START) | next_table$TIMESTAMP_START > max(a$TIMESTAMP_START)
    a <- dplyr::bind_rows(a, next_table[outside, , drop = FALSE])
  }
  a[order(a$TIMESTAMP_START), , drop = FALSE]
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
