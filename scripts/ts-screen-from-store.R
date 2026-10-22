# TEMPORARY (remove in the final cleanup): compute the soil-temperature sensor
# screen (R/ts-screen.R) from an existing targets store without tar_make(),
# for use while another run holds the store's lock. The pipeline's own
# targets (ts_screen, ts_screen_all, ts_screen_files) are the canonical way.
#
# Usage (from the repository root):
#   Rscript scripts/ts-screen-from-store.R [--store=_targets] [--out=data-proc/ts-screen] [--sites=A,B] [--cores=8]
# Reads each site's step01, ts_sensor and tas_inputs targets; writes the
# screen's four tables (write_ts_screen()) to --out.

# the R code next to this script (so it can run from another checkout than
# the working directory's, whose data/ and store it reads)
script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
code <- if (length(script)) dirname(dirname(normalizePath(script))) else "."
for (f in c("paths", "io", "step02-prep", "step02-tas", "ts-qualify", "ts-columns", "ts-screen")) source(file.path(code, "R", paste0(f, ".R")))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit) else default
}
store <- opt("store", "_targets")
out <- opt("out", file.path("data-proc", "ts-screen"))
settings <- read_site_settings()
site_info <- utils::read.csv(file.path("data", "site_info.csv"))
sites <- if (nzchar(opt("sites", ""))) strsplit(opt("sites", ""), ",")[[1]] else site_info$site_ID

screen_one <- function(s) {
  n <- gsub("-", "_", s)
  tryCatch(ts_screen_site(targets::tar_read_raw(paste0("step01_", n), store = store),
                          targets::tar_read_raw(paste0("ts_sensor_", n), store = store),
                          settings[settings$site_ID == s, ],
                          targets::tar_read_raw(paste0("tas_inputs_", n), store = store)),
           error = function(e) { message(s, ": ", conditionMessage(e)); NULL })
}
results <- parallel::mclapply(sites, screen_one, mc.cores = as.integer(opt("cores", "8")))
failed <- sites[vapply(results, is.null, logical(1))]
if (length(failed)) message("not screened: ", paste(failed, collapse = ", "))
screen <- combine_ts_screen(Filter(Negate(is.null), results))
paths <- write_ts_screen(screen, out, validation = ts_screen_validation(screen, site_info, settings))
message("wrote ", paste(paths, collapse = ", "))
