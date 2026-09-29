# Environmental and biological conditions that may drive thermal response
# strength: soil carbon, climate, and MODIS spectral indices (GPP, LAI, EVI,
# NDVI), joined to the pipeline's TAS estimates. Writes
# data-proc/analysis/acclimation_data.csv, which 03_02 and 04_02 read.
# Authors: Junna Wang, October, 2025

stopifnot(
  requireNamespace("amerifluxr"), requireNamespace("dplyr"), requireNamespace("lubridate"),
  requireNamespace("terra"), requireNamespace("zoo")
)

dir_rawdata <- "data-raw"

site_info <- read.csv(file.path("data-core", "site_info.csv"))

#--------------------------------------------SOIL DATA-------------------------------------
# Measured soil carbon (g C / kg soil) from the newest AmeriFlux BIF table on
# disk (date-stamped; xlsx or csv), fetched by the `ameriflux_bif_file` target.
bif_files <- list.files(dir_rawdata, pattern = "^AMF_AA-Net_BIF_.*[.](xlsx|csv)$", full.names = TRUE)
if (length(bif_files) == 0) {
  stop("No AmeriFlux BIF table in ", dir_rawdata, ". Run `pixi run download-bif`.")
}
bif_path <- bif_files[which.max(file.mtime(bif_files))]
BIF <- if (grepl("[.]xlsx$", bif_path)) {
  amerifluxr::amf_read_bif(bif_path)
} else {
  read.csv(bif_path)
}
BIF.site <- BIF |>
  dplyr::filter(SITE_ID %in% site_info$site_ID) |>
  dplyr::filter(VARIABLE == "SOIL_CHEM_C_ORG") |>
  dplyr::group_by(SITE_ID) |>
  dplyr::summarise(soc_obs = mean(as.numeric(DATAVALUE), na.rm = TRUE), .groups = "drop")

# Elsewhere, total SOC stocks (t/ha) from GSOCmap 1.5.0, rescaled to the
# measured values where there are some.
stat.soil <- data.frame(site_ID = site_info$site_ID)
GSOCmap <- terra::rast(file.path(dir_rawdata, "GSOCmap1.5.0.tif"))
xy <- data.frame(x = site_info$LONG, y = site_info$LAT)
stat.soil$GSOC <- terra::extract(GSOCmap, xy)$GSOCmap1.5.0

tmp <- BIF.site |> dplyr::left_join(stat.soil, by = c("SITE_ID" = "site_ID"))
model <- lm(data = tmp, GSOC ~ soc_obs)
BIF.site$GSOC_obs <- predict(model, BIF.site)

stat.soil <- stat.soil |>
  dplyr::left_join(BIF.site, by = c("site_ID" = "SITE_ID")) |>
  dplyr::mutate(SOC = dplyr::coalesce(GSOC_obs, GSOC))

#---------------------------------------------------CLIMATE DATA---------------------------------
# Per site, over the years step 01 qualified: mean annual NEE (all, daytime,
# nighttime; g C / m2), and air temperature's mean annual value (MATA),
# seasonal variation (SSTA), inter-annual variation (IATA), daily range
# (DRTA), and warming rate (the linear trend in annual means).
site_climate <- function(ac_file) {
  name_site <- sub("_ac\\.csv$", "", basename(ac_file))
  message(name_site)
  ac <- read.csv(ac_file)

  # use only the years with qualified data
  night_file <- file.path(dirname(ac_file), paste0(name_site, "_nightNEE.csv"))
  good_years <- unique(read.csv(night_file)$YEAR)

  # Fill air-temperature gaps from its DOY x time-of-day climatology.
  T_gf <- ac |>
    dplyr::group_by(DOY, HOUR, MINUTE) |>
    dplyr::summarise(TA_gf = mean(TA, na.rm = TRUE), .groups = "drop")
  ac <- ac |> dplyr::left_join(T_gf, by = c("DOY", "HOUR", "MINUTE"))
  ac$TA <- dplyr::coalesce(ac$TA, ac$TA_gf)

  # Time step in minutes, and the conversion from summed umol/m2/s to g C / m2.
  dt <- abs(ac$MINUTE[2] + ac$HOUR[2] * 60 - ac$MINUTE[1] - ac$HOUR[1] * 60)
  to_gC <- function(x) x * dt * 60 / 1000000 * 12

  ac <- ac |> dplyr::filter(YEAR %in% good_years)
  annual_sum <- function(dat) {
    dat |>
      dplyr::group_by(YEAR) |>
      dplyr::summarise(NEE = sum(NEE_uStar_f, na.rm = TRUE), .groups = "drop")
  }
  annual <- annual_sum(ac)
  annual_day <- annual_sum(ac |> dplyr::filter(daytime))
  annual_night <- annual_sum(ac |> dplyr::filter(!daytime))

  data_yearly <- ac |>
    dplyr::group_by(DOY) |>
    dplyr::summarise(TA = mean(TA, na.rm = TRUE), .groups = "drop")
  data_1day <- ac |>
    dplyr::group_by(YEAR, DOY) |>
    dplyr::summarise(TArange = max(TA, na.rm = TRUE) - min(TA, na.rm = TRUE), TA = mean(TA, na.rm = TRUE),
                     .groups = "drop")
  data_1year_sd <- data_1day |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(TA = sd(TA, na.rm = TRUE), .groups = "drop")
  data_1year_mn <- data_1day |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(TA = mean(TA, na.rm = TRUE), .groups = "drop")

  # Warming rate: a linear trend, which is closest to Sen's slope and copes
  # with missing years.
  data_annual <- ac |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(TA = mean(TA, na.rm = TRUE), .groups = "drop")
  if (name_site == "CH-Aws") {
    data_annual <- data_annual |> dplyr::filter(YEAR >= 2015)
  }
  trend <- coef(summary(lm(data = data_annual, TA ~ YEAR)))["YEAR", c("Estimate", "Pr(>|t|)")]

  data.frame(
    site_ID = name_site,
    NEE = to_gC(mean(annual$NEE)),
    NEE_day = to_gC(mean(annual_day$NEE)),
    NEE_night = to_gC(mean(annual_night$NEE)),
    MATA = mean(data_yearly$TA),
    SSTA = mean(data_1year_sd$TA),
    IATA = sd(data_1year_mn$TA),
    DRTA = mean(data_1day$TArange),
    warm_rate = unname(trend[["Estimate"]]),
    warm_ratep = unname(trend[["Pr(>|t|)"]])
  )
}

files <- list.files(file.path("data-proc", "respiration"), pattern = "_ac.csv$", full.names = TRUE, recursive = TRUE)
stat.climate <- do.call(rbind, lapply(files, site_climate))

#---------------------------------------------------SPECTRAL DATA------------------------------------------------
# MODIS EVI/NDVI/LAI/Fpar/GPP point extractions from NASA AppEEARS
# (`pixi run download-appeears`; see docs/data-provenance.md). Without them
# the spectral predictors are NA, and 03_02 cannot run.
modis_files <- file.path(dir_rawdata, c(
  "towers-MOD13A2-061-results.csv",    # EVI, NDVI
  "towers-MOD15A2H-061-results.csv",   # Fpar, LAI
  "towers-MYD17A2HGF-061-results.csv"  # GPP
))

if (!all(file.exists(modis_files))) {
  message(
    "03_01: MODIS spectral tables not found; EVI/NDVI/LAI/GPP will be NA.\n",
    "  missing: ",
    paste(basename(modis_files[!file.exists(modis_files)]), collapse = ", ")
  )
  data.spectral <- data.frame(
    ID = site_info$site_ID,
    EVI = NA_real_, NDVI = NA_real_, Fpar = NA_real_, LAI = NA_real_, GPP = NA_real_
  )
} else {
  # Each product: drop poor-quality values, average by site and calendar
  # month (floored at 0), fill empty months by interpolation, then average
  # the months.
  file1 <- read.csv(modis_files[[1]])
  poor <- !file1$MOD13A2_061__1_km_16_days_VI_Quality_MODLAND_Description %in%
    c("VI produced, good quality", "VI produced, but check other QA")
  file1$MOD13A2_061__1_km_16_days_EVI[poor] <- NA
  file1$MOD13A2_061__1_km_16_days_NDVI[poor] <- NA
  file1$Date <- as.Date(file1$Date, format = "%m/%d/%y")
  file1_interp <- file1 |>
    dplyr::mutate(month = lubridate::month(Date)) |>
    dplyr::group_by(ID, month) |>
    dplyr::summarise(NDVI = max(mean(MOD13A2_061__1_km_16_days_NDVI, na.rm = TRUE), 0.0),
                     EVI = max(mean(MOD13A2_061__1_km_16_days_EVI, na.rm = TRUE), 0.0), .groups = "drop") |>
    dplyr::group_by(ID) |>
    dplyr::mutate(NDVI_interp = zoo::na.approx(NDVI, rule = 2), EVI_interp = zoo::na.approx(EVI, rule = 2))

  file2 <- read.csv(modis_files[[2]])
  poor <- file2$MOD15A2H_061_FparLai_QC_MODLAND_Description == "Other Quality (back-up algorithm or fill values)"
  file2$MOD15A2H_061_Fpar_500m[poor] <- NA
  file2$MOD15A2H_061_Lai_500m[poor] <- NA
  file2$Date <- as.Date(file2$Date, format = "%m/%d/%y")
  file2_interp <- file2 |>
    dplyr::mutate(month = lubridate::month(Date)) |>
    dplyr::group_by(ID, month) |>
    dplyr::summarise(Fpar = max(mean(MOD15A2H_061_Fpar_500m, na.rm = TRUE), 0.0),
                     Lai = max(mean(MOD15A2H_061_Lai_500m, na.rm = TRUE), 0.0), .groups = "drop") |>
    dplyr::group_by(ID) |>
    dplyr::mutate(Fpar_interp = zoo::na.approx(Fpar, rule = 2), Lai_interp = zoo::na.approx(Lai, rule = 2))

  file3 <- read.csv(modis_files[[3]])
  poor <- file3$MYD17A2HGF_061_Psn_QC_500m_MODLAND_Description == "Other quality (back-up algorithm or fill values)"
  file3$MYD17A2HGF_061_Gpp_500m[poor] <- NA
  file3$Date <- as.Date(file3$Date, format = "%m/%d/%y")
  file3_interp <- file3 |>
    dplyr::mutate(month = lubridate::month(Date)) |>
    dplyr::group_by(ID, month) |>
    dplyr::summarise(GPP = max(mean(MYD17A2HGF_061_Gpp_500m, na.rm = TRUE), 0.0), .groups = "drop") |>
    dplyr::group_by(ID) |>
    dplyr::mutate(GPP_interp = zoo::na.approx(GPP, rule = 2))

  VI <- file1_interp |>
    dplyr::group_by(ID) |>
    dplyr::summarise(EVI = mean(EVI_interp, na.rm = TRUE), NDVI = mean(NDVI_interp, na.rm = TRUE))
  # LAI: unitless
  LAI <- file2_interp |>
    dplyr::group_by(ID) |>
    dplyr::summarise(Fpar = mean(Fpar_interp, na.rm = TRUE), LAI = mean(Lai_interp, na.rm = TRUE))
  # GPP: kgC/m2/8d -> kgC/m2/year
  GPP <- file3_interp |>
    dplyr::group_by(ID) |>
    dplyr::summarise(GPP = mean(GPP_interp, na.rm = TRUE) / 8 * 365.25)
  data.spectral <- VI |>
    dplyr::left_join(LAI, by = "ID") |>
    dplyr::left_join(GPP, by = "ID")
}

#--------------------------------combine soil, climate, spectral, and thermal response strength data together-------------
data.TAS_tot <- read.csv(file.path("data-proc", "analysis", "outcome_temp.csv")) |>
  dplyr::rename("TAS_tot" = "TAS", "TAS_totp" = "TASp")
data.TAS <- read.csv(file.path("data-proc", "analysis", "outcome_temp_water_gpp.csv"))

# Columns by name, so a column added upstream cannot shift what 03_02 gets.
acclimation <- site_info[, c("site_ID", "LAT", "LONG", "ELEV", "IGBP", "Climate_class", "MAP")] |>
  dplyr::left_join(stat.climate[, c("site_ID", "NEE", "NEE_day", "NEE_night", "MATA",
                                    "SSTA", "IATA", "DRTA", "warm_rate", "warm_ratep")], by = "site_ID") |>
  dplyr::left_join(data.spectral[, c("ID", "EVI", "NDVI", "LAI", "GPP")], by = c("site_ID" = "ID")) |>
  dplyr::left_join(stat.soil[, c("site_ID", "SOC")], by = "site_ID") |>
  dplyr::left_join(data.TAS_tot[, c("site_ID", "TAS_tot", "TAS_totp")], by = "site_ID") |>
  dplyr::left_join(data.TAS[, c("site_ID", "TAS", "TASp")], by = "site_ID")

dir.create("data-proc/analysis", recursive = TRUE, showWarnings = FALSE)
write.csv(acclimation, file = file.path("data-proc", "analysis", "acclimation_data.csv"), row.names = FALSE)
