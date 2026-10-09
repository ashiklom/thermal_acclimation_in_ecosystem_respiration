# Known issues and questions

These are known and deliberately left alone, either because they are inherited from the manuscript's code or because fixing them would change results and needs its own decision.
Each entry says where it lives.

## Discussion topics and questions

- **Growing season definition**.
  The manuscript says, "We defined the growing season as the period when Ts was above 2°C, and daily NEP was above 0.8 g C m-2 or above 20% of maximum daily NEP within a year for five consecutive days" (L603-604)
  The code (`R/growing-season.R :: detect_growing_season`):
  (1) Starts from the 7th qualifying DOY, not after 5 consecutive days;
  (2) Uses a Ts cutoff of 0°C, not 2°C;
  (3) Ameriflux sites use **only** the "20% of maximum NEP" rule, without the 0.8 g C m-2 minimum
  (4) FI-Sod and DE-RuC are undocumented special cases that use NEE < 0
  Also, the manuscript says each site-year gets its own growing season definition, but the code calculates (or sets by hand) a single growing season extent for every site across all years (I think the code is correct here, and it's just the manuscript language that needs to be tweaked).

- **Gap threshold**.
  The manuscript says, "For each selected site, we removed years with a single CO2 flux measurement gap longer than one month during the growing season, as long gaps may introduce bias into temperature–ER relationships." (L507-510).
  The code (`R/growing-season.R :: compute_gap_thresholds`) is looser:
  For most sites, the max can be up to 22.5% of the growing season (~45 days for a 200 day growing season).
  For 8 sites, the threshold is 60 days.

- **Rejecting window-years**.
  The manuscript says, "To avoid extrapolation and ensure accurate estimation of Rcontrol and Rtreatment at Tset, we required that:
  (1) the Ts range (2.5th–97.5th quantiles) of Ustar-filtered flux observations encompassed Tset; and
  (2) each window-year contained enough observations (100 points for half-hourly fluxes and 60 points for hourly fluxes)." (L614-617).
  The code uses the 100/60 observation criteria to widen windows but **not** to reject windows (so, e.g., a fully-widened 2-month window can still be fitted if it has more than 25 observations).
  Also, the **TS quantiles are calculated inconsistently** across different sites:
  For sites that use measured soil temperature, quantiles are calculated from the DOY climatology over NEE-uptake days (DOYs whose multi-year mean NEE is below the growing-season cutoff), floored at 0 °C.
  But for sites that use soil temperature estimated via regression, quantiles are calculated from the raw half-hourly values within the growing season (`[gStart, gEnd]`).
  The new code makes this explicit with two recipe axes, `ts_bounds_measured` and `ts_bounds_estimated`: the manuscript is `climatology_uptake` / `halfhourly`, and either can instead be `climatology_uptake` (the `uptake_all` recipe applies it to estimates too), `climatology` (DOY climatology over `[gStart, gEnd]`, not the NEE-uptake days) or `halfhourly` (half-hourly values over `[gStart, gEnd]`), so one definition can be applied to any soil temperature column (e.g., measured TS + half-hourly quantiles).
  Finally, there are a few additional filters not mentioned in the manuscript (probably OK).

- **Window size**
  The manuscript says the growing season is divided into "successive two-week moving windows" / "consecutive moving windows with a window length of two weeks".
  As currently coded, the analysis uses **15-day** moving windows (because `dplyr::between(DOY, window_start, window_end)` is inclusive at both ends), with the last day of each moving window equal to the first day of the next moving window.
  I haven't tested the impacts of this on the results.
  Is this what you want, or is it important for the windows to be strictly non-overlapping?

## Pipeline

- **`DOY_gpp` at wrapped (deep Southern hemisphere) sites.**
  At southern hemisphere sites where the growing year crosses New Year (AU-Tum, ZA-Kru), the previous day's daytime NEE is looked up as `DOY - 1`.
  That drops, or mis-joins, the 1 January mornings: 0.13 % of AU-Tum's record, in the direct model only.
  Inherited from the original workflows.
  See docs/growing-year.md, "One known residual".

- **AmeriFlux reprocessings go undetected.**
  AmeriFlux publishes no BASE version string, so the update scan compares published year spans.
  A reprocessing that re-releases the same years is not noticed.
  See docs/data-provenance.md.

- **Revised overlapping years are ignored.**
  The splice keeps the earlier product wherever two overlap, so a new release changes a record only after the earlier product's end.

- **New site-years skip manual QC.**
  `year_removed`, `gStart`/`gEnd` and the gap thresholds were set by hand against the manuscript's years.
  `data-proc/analysis/new_siteyears.csv` lists the later ones for review.

## `workflows/`

- **03_01 stops on the current MODIS tables.**
  Where a site has no good-quality value of an index at all (DE-Akm's Fpar), its monthly series is all NA and `zoo::na.approx(..., rule = 2)` fails the whole script. It needs a rule for such sites (NA, or drop the index there).

- **03_01: the CH-Aws cut-off is hard-coded** (`YEAR >= 2015`).

- **03_01 → 03_02: a site without a TAS is not filtered out.**
  03_01 left-joins the two TAS tables, so a site missing one reaches `randomForest()`, whose default `na.fail` rejects it. 
  03_02 now catches that per fit and warns, but the join is unchanged.

- **04_02:**
  - The result columns start at 0 rather than NA, so a site that was never processed looks like a real zero.
    The run report filters those out.
  - `YEAR[1:3]` assumes at least three qualifying years.
    A site with fewer gets an NA control year, an empty data set, and a `brm` error.
  - `-which.min()` on a site whose ratios are all NA selects nothing and yields NaN.
  - `zoo::na.approx()` drops leading and trailing NAs, which can cause a length-mismatch error.
  - No seed is set for the Stan fits.
  - It has not been checked whether the pipeline's wrapped `gStart`/`gEnd` (which can exceed 366) and the DOY in `_ac.csv` agree at AU-Tum and ZA-Kru.
