# Known issues

These are known and deliberately left alone,
either because they are inherited from the manuscript's code
or because fixing them would change results and needs its own decision.
Each entry says where it lives.

## Pipeline

- **`DOY_gpp` at wrapped sites.**
  At the two sites whose growing year crosses New Year (AU-Tum, ZA-Kru),
  the previous day's daytime NEE is looked up as `DOY - 1`.
  That drops, or mis-joins, the 1 January mornings:
  0.13 % of AU-Tum's record, in the direct model only.
  Inherited from the original workflows.
  See docs/growing-year.md, "One known residual".
- **AmeriFlux reprocessings go undetected.**
  AmeriFlux publishes no BASE version string,
  so the update scan compares published year spans.
  A reprocessing that re-releases the same years is not noticed.
  See docs/data-provenance.md.
- **Revised overlapping years are ignored.**
  The splice keeps the earlier product wherever two overlap,
  so a new release changes a record only after the earlier product's end.
- **New site-years skip manual QC.**
  `year_removed`, `gStart`/`gEnd` and the gap thresholds were set by hand
  against the manuscript's years.
  `data-proc/analysis/new_siteyears.csv` lists the later ones for review.
- **Dependencies outside conda-forge.**
  `amerifluxr`, `REddyProc`, `gslnls` and a working `base64url`
  come from `pixi run deps` (`dependencies.R`),
  so they are not locked by `pixi.lock`.

## `workflows/`

- **03_01 stops on the current MODIS tables.**
  Where a site has no good-quality value of an index at all (DE-Akm's Fpar),
  its monthly series is all NA
  and `zoo::na.approx(..., rule = 2)` fails the whole script.
  It needs a rule for such sites (NA, or drop the index there).
- **03_01: the CH-Aws cut-off is hard-coded** (`YEAR >= 2015`).
- **03_01 → 03_02: a site without a TAS is not filtered out.**
  03_01 left-joins the two TAS tables,
  so a site missing one reaches `randomForest()`,
  whose default `na.fail` rejects it.
  03_02 now catches that per fit and warns, but the join is unchanged.
- **04_02:**
  - The result columns start at 0 rather than NA,
    so a site that was never processed looks like a real zero.
    The run report filters those out.
  - `YEAR[1:3]` assumes at least three qualifying years.
    A site with fewer gets an NA control year, an empty data set, and a `brm` error.
  - `-which.min()` on a site whose ratios are all NA selects nothing and yields NaN.
  - `zoo::na.approx()` drops leading and trailing NAs,
    which can cause a length-mismatch error.
  - No seed is set for the Stan fits.
  - It has not been checked whether the pipeline's wrapped `gStart`/`gEnd`
    (which can exceed 366)
    and the DOY in `_ac.csv` agree at AU-Tum and ZA-Kru.
