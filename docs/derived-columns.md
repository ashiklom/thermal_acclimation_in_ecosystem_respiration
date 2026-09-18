# Derived soil temperature and soil water columns

Soil temperature and soil water are the two drivers this analysis is actually
about, and at many sites neither is simply measured. Roughly a third of the
sites need soil temperature reconstructed from something else, and half have no
usable soil water at all. This describes where each column comes from, which
one a model run uses, and why the choice is made where it is.

## The columns

`prep_nee_ac()` returns `ac` (the full half-hourly record) and `nightNEE` (the
high-quality nighttime subset). Both carry every variant the site offers:

| column | source |
|---|---|
| `TS_measured` | soil temperature as step 01 established it — measured, or reconstructed per site (see below) |
| `TS_linear` | a `TS ~ TA` regression, present only at sites declaring `ts_col = "TS_linear"` |
| `TS` | the column a run selected; equal to `TS_measured` on the way out of step 01 |
| `SWC_measured` | soil water from the flux tower, in **percent** |
| `SWC_era5` | soil water from ERA5-Land reanalysis, rescaled to percent on read |
| `SWC` | the column a run selected |

`ts_bounds` is a third element of the returned list: one row per available `TS`
column, carrying the 2.5/97.5 percentiles of growing-season soil temperature
for *that column*.

## Why bounds travel with the column

`tStart`/`tEnd` gate the window-skip test in `total_tas_window()`: a two-week
window whose mean soil temperature falls outside them is dropped. They are
percentiles of the soil temperature column, so bounds computed from one column
describe nothing about another.

This used to be a live defect. The second step replaced `TS` and then
recomputed the bounds inline, and getting that ordering wrong — or forgetting
the recompute — left the skip test comparing against a range the data no longer
occupied. The effect is not subtle: at US-Kon the bounds move from
18.47/26.73 °C on measured soil temperature to 12.93/31.33 °C after
substitution. Keying the bounds by column name makes choosing a column and
choosing its bounds one act instead of two.

## Which column a run uses

Soil temperature is a per-site property, declared in `site_info.csv`:

- `ts_col` — `TS_measured` (82 sites) or `TS_linear` (35 sites).
- `ts_linear_domain` — the rows the regression is fitted on: `ac` (34 sites,
  restricted to TA > 0) or `night` (US-Tw1 only, fitted on the nighttime table
  because the original noted that "slope will be too low if using ac data for
  the subtropical wetland sites").

Soil water is **not** a single per-site column, because the choice depends on
the model as well as the site:

| | total model | direct model |
|---|---|---|
| `SWC_use = YES` | `SWC_measured` | `SWC_measured` |
| `SWC_use = NO` | none — soil water is absent from the formula | `SWC_era5` |

`BRM_FORMULA_DIRECT` has soil water in it and needs a value everywhere;
`BRM_FORMULA_TOTAL` does not mention it. So the fallback to reanalysis is a
property of the run, and lives in `default_swc_col(site_info, direct)`.

Both `total_tas_site()` arguments can be overridden — `ts_col=`, `swc_col=` —
which is the point of keeping the columns side by side: a sensitivity run can
fit the same site both ways and compare, without re-preparing the data. Note
that many sites flagged `SWC_use = NO` still *report* a soil water column that
the analysis deliberately discards (CH-Dav has 326,351 non-missing values of
it), so measured-versus-reanalysis is a comparison you can now actually make.

### One asymmetry worth knowing

Measured soil water is treated as a requirement: nighttime observations that
lack it are dropped, which is substantial — DE-Tha goes from 66,874 nighttime
rows to 21,821. The ERA5 fallback is deliberately *not* filtered on, because it
is a daily reanalysis with its own gaps and filtering on it would discard
observations the original kept. That asymmetry is inherited, not chosen.

## How `TS_measured` comes to exist

In order of application, and all in step 01:

- **CZ-Stn** — the shallow sensor is incomplete; the second depth is used instead.
- **FI-Sod** — the shallow sensor is unreliable before 2006 and is
  reconstructed by chaining two regressions between the sensor depths. See
  `recalibrate_fi_sod_soil_temp()`; the fitting periods are expressed as dates
  because the original's row indices did not survive a re-download.
- **GF-Guy** — air temperature is substituted, so that all tropical sites use
  bottom air temperature.
- **FR-Fon, CH-Dav, DE-Akm, DE-Hte, FR-Bil, FR-Pue, FR-FBn, CZ-RAJ** —
  predicted by `fix_soil_temp()`: a random forest on air temperature and net
  radiation where net radiation is available (`netrad_column` in
  `site_info.csv`), a two-predictor regression at two sites with short records,
  and a `TS ~ TA` fit otherwise.
- **AmeriFlux sites with no usable soil temperature** —
  `SITES_TS_FROM_TA_RECENT` and `SITES_TS_FROM_TA_COLD` replace the column
  wholesale from a `TS ~ TA` fit, differing only in the rows fitted on. US-Cwt
  imports coefficients from a nearby site of the same IGBP class, and US-MBP
  fills only its gaps.

`SITES_TS_FROM_TA_*` and `ts_col = "TS_linear"` are disjoint by construction,
and there is a test for it: a site in both would have its soil temperature
regressed twice, from two different fits.

## Ordering, and why it is load-bearing

Every step-01 filter — the QC flag test, the data-gap scan, the
growing-season detection, the TS ≥ 2 °C truncation at CH-Dav/US-Ha1/US-GLE —
runs on **measured** soil temperature. `prep_nee_ac()` asserts that `TS` still
equals `TS_measured` when it returns, precisely so that this cannot drift.

Estimated columns are added afterwards and selected in step 02. This preserves
the original's ordering, and the ordering matters: filtering on a regressed
column would keep a different set of rows. It also leaves a real inconsistency
visible rather than hidden — at US-GLE the TS ≥ 2 °C truncation raises `tStart`
to 2 °C on the measured column, and the substituted column's own lower bound is
**−2.26 °C**, because the regression was never truncated. That is inherited
behaviour, flagged rather than changed.

Two more collisions of the same kind, both inherited:

- **FI-Sod** reconstructs pre-2006 soil temperature in step 01 — an expensive
  two-stage chain between sensor depths — and then, being a `TS_linear` site,
  discards it in favour of a `TS ~ TA` fit.
- **US-MBP** fills soil temperature *gaps* from air temperature in step 01, and
  then has the whole column replaced by a differently-fitted regression.

None of these is obviously wrong — they may well be intentional — but none was
visible before the columns were separated, and all three are worth putting to
the authors.

## Verifying a change here

```bash
pixi run ts-baseline    # 12 sites, one per code path
```

`tests/fixtures/ts-swc-baseline.csv` freezes distribution digests of both
tables, before and after the step-02 manipulation, for both model variants.
`tests/ts-swc-baseline.R` carries a verbatim transcription of that manipulation
as it stood at `b077bb9`, deliberately calling nothing in `R/`, and checks that
the current selection path still reproduces it. Step 01 is cached per site,
keyed by a digest of everything under `R/`, so any pipeline edit invalidates
the cache rather than verifying stale results.

The random-forest branch of `fix_soil_temp()` is **not** covered: it needs one
of the eight `netrad_column` sites, none of which is in the baseline set.
