# Flux data provenance, and how to keep it current

## Why a site has more than one source

No single published product covers the full record at most of these sites, so
`site_info.csv`'s `source` column is an **ordered, `+`-separated provenance
list**, oldest product first:

```
DE-Tha   FLUXNET+ICOS
GF-Guy   WW2020+FLUXNET+ICOS
US-Kon   AmeriFlux_BASE
AU-Tum   TERN
```

The original manuscript code did the same thing with underscore-joined strings
(`FLUXNET2025_ICOS2025`), for the same reason.

The reason it matters is specific to ICOS. ICOS publishes **two different**
ecosystem products:

| Product | What it is | Coverage |
|---|---|---|
| `ICOS` | "Ecosystem final quality (L2) product in ETC-Archive format", datatype `etcL2Fluxnet` | **Only the period since the station was ICOS-labelled** |
| `FLUXNET` | FLUXNET-Archive product, served by `fluxnet-shuttle` (and by ICOS as `miscFluxnetArchiveProduct`) | The full merged record |
| `WW2020` | Warm Winter 2020, release 2022-1 | 1989–2020, 73 stations, incl. non-ICOS |

The L2 product is keyed to **labelling**, not to how long a tower has run. ICOS
describes each release by its labelled-station count, and that count has grown
29 → 61 → 73 → 80 across the 2021-1 to 2025-1 releases. A station labelled in
2019 therefore has an L2 product that starts in 2019 however long it has been
measuring. Fetching only that product truncated 21 of these 26 records — DE-Tha
to 7 years against the 28 the manuscript used, NL-Loo to 4 against 16.

The `FLUXNET` product is also the better input on its merits. Its
`*_FLUXMET_HH_*.csv` carries every column the pipeline needs under the exact
FLUXNET2015 names, including a real `NIGHT` flag and real `TS_F_MDS_1_QC`. An
earlier version of `scripts/download-icos.py` rebuilt a FLUXNET-shaped table out
of three separate ICOS L2 products, which meant computing `NIGHT` from a
shortwave threshold (`SW_IN_F_MDS <= 20`) and fabricating QC flags from value
presence — both of which then fed the QC filters in `prep_nee_ac()`. That
normalisation is gone; the archives are now saved and read as shipped.

`ICOS` is still spliced on top of `FLUXNET` because it reaches later at some
sites: UK-AMo's shuttle product ends 2024 while its L2 product reaches 2026.

### Splice rule

`read_spliced_products()` in `R/prepare-site-data.R` uses the original
workflow's rule: sort the products by first timestamp, then append each later
product **only from the first timestamp after the running record's end**. The
earlier, longer-history product therefore wins wherever two overlap. Products
are re-sorted by actual first timestamp rather than trusting the declared order,
so a mis-ordered `source` string cannot silently truncate a record.

The comparison is on the 12-digit timestamp *strings*, so the tables are read
under `FLUXNET_COL_TYPES` (`R/constants.R`), which declares
`TIMESTAMP_START`/`TIMESTAMP_END` as character and everything else as double.
That is a contract, not a convenience: under type guessing the timestamps come
back as doubles and have to be converted back, and a column that is `-9999` for
its entire length -- these files are full of them -- can be typed `logical`,
which would make the `TS >= 2 C` filter compare against a logical NA. Sentinel
removal stays a numeric `dat[dat == -9999] <- NA` so that a future release
writing `-9999.0` is still caught.

### FLUXNET2015, and why it is acquired by hand

`FLUXNET2015` is the last static FLUXNET release (through 2014). It is the only
product here with no programmatic interface: downloading requires an
interactive login at fluxnet.org plus acceptance of the FLUXNET2015 Data
Policy, which is granted per site-year (Tier 1 vs Tier 2). The FLUXNET Shuttle
does have an API but federates AmeriFlux, ICOS and TERN only, none of which
hold the pre-2015 record. `download_fluxnet2015()` in `R/download.R`
therefore prints the procedure and the exact target directory rather than
attempting a download; `check-data-updates.py` checks only that it is present,
since a closed release cannot update.

Unzip into `data-raw/FLUXNET2015/<site>/`, keeping the archive alongside the
extracted tables.

### FI-Sod: the one record with a hole in the middle

FI-Sod needs all three of its products and still has a gap:

| period | source | status |
|---|---|---|
| 2001–2014 | FLUXNET2015 FULLSET | held |
| 2015–2022 | — | **no public product** |
| 2023–2025 | ICOS ETC-Archive L2 (shuttle copy stops at 2024) | held |

The gap is a publication gap, not necessarily a measurement one. FI-Sod was
ICOS-labelled on **2023-05-23**, and ICOS publishes from just before labelling
— every ICOS object for the station, current or deprecated, starts
2022-12-31. FLUXNET2015 closed at 2014. FI-Sod is **absent from Warm Winter
2020** (73 members, checked directly), so that route is closed too. Anything
for 2015–2022 would have to come from the site PI at FMI or a national
archive.

Even so, the site now reconciles well: gStart 124 and gEnd 270 match the
manuscript exactly, and it yields 12 qualifying years against the
manuscript's 10.

### Known shortfall

**IT-Noe** is absent from Warm Winter 2020 altogether, and both current products
start in 2021/2022, so it reconstructs to roughly 5 of the 11 years the
manuscript used. Its pre-2021 history would have to come from somewhere else
(Drought-2018, or the European Fluxes Database). Everything else reconstructs to
at least the original's year count.

### Non-flux inputs

Four datasets outside the flux archives feed the downstream analysis. Three now
have downloaders and targets; one does not, and that is deliberate.

| input | target | source |
|---|---|---|
| AmeriFlux BADM/BIF | `ameriflux_bif_file` | `amerifluxr::amf_download_bif()`, CC-BY-4.0 |
| FAO GSOC v1.5.0 | `gsoc_file` | FAO Google Cloud bucket, ~760 MB |
| WorldClim 2.1 tmin | `worldclim_files` | geodata.ucdavis.edu, ~4.8 GB |
| MODIS EVI/NDVI/LAI/GPP | *none* | NASA AppEEARS — see below |

Two details worth keeping. The BIF filename carries the date it was produced and
arrives as `.xlsx`, so `03_01` discovers it by pattern rather than naming it —
it used to hard-code a datestamp that no longer existed. And the GSOC raster is
saved as `GSOCmap1.5.0.tif` rather than under FAO's own name because `03_01`
indexes the extraction by layer name (`terra::extract(...)$GSOCmap1.5.0`), which
terra derives from the file.

The WorldClim baseline is the CRU-TS-downscaled **monthly series for 2000-2020**,
not the 1970-2000 climatology that `wc2.1_2.5m_tmin.zip` holds. `04_01`'s glob
accepts either, but its comment specifies 2000-2020, and substituting the
climatology would shift every projected temperature change by the warming
between the two baselines without any error being raised.

### MODIS, and why it is not downloaded

`03_01` uses NDVI, EVI, LAI, Fpar and GPP from three AppEEARS point extractions:

```
data-raw/towers-MOD13A2-061-results.csv     EVI, NDVI
data-raw/towers-MOD15A2H-061-results.csv    Fpar, LAI
data-raw/towers-MYD17A2HGF-061-results.csv  GPP
```

AppEEARS is not a file server. It needs an Earthdata Login and an asynchronous
submit/poll/download cycle, and a request has to name the products, layers, the
117 site coordinates and a date range. Rather than half-implement that, `03_01`
emits `NA` for those five predictors when the tables are absent and says so.

The knock-on is worth stating plainly: `03_02` calls `randomForest()` with the
default `na.action = na.fail` and `LAI` is one of its five predictors, so the
driver analysis cannot run until these are supplied. To fill the gap by hand,
submit an AppEEARS *point* sample for the coordinates in `data-core/site_info.csv`
over the study period for the three products above, request CSV output, and drop
the results into `data-raw/` under the names shown.

---

## The monthly check

```bash
pixi run check-updates
```

That runs `scripts/check-data-updates.py`, which for every site and every
product in its provenance list compares the archive on disk against what the
provider publishes right now, and prints one line per product that needs
attention:

```
  UPDATE   DE-Tha   ICOS      ICOSETC_DE-Tha_..._2020-2025_v1.3_r1.zip -> ..._2020-2026_v1.4_r1.zip
  MISSING  FR-Fon   WW2020    -> FLX_FR-Fon_FLUXNET2015_FULLSET_2005-2020_beta-3.zip
  ?        IT-Noe   WW2020    not published / not queryable
```

Statuses are `ok`, `UPDATE`, `MISSING` and `?`. Exit status is 0 only when
everything is `ok`, so this works as a cron or CI check.

How each product is compared:

- **ICOS** — queried live from the ICOS metadata service. Archive filenames
  encode span, version and release
  (`ICOSETC_DE-Tha_FLUXNET_FLUXMET_HH_2020-2025_v1.3_r1.zip`), so a string
  comparison is a reliable change detector. New releases appear roughly
  annually and also add newly-labelled stations — which is the case worth
  watching, because a newly-labelled station means a *longer* L2 product.
- **FLUXNET** — compared against a `fluxnet-shuttle` snapshot. The snapshot
  goes stale, and a stale snapshot makes the check meaningless, so its age is
  printed and flagged past 45 days. Refresh it in the same run:

  ```bash
  pixi run check-updates -- --refresh-snapshot
  ```
- **WW2020** — a closed 2022 release. Checked for membership and filename, but
  an `UPDATE` here would mean the collection itself was revised.

### Acting on the result

```bash
# one product for one site
pixi run download-icos -- --product icos   --sites DE-Tha --overwrite
pixi run download-icos -- --product ww2020 --sites GF-Guy
./scripts/download-fluxnet.sh --sites DE-Tha --overwrite

# or let the pipeline fetch whatever a site declares
pixi run R -e 'targets::tar_source(); download_site(get_site_info("DE-Tha"), overwrite = TRUE)'
```

`download_site()` walks the site's provenance list, calls the right downloader
for each product, and fails loudly if a download reports success but leaves
nothing the readers can find.

If a station has just been labelled, or its span has grown, also re-check
whether it still needs `WW2020` underneath — `scripts/audit-icos-coverage.R`
prints span-by-span coverage against the manuscript's year counts and says
`RECOVERED` or `short` per site.

---

## How this reaches the pipeline

```
data-raw/<PRODUCT>/<site>/…          downloaded archive + extracted HH table
        │
        ├─ product_file(site, product)         R/utils.R, one lookup for readers
        │                                      and downloaders alike
        ▼
read_spliced_products(site_info)     R/prepare-site-data.R, splices in order
        ▼
prep_fluxnet_family() / prep_ameriflux()
        ▼
prep_nee_ac(site_info)  ->  site_data  ->  site_tas_total / site_tas_direct

site_info.csv (site_info_file, a file target)
        ▼
get_site_info(site, path = site_info_file)  ->  site_info target, per site
        └─ threaded into download_site(), prep_nee_ac(), total_tas_site()
```

In `_targets.R` each site's `site_dl` target is `format = "file"` over the paths
`download_site()` returns, so re-downloading a product changes the file
fingerprint and invalidates that site's `site_data` and its model fits — and
only that site's. The two caveats that used to sit here are both closed:

- The global `tar_option_set(cue = tar_cue("never"))` is gone, so invalidation
  actually happens. What keeps a run affordable instead is running fewer sites:
  `pipeline_sites()` returns the six-site `DEV_SITES` sample by default, and
  `THERMAL_SITES=all` restores the full list.
- `site_info.csv` is a `format = "file"` target (`site_info_file`), read once
  per site into a `site_info` target that every later stage takes as an
  argument. Editing a `source` string now invalidates exactly the sites whose
  row could have changed — and nothing else.

One read that cannot be a target: `pipeline_sites()` itself, because `tar_map()`
needs the site names while the pipeline is being *constructed*, before any
target runs. Adding or removing a site from `DEV_SITES` therefore changes the
shape of the graph rather than invalidating a target in it.

After a refresh, the check that the data is still sane is:

```bash
pixi run test        # unit tests, ~10 s
pixi run reconcile   # step-01 outputs vs the manuscript's, ~7 min for 8 sites
```

`reconcile` reports a diff rather than asserting equality, since the provenance
change is expected to move things; what it is for is making every difference
visible and attributable.

### If you are updating the provenance list itself

`source` is generated, not hand-edited. Change `scripts/revise-site-info.R` and
re-run it; it rebuilds `data-core/site_info.csv` from
`data-core/site_info_orig.csv` and asserts its own invariants. Then re-run the
two checks above.

### Cleaning up after the old ICOS downloader

The previous script wrote a hand-normalised `data-raw/ICOS/<site>/<site>_ICOS_L2_FLUXNET_HH.csv`.
Those files no longer match any reader pattern and are simply ignored, but they
are dead weight and easy to mistake for current data:

```bash
find data-raw/ICOS -name '*_ICOS_L2_FLUXNET_HH.csv' -delete
```
