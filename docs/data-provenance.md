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

### Known shortfall

**IT-Noe** is absent from Warm Winter 2020 altogether, and both current products
start in 2021/2022, so it reconstructs to roughly 5 of the 11 years the
manuscript used. Its pre-2021 history would have to come from somewhere else
(Drought-2018, or the European Fluxes Database). Everything else reconstructs to
at least the original's year count.

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
pixi run R -e 'targets::tar_source(); download_site("DE-Tha", overwrite = TRUE)'
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
prep_nee_ac()  ->  site_data target  ->  site_tas_total / site_tas_direct
```

In `_targets.R` each site's `site_dl` target is `format = "file"` over the paths
`download_site()` returns, so re-downloading a product changes the file
fingerprint and invalidates that site's `site_data` and its model fits — and
only that site's. Two caveats on that, both open issues rather than settled
behaviour:

- `tar_option_set(cue = tar_cue("never"))` is currently set globally, which
  disables invalidation entirely. Until that is scoped to the expensive targets
  only, a re-download will *not* trigger a rebuild.
- `site_info.csv` is read at pipeline-construction time rather than as a file
  target, so editing a `source` string is invisible to the dependency graph.

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
