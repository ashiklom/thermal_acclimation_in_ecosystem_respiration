# The growing year, and sites whose season crosses New Year

Almost everything in step 01 and step 02 is indexed by **growing year** rather
than calendar year: the data-gap scan that qualifies a year, the control-year
choice, the 14-day windows the model fits in, and the year factor in the
across-year regression that identifies thermal acclimation. At a
northern-hemisphere site the two coincide, and the distinction never surfaces.

At a site whose growing season runs across 1 January it does. AU-Tum's season
is roughly DOY 185 to DOY 14 of the following year; expressed in calendar day
of year that is the interval `[185, 366] ∪ [1, 14]`, which is not an interval
at all. `between(DOY, gStart, gEnd)` — the test used in a dozen places — cannot
express it, and a season "from 185 to 14" is empty.

## The wrapped frame

The readers therefore put such a site into a shifted day-of-year frame, as the
original workflows did:

```
DOY < origin  ->  DOY + 366
```

With `origin = 183`, DOY runs 183…548, 1 January is 367, and AU-Tum's season is
the ordinary interval `[185, 380]`. Three functions in
`R/respiration_helpers.R` define the frame and are the only places that know
about it:

- `wrap_growing_doy(doy, origin)` — into the frame, at both readers.
- `unwrap_growing_doy(doy)` — back out, wherever a real calendar date is
  needed: building the season-boundary timestamps the gap scan compares
  against, and handing season starts to REddyProc, which wants a `yday`.
- `growing_year_of(doy, year)` — the invariant everything else rests on. A
  wrapped `DOY > 366` if and only if the row falls in the calendar year *after*
  the one its growing year began in, so the growing year is `YEAR` for
  `DOY <= 366` and `YEAR - 1` otherwise. At an unwrapped site DOY never exceeds
  366 and this is the identity.

The shift is 366 whatever the year's length, as in the original. In a non-leap
growing year the wrapped series therefore steps 365 → 367 over New Year and
DOY 366 is empty. No day is lost, only labelled one higher than the elapsed-day
count; every consumer is a `between()` window or a by-DOY average, both of
which tolerate the skip. A year-length-dependent shift would be worse: the same
calendar date would get different DOYs in different years, and the by-DOY
climatologies that set `tStart`/`tEnd` would smear.

## Why it is declared, not inferred from latitude

`growing_year_start` is a column in `data-core/site_info.csv`, empty at all but
two sites. It is **not** `LAT < 0`, and the difference is load-bearing:

| site | LAT | wrapped? | why |
| --- | --- | --- | --- |
| AU-Tum | −35.7 | yes | temperate, real seasonality, season crosses New Year |
| ZA-Kru | −25.0 | yes | as above; raw data not currently obtainable |
| BR-Ma2 | −2.6 | no | equatorial, no seasonality; `gStart`/`gEnd` pinned to 1/366 |
| BR-Sa1 | −2.9 | no | as above |

Wrapping BR-Ma2 or BR-Sa1 would move the data into 183…548 while leaving their
declared bounds at 1–366, so the windows would tile only July–December and half
the record would fall outside every window. The original workflows wrapped by
an explicit site list for exactly this reason — and wrapped no AmeriFlux site
at all, which is what the two Brazilian sites are.

## What a new wrapped site needs

1. `growing_year_start` in site_info.csv — 183 for a southern-hemisphere site,
   or whichever DOY puts the season's trough at the frame boundary.
2. Nothing else, in principle. `gStart`/`gEnd` overrides, if the site needs
   them, are given in the wrapped frame (AU-Tum's `gEnd = 380` is 14 January).
3. A check of what it costs. A wrapped site loses one growing year at each end
   of its record — the first began before the data start, the last runs past
   their end — which `total_tas_site()` trims.

## One known residual, inherited

`total_tas_site()` attaches the previous day's daytime NEE — the GPP proxy the
direct model uses — by `DOY_gpp = DOY - 1` for the pre-noon half of each night.
In the wrapped frame the series steps 365 → 367 over New Year, so at a wrapped
site the 1 January mornings ask for DOY 366:

- in a non-leap calendar year there is no such row, and those half-hours drop
  out of the direct model;
- in a leap year there *is* one — 31 December of the same calendar year, which
  belongs to the next growing year — and they join to it, a year off.

Measured on AU-Tum (2002–2026, 206 424 rows): 204 rows dropped, 60 rows
mis-joined, 0.13 % of the record, confined to the pre-noon half of one calendar
day per year, and only in the direct model. The total model does not read the
column.

This is inherited verbatim from the original workflows, which wrapped the same
way and used the same `DOY - 1`. It is left alone rather than fixed because the
fix — deriving the previous day from the calendar date instead of from DOY —
changes a line every site runs through, for an effect this size. Worth doing
alongside the next deliberate change to that block, not on its own.
