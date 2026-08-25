# US-CMW SWC Ambiguity

Document the US-CMW AmeriFlux soil water content ambiguity in `_agent-docs/cmw-ambiguities.html`.
The issue concerns which raw AmeriFlux SWC column should be configured for US-CMW.
Use the other HTML workflow documentation to explain how the explicit site metadata mapping is consumed.

The analysis scripts are:

- `scripts/audit-cmw-swc-options.R` audits `SWC_1_6_1`, `SWC_1_7_1`, and their row-wise mean after 2000.
- `scripts/plot-cmw-swc-options.R` draws the comparison figures under `figures/`.

The local archive contains `SWC_1_6_1` and `SWC_1_7_1`, while the former configured field, `SWC_PI_F_1_1_A`, is absent.
After the production workflow's `YEAR > 2000` truncation, both candidate fields are present in 93.39% of records and both are present together in 343,831 records.
Their correlation is 0.593, and the mean difference `SWC_1_6_1 - SWC_1_7_1` is 0.798.

The general AmeriFlux naming convention indicates that `SWC_1_6_1` and `SWC_1_7_1` share the same horizontal profile and replicate index but have different vertical/depth ranks.
The site-specific depths and sensor setup could not be confirmed from the locally bundled BADM workbook, so this interpretation remains provisional.
These fields should therefore not be silently averaged: they are likely measurements at different depths rather than redundant sensors at one depth.

Other sites select one explicit sensor-level SWC field in `data/site_info.csv`.
No existing site uses a documented multi-SWC notation or NA-omitted cross-sensor average.
The code can use an aggregate field such as ICOS `SWC_F_MDS_1`, or an external ERA5-Land soil-moisture product for sites configured with `SWC_use=NO`, but those are different source conventions.

Decision and implementation:
Use `SWC_1_6_1` for US-CMW for now because it is the shallower-index candidate, and do not average it with `SWC_1_7_1`.
The active `data/site_info.csv` mapping is updated accordingly.
The demo metadata copy is intentionally unchanged because this request applies to the active site metadata only.
The figures and audit CSVs are generated analysis artifacts and are not part of the documentation commit.

Follow-up:
Verify the actual sensor depths and setup from BADM, the AmeriFlux Variable Info tool, or the site PI.
If that documentation contradicts the naming-based interpretation, revise the mapping and consider a sensitivity analysis.
