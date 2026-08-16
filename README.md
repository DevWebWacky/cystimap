
# cystimap

<!-- badges: start -->

<!-- badges: end -->

**cystimap** is an R package for reproducible spatial risk mapping. It
provides a complete, disease-independent and discipline-independent
workflow, from raw point observations to publication-ready maps,
covering study area preparation, spatial interpolation, risk
classification, scenario and sensitivity analysis, consensus mapping,
administrative summaries, and reproducible export.

It was developed to support epidemiological and public health risk
mapping, but the workflow applies equally to environmental health,
veterinary epidemiology, ecology, agriculture, conservation, and
disaster risk assessment, any field requiring raster-based spatial
analysis.

## Why cystimap?

Existing R spatial tools (`gstat`, `automap`, `classInt`, `terra`) each
solve one piece of the puzzle, but a full risk-mapping workflow
typically means gluing them together by hand, re-writing the same
validation and provenance logic each time. cystimap instead offers:

- **One continuous object** that carries the study area, data, surfaces,
  classifications, and scenario results through the entire pipeline, so
  every stage can build on the last with a single consistent interface.
- **Automatic and Manual control modes** for spatial interpolation
  (Ordinary Kriging), so users can let the package select and validate
  variogram models, or supply their own. Guided mode is planned for a
  future release.
- **A built-in scenario framework**, generate alternative analytical
  scenarios, measure their agreement (Kappa, Jaccard, exact agreement),
  evaluate sensitivity to analytical choices, and identify consensus
  areas that remain important across assumptions.
- **Full provenance logging**, every automatic decision is recorded, so
  results and figures can be traced back to exactly how they were
  produced.
- **Never guesses a CRS**, every function requires an explicit or
  detectable coordinate reference system, never a silent assumption.

## Installation

You can install the development version of cystimap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("devwebwacky/cystimap")
```

## Workflow overview

cystimap follows a linear but independently-usable workflow:

1.  **`prepare_study_area()`** : define the analytical boundary, CRS,
    raster template, mask, and prediction grid
2.  **`read_data()`** : read spatial or non-spatial data from common
    formats
3.  **`create_indicator()`** : create an indicator, attach spatial
    locations, and aggregate to one value per location
4.  **`smooth_surface()`** : interpolate point observations into a
    continuous raster surface (Ordinary Kriging, IDW, Nearest Neighbour,
    or validate an existing raster)
5.  **`classify_surface()`** : convert a continuous surface into binary,
    multiclass, or sensitivity-analysis classes
6.  **`combine_surfaces()`** : combine classified surfaces into a single
    analytical layer (overlap, weighted, additive, logical rules)
7.  **`define_scenarios()` > `run_scenarios()` >
    `scenario_sensitivity()` → `scenario_agreement()` >
    `scenario_consensus()`** : build alternative analytical scenarios,
    execute them, and evaluate their sensitivity, agreement, and
    consensus
8.  **`admin_summary()`** : summarise any raster output within
    administrative or other polygons
9.  **`map_surface()`** : produce a publication-quality map
10. **`export_results()`** : export all rasters, tables, logs, and a
    reproducibility manifest

See `vignette("cystimap-workflow")` for a complete worked example.

## Example

``` r
library(cystimap)
library(sf)

nc <- st_read(system.file("shape/nc.shp", package = "sf"))

result <- prepare_study_area(
  boundary = nc, crs = 32119, resolution = 5000, dissolve = TRUE
)

result <- read_data(result, file = "cases.csv", name = "cases")

result <- create_indicator(
  result, data_name = "cases", indicator_name = "case_rate",
  formula = cases / population * 1000,
  coords = c("lon", "lat"), coords_crs = 4326
)

result <- smooth_surface(
  result, indicator_name = "case_rate", surface_name = "risk_surface",
  method = "ordinary_kriging", mode = "automatic", cv_method = "loo"
)

result <- classify_surface(
  result, surface_name = "risk_surface", classified_name = "risk_binary",
  type = "binary", cutoff_method = "quantile", cutoffs = 0.5
)

map_surface(result, raster_source = "surfaces", raster_name = "risk_surface")
```

## Project status

cystimap is under active development. The core workflow (Functions 1-3,
6-9, 13-15) is implemented and tested. Several advanced methods
e.g. Universal/Regression Kriging, additional cross-validation designs,
weighted consensus, automated Word/PDF reporting are planned for future
versions — see `NEWS.md` for details as the project develops.

## License

MIT
