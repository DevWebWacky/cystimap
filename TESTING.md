# Testing Guide for cystimap

Thank you for helping test **cystimap**! This guide will walk you
through installing the package and testing every function, one at a
time, using your own data. You do not need to have used cystimap
before, each section explains what the function does before asking
you to test it.

If anything is confusing, breaks, or behaves unexpectedly, please
report it (see "How to Report Issues" at the end). That is exactly
what this testing round is for.

---

## 1. What is cystimap?

cystimap is an R package for spatial risk mapping. It takes you from
raw point observations (e.g. disease cases, survey data, environmental
measurements) all the way to publication-ready risk maps, through a
single connected workflow:

study area -> data -> indicators -> smooth surface -> classify
-> combine -> scenarios/sensitivity/consensus -> summaries -> maps -> export


Every step is recorded automatically, so results can always be traced
back to exactly how they were produced.

---

## 2. Installation

You need R and RStudio installed. Then, in the RStudio Console:

```r
install.packages(c("devtools", "usethis"))

devtools::install_github("devwebwacky/cystimap", build_vignettes = TRUE)

library(cystimap)
```

If installation fails, copy the **full error message** and report it
(see Section 12). This is itself a useful test, if installation is
confusing or breaks, that is valuable feedback.

### See the full worked example

```r
vignette("cystimap-workflow", package = "cystimap")
```

This opens a complete walkthrough using a built-in North Carolina
dataset. Skim it once before starting your own tests, so you can see
what a normal run looks like.

---

## 3. Before you start: prepare your data

You will get the most useful results by testing with **your own real
data**, not made-up numbers. Before starting, have ready:

- A boundary file for your study area (shapefile, GeoPackage, or
  similar) e.g. a country, region, or district boundary
- A dataset of point observations with coordinates (csv, Excel, etc.)
  — e.g. case locations, survey points, sample sites
- (Optional) An administrative boundary layer (e.g. districts) if you
  want to test area summaries
- (Optional) Any existing raster file you have, to test the
  "existing raster" pathway

If you do not have real data handy for a particular step, that is
fine, note it in your report and skip to the next function.

---

## 4. Function 1: `prepare_study_area()`

**What it does:** Sets up the geographic foundation for everything
else — the boundary, coordinate reference system (CRS), and raster
grid that every later step will align to.

**Important:** cystimap will never guess your CRS. If it cannot
detect one automatically, you must supply it explicitly.

```r
library(sf)
my_boundary <- st_read("path/to/your_boundary.shp")

result <- prepare_study_area(
  boundary   = my_boundary,
  crs        = 32630,      # replace with YOUR projected CRS EPSG code (in metres)
  resolution = 1000,       # grid cell size in metres - adjust to your area size
  dissolve   = TRUE
)

print(result)
```

**Try doing these:**
- Run it with your real boundary and a sensible resolution
- Try leaving out `crs` if your boundary file already has one — does
  it get picked up correctly?
- Try supplying a geographic CRS (like 4326, standard lon/lat)
  together with a `resolution` — this **should fail** with a clear
  error message. If it does not fail, that is a bug, please report it.
- If you have a lake, protected area, or similar zone to exclude, try:
```r
result <- prepare_study_area(
  boundary = my_boundary, crs = 32630, resolution = 1000,
  exclusions = my_exclusion_layer, exclusion_action = "mask"
)
```

**What "correct" looks like:** `print(result)` shows your CRS, and
"Study area" and "Template" both say "set".

---

## 5. Function 2: `read_data()`

**What it does:** Reads your observation data (csv, Excel, shapefile,
etc.) into the workflow, automatically detecting the file type and
aligning any spatial data to your study area CRS.

```r
result <- read_data(result, file = "path/to/your_data.csv", name = "observations")

result$data$observations   # view what was read in
```

**Try doing this:**
- Test with whatever file formats you actually use (csv, xlsx, shp...)
- If your file is spatial (a shapefile of points), confirm it gets
  reprojected to match your study area CRS

**What "correct" looks like:** Your data appears under
`result$data$observations`, with the same number of rows as your
original file.

---

## 6. Function 3: `create_indicator()`

**What it does:** Combines three steps into one: (1) calculates an
indicator variable from your raw columns, (2) attaches spatial
coordinates, and (3) aggregates to one value per location if needed.

```r
result <- create_indicator(
  result,
  data_name      = "observations",
  indicator_name = "my_indicator",
  formula        = your_column_1 / your_column_2 * 1000,  # write your own formula
  coords         = c("lon_column_name", "lat_column_name"),
  coords_crs     = 4326   # the CRS your coordinates are recorded in (usually 4326 for GPS)
)

result$data$indicators$my_indicator
```

**Try this:**
- Use a formula that is actually meaningful for your data (a rate,
  ratio, or standardised score)
- If you have multiple records at the same location, test aggregation:
```r
result <- create_indicator(
  result, data_name = "observations", indicator_name = "agg_indicator",
  formula = your_value_column, coords = c("lon","lat"), coords_crs = 4326,
  aggregate_by = "location_id_column", agg_fun = "mean"
)
```
- Try it with some missing coordinates in your data (if realistic),
  you should see a warning that rows were dropped, not a silent error

**What "correct" looks like:** The result is a spatial (`sf`) object
with your indicator column and correct coordinates.

---

## 7. Function 6: `smooth_surface()`

**What it does:** Turns your point observations into a continuous
raster surface (a map showing predicted values everywhere, not just
at your sample points), using kriging, IDW, or nearest-neighbour
interpolation.

```r
# Simplest: automatic Ordinary Kriging
result <- smooth_surface(
  result, indicator_name = "my_indicator", surface_name = "risk_surface",
  method = "ordinary_kriging", mode = "automatic", cv_method = "loo"
)

result$surfaces$risk_surface$prediction     # the raster surface
result$surfaces$risk_surface$cv_summary     # how reliable is it?
```

**Try doing these:**
- Run automatic Ordinary Kriging on your real data and check
  `cv_summary$correlation`, for real, spatially-structured data this
  should be noticeably better than near-zero
- Try IDW as a comparison:
```r
result <- smooth_surface(result, indicator_name = "my_indicator",
                          surface_name = "idw_surface", method = "idw", power = 2)
```
- If you are comfortable with variograms, try Manual mode:
```r
library(gstat)
my_vgm <- variogram(my_indicator ~ 1, result$data$indicators$my_indicator)
my_fit <- fit.variogram(my_vgm, vgm("Sph"))

result <- smooth_surface(result, indicator_name = "my_indicator",
                          surface_name = "manual_surface", method = "ordinary_kriging",
                          mode = "manual", manual_model = my_fit)
```
- If you have an existing raster (e.g. a NDVI or elevation layer),
  try the existing-raster pathway:
```r
result <- smooth_surface(result, surface_name = "existing_test",
                          method = "existing_raster", existing_raster = "path/to/raster.tif")
```

**What "correct" looks like:** A raster is produced, masked to your
study area boundary (values only inside the boundary, blank outside).

---

## 8. Function 7: `classify_surface()`

**What it does:** Converts a continuous surface into categories (e.g.
"low risk" / "high risk"), using a cut-off method of your choice.

```r
result <- classify_surface(
  result, surface_name = "risk_surface", classified_name = "risk_binary",
  type = "binary", cutoff_method = "quantile", cutoffs = 0.5
)

result$classified$risk_binary$frequencies   # how much area in each class
```

**Things to try:**
- Try at least 3 different `cutoff_method` values: `"quantile"`,
  `"equal_interval"`, `"jenks"`
- Try `sensitivity_mode = TRUE` with several cutoffs, to see separate
  binary rasters for each threshold:
```r
result <- classify_surface(
  result, surface_name = "risk_surface", classified_name = "risk_sensitivity",
  type = "binary", cutoff_method = "quantile",
  cutoffs = c(0.25, 0.5, 0.75), sensitivity_mode = TRUE
)
```

**What "correct" looks like:** The classified raster has whole-number
class values (0, 1, 2...), and `frequencies` shows the area/percentage
per class summing to 100%.

---

## 9. Function 8: `combine_surfaces()`

**What it does:** Merges two or more classified layers into one —
e.g. finding where several risk factors overlap.

```r
# You need at least 2 classified surfaces - create a second one first if needed
result <- classify_surface(result, surface_name = "risk_surface",
                            classified_name = "risk_binary_alt",
                            type = "binary", cutoff_method = "quantile", cutoffs = 0.25)

result <- combine_surfaces(
  result, classified_names = c("risk_binary", "risk_binary_alt"),
  combined_name = "combined_risk", method = "binary_overlap"
)

result$results$combined$combined_risk$frequencies
```

**try:**
- Try `method = "logical"` with `logical_rule = "AND"` and `"OR"`
- Try `method = "weighted"` with `weights = c(risk_binary = 0.6, risk_binary_alt = 0.4)`

**What "correct" looks like:** A combined raster reflecting the chosen
rule, with a sensible frequency table.

---

## 10. Function 9: Scenarios (`define_scenarios()` through `scenario_consensus()`)

**What it does:** Lets you test how sensitive your results are to
different analytical choices, by building and comparing multiple
"what-if" versions of your analysis.

This is the most involved function — take your time here.

```r
# Group your classified surfaces by what they represent
result <- define_scenarios(
  result,
  variable_groups = list(
    threshold_choice = c("risk_binary", "risk_binary_alt")
  ),
  combine_method = "binary_overlap"
)

result <- run_scenarios(result)
result <- scenario_sensitivity(result, outcome_value = 1, comparison = "pairwise")
result <- scenario_agreement(result)
result <- scenario_consensus(result, outcome_value = 1, min_scenarios = 1)

result$results$sensitivity
result$results$agreement
result$results$consensus$summary_table
```

**Try this:**
- Build a scenario setup with a real analytical question in mind (if
  you have 2+ meaningfully different versions of a variable)
- Try `comparison = "baseline"` instead of `"pairwise"` (you will need
  to set a `baseline` in `define_scenarios()` first)

**What "correct" looks like:** Sensitivity, agreement, and consensus
tables all produce sensible numbers (percentages between 0-100,
Kappa/Jaccard between -1 and 1 or 0 and 1 respectively).

---

## 11. Function 13: `admin_summary()`

**What it does:** Summarises any raster result (mean, area, dominant
class, etc.) within polygons — e.g. districts.

```r
my_districts <- st_read("path/to/districts.shp")

result <- admin_summary(
  result, raster_source = "surfaces", raster_name = "risk_surface",
  polygons = my_districts, zone_id = "DISTRICT_NAME_COLUMN",
  stats = c("mean", "min", "max")
)

result$results$admin_summary$surfaces__risk_surface$summary_table
```

**What "correct" looks like:** One row per district, with sensible
mean/min/max values.

---

## 12. Function 14: `map_surface()`

**What it does:** Produces a publication-quality map of any result.

```r
map_surface(
  result, raster_source = "surfaces", raster_name = "risk_surface",
  title = "My Risk Surface", legend_title = "Risk Score"
)
```

**Things to try:**
- Map a classified/categorical result too, with `class_labels`
- Try combining two maps with `map_panel(p1, p2, ncol = 2)`

**What "correct" looks like:** A readable map with a legend, your
study area boundary, and (if enabled) a north arrow and scale bar.

---

## 13. Function 15: Export and Save/Load

**What it does:** Exports all your results to files, and allows
saving/reloading your full analysis between R sessions.

```r
export_results(result, output_dir = "my_test_export")
list.files("my_test_export", recursive = TRUE)

save_cystimap(result, "my_analysis.rds")
# restart R, then:
result2 <- load_cystimap("my_analysis.rds")
print(result2)
```

**What "correct" looks like:** Files appear in `my_test_export/` with
a `manifest.csv` listing everything, and `result2` after reloading
prints correctly with all your surfaces/classifications intact.

---

## 14. How to Report Issues

For every problem you find, please note:

1. **Which function** (e.g. `smooth_surface()`)
2. **What you ran** (paste the exact code)
3. **What you expected** vs **what actually happened**
4. **The full error or warning message** (do not summarise it, copy
   the whole thing)

Please open a GitHub Issue on the repository, one per problem, so we
can track and fix them individually:

`https://github.com/devwebwacky/cystimap/issues`

If you are unsure whether something is a bug or expected behaviour,
report it anyway; it is much easier for us to say "that is expected"
than to miss a real problem.

Thank you for testing!

