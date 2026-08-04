#' Define variable groups and generate scenarios
#'
#' Each variable group represents one analytical factor with two or
#' more already-classified alternatives (from `classify_surface()`).
#' This function generates every valid combination across groups (or
#' a user-restricted subset), and stores each combination as a named
#' scenario with a reproducible definition.
#'
#' @param x a cystimap object
#' @param variable_groups a named list of character vectors. Each name
#'   is a factor (e.g. "sanitation"), each vector holds the names of
#'   classified surfaces in `x$classified` representing alternatives
#'   for that factor, e.g.
#'   `list(sanitation = c("unimproved_binary","opendefecation_binary"),
#'         poverty = c("wealth_q1_binary","wealth_q12_binary"))`
#' @param combine_method combination method passed to
#'   `combine_surfaces()` for every scenario (e.g. "binary_overlap",
#'   "additive", "weighted", "logical")
#' @param combine_args a named list of additional arguments passed to
#'   `combine_surfaces()` (e.g. `list(logical_rule = "AND")` or
#'   `list(weights = c(...))`). Applied identically to every scenario
#'   unless the weights need to reference generated names — see notes.
#' @param restrict_to optional list of specific combinations to build
#'   instead of the full cross-product. Each element is a named list
#'   matching one choice per group, e.g.
#'   `list(list(sanitation = "unimproved_binary", poverty = "wealth_q1_binary"))`
#' @param baseline optional named list identifying which combination is
#'   the baseline scenario (same format as one element of `restrict_to`)
#'
#' @return the cystimap object `x`, with scenario definitions stored in
#'   `x$scenarios$definitions` (not yet executed — see `run_scenarios()`)
#' @export
define_scenarios <- function(x, variable_groups, combine_method = "binary_overlap",
                             combine_args = list(), restrict_to = NULL, baseline = NULL) {

  validate_cystimap(x)

  if (length(variable_groups) < 1) {
    stop("`variable_groups` must contain at least one factor group.", call. = FALSE)
  }

  #validate all referenced classified surfaces exist
  all_refs <- unlist(variable_groups, use.names = FALSE)
  missing_refs <- setdiff(all_refs, names(x$classified))
  if (length(missing_refs) > 0) {
    stop("Classified surface(s) referenced in `variable_groups` not found: ",
         paste(missing_refs, collapse = ", "), call. = FALSE)
  }

  #generate combinations
  if (!is.null(restrict_to)) {
    combos <- restrict_to
  } else {
    grid <- expand.grid(variable_groups, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
    combos <- lapply(seq_len(nrow(grid)), function(i) as.list(grid[i, , drop = FALSE]))
  }

  if (length(combos) == 0) {
    stop("No scenario combinations were generated. Check `variable_groups` or `restrict_to`.", call. = FALSE)
  }

  #build scenario definitions
  scenario_defs <- lapply(seq_along(combos), function(i) {
    combo <- combos[[i]]
    is_baseline <- !is.null(baseline) && identical(combo[names(baseline)], baseline)
    list(
      scenario_id   = paste0("scenario_", sprintf("%03d", i)),
      variables     = combo,
      combine_method = combine_method,
      combine_args   = combine_args,
      is_baseline   = is_baseline
    )
  })
  names(scenario_defs) <- vapply(scenario_defs, function(s) s$scenario_id, character(1))

  if (!is.null(baseline) && !any(vapply(scenario_defs, function(s) s$is_baseline, logical(1)))) {
    warning("`baseline` was supplied but did not match any generated scenario. ",
            "No baseline has been flagged.", call. = FALSE)
  }

  x$scenarios$groups      <- variable_groups
  x$scenarios$definitions <- scenario_defs
  x$scenarios$outputs     <- list()   # populated later by run_scenarios()

  x <- log_step(x, "define_scenarios", details = list(
    n_groups    = length(variable_groups),
    group_sizes = vapply(variable_groups, length, integer(1)),
    n_scenarios = length(scenario_defs),
    combine_method = combine_method,
    has_baseline = !is.null(baseline)
  ))

  x
}


#' Execute all defined scenarios
#'
#' Runs `combine_surfaces()` once per scenario definition created by
#' `define_scenarios()`, using the classified surfaces named in each
#' scenario's variable selection. Requires at least 2 variable groups
#' (so each scenario has 2+ surfaces to combine) — if only one group
#' was defined, each scenario's single surface is stored as-is with a
#' warning, since there is nothing to combine.
#'
#' @param x a cystimap object with scenario definitions already created
#'   via `define_scenarios()`
#'
#' @return the cystimap object `x`, with each scenario's output stored
#'   in `x$scenarios$outputs[[scenario_id]]`
#' @export
run_scenarios <- function(x) {

  validate_cystimap(x)

  if (is.null(x$scenarios$definitions) || length(x$scenarios$definitions) == 0) {
    stop("No scenario definitions found. Run define_scenarios() first.", call. = FALSE)
  }

  defs <- x$scenarios$definitions
  outputs <- list()
  n_groups <- length(x$scenarios$groups)

  for (sid in names(defs)) {
    def <- defs[[sid]]
    surface_names <- unlist(def$variables, use.names = FALSE)

    if (n_groups < 2 || length(surface_names) < 2) {
      # nothing to combine - store the single classified surface as the scenario output
      warning("Scenario '", sid, "' has fewer than 2 variable groups — ",
              "storing the single classified surface without combination.", call. = FALSE)
      outputs[[sid]] <- list(
        combined_raster = x$classified[[surface_names[1]]]$rasters[["combined"]],
        input_names     = surface_names,
        method          = "none (single group)",
        frequencies     = x$classified[[surface_names[1]]]$frequencies,
        warnings        = "no combination performed - only one variable group supplied"
      )
      next
    }

    combine_call_args <- c(
      list(x = x, classified_names = surface_names, combined_name = sid,
           method = def$combine_method),
      def$combine_args
    )

    x_temp <- do.call(combine_surfaces, combine_call_args)
    outputs[[sid]] <- x_temp$results$combined[[sid]]
  }

  x$scenarios$outputs <- outputs

  x <- log_step(x, "run_scenarios", details = list(
    n_scenarios = length(outputs),
    scenario_ids = names(outputs)
  ))

  x
}

#' Compare scenarios and calculate sensitivity statistics
#'
#' Compares every scenario's combined raster against a baseline (or
#' against every other scenario pairwise if no baseline is set),
#' calculating area gained, lost, unchanged, and percentage change for
#' a specified "outcome" class.
#'
#' @param x a cystimap object with scenarios already executed via
#'   `run_scenarios()`
#' @param outcome_value the raster value representing the outcome of
#'   interest (e.g. 1 for "present", or the maximum overlap value).
#'   Cells equal to this value are treated as "positive" for comparison.
#' @param comparison one of "baseline" (compare every scenario to the
#'   scenario flagged `is_baseline` in `define_scenarios()`) or
#'   "pairwise" (compare every scenario to every other scenario)
#'
#' @return the cystimap object `x`, with a sensitivity comparison table
#'   stored in `x$results$sensitivity`
#' @export
scenario_sensitivity <- function(x, outcome_value = 1,
                                 comparison = c("baseline", "pairwise")) {

  validate_cystimap(x)
  comparison <- match.arg(comparison)

  if (is.null(x$scenarios$outputs) || length(x$scenarios$outputs) == 0) {
    stop("No scenario outputs found. Run run_scenarios() first.", call. = FALSE)
  }

  outputs <- x$scenarios$outputs
  ids <- names(outputs)

  compare_pair <- function(id_a, id_b) {
    ra <- outputs[[id_a]]$combined_raster
    rb <- outputs[[id_b]]$combined_raster

    if (!terra::compareGeom(ra, rb, stopOnError = FALSE)) {
      warning("Scenarios '", id_a, "' and '", id_b, "' are not spatially aligned — skipped.", call. = FALSE)
      return(NULL)
    }

    bin_a <- ra == outcome_value
    bin_b <- rb == outcome_value

    cell_area <- prod(terra::res(ra))
    gained    <- sum(terra::values(bin_b & !bin_a), na.rm = TRUE) * cell_area
    lost      <- sum(terra::values(bin_a & !bin_b), na.rm = TRUE) * cell_area
    unchanged <- sum(terra::values(bin_a & bin_b), na.rm = TRUE) * cell_area
    area_a    <- sum(terra::values(bin_a), na.rm = TRUE) * cell_area
    area_b    <- sum(terra::values(bin_b), na.rm = TRUE) * cell_area
    pct_change <- if (area_a > 0) 100 * (area_b - area_a) / area_a else NA

    data.frame(
      scenario_a = id_a, scenario_b = id_b,
      area_a = area_a, area_b = area_b,
      area_gained = gained, area_lost = lost, area_unchanged = unchanged,
      absolute_change = abs(area_b - area_a),
      percentage_change = pct_change
    )
  }

  if (comparison == "baseline") {
    baseline_id <- names(Filter(function(d) isTRUE(d$is_baseline), x$scenarios$definitions))
    if (length(baseline_id) == 0) {
      stop("No baseline scenario flagged. Set `baseline` in define_scenarios(), ",
           "or use comparison = 'pairwise'.", call. = FALSE)
    }
    baseline_id <- baseline_id[1]
    others <- setdiff(ids, baseline_id)
    rows <- lapply(others, function(oid) compare_pair(baseline_id, oid))

  } else {
    pairs <- utils::combn(ids, 2, simplify = FALSE)
    rows <- lapply(pairs, function(p) compare_pair(p[1], p[2]))
  }

  sens_table <- do.call(rbind, Filter(Negate(is.null), rows))

  x$results$sensitivity <- sens_table

  x <- log_step(x, "scenario_sensitivity", details = list(
    comparison = comparison,
    outcome_value = outcome_value,
    n_comparisons = nrow(sens_table)
  ))

  x
}

#' Calculate agreement statistics between scenario outputs
#'
#' Compares every pair of scenario outputs using Cohen's Kappa,
#' Jaccard Similarity, and Exact Agreement. Designed for binary or
#' multiclass classified outputs (continuous rasters should be
#' classified first).
#'
#' @param x a cystimap object with scenarios already executed via
#'   `run_scenarios()`
#'
#' @return the cystimap object `x`, with a pairwise agreement table
#'   stored in `x$results$agreement`
#' @export
scenario_agreement <- function(x) {

  validate_cystimap(x)

  if (is.null(x$scenarios$outputs) || length(x$scenarios$outputs) == 0) {
    stop("No scenario outputs found. Run run_scenarios() first.", call. = FALSE)
  }

  outputs <- x$scenarios$outputs
  ids <- names(outputs)

  if (length(ids) < 2) {
    stop("At least 2 scenarios are required to calculate agreement.", call. = FALSE)
  }

  agree_pair <- function(id_a, id_b) {
    ra <- outputs[[id_a]]$combined_raster
    rb <- outputs[[id_b]]$combined_raster

    if (!terra::compareGeom(ra, rb, stopOnError = FALSE)) {
      warning("Scenarios '", id_a, "' and '", id_b, "' are not spatially aligned — skipped.", call. = FALSE)
      return(NULL)
    }

    va <- terra::values(ra)[, 1]
    vb <- terra::values(rb)[, 1]
    valid <- !is.na(va) & !is.na(vb)
    va <- va[valid]; vb <- vb[valid]

    if (length(va) == 0) {
      warning("No overlapping valid cells between '", id_a, "' and '", id_b, "'.", call. = FALSE)
      return(NULL)
    }

    # exact agreement
    exact_agreement <- mean(va == vb)

    # Cohen's Kappa (computed directly, no external dependency)
    kappa <- calculate_kappa(va, vb)

    # Jaccard similarity (based on positive/outcome class overlap)
    # uses the maximum shared value as "positive" by default
    positive_class <- max(va, na.rm = TRUE)
    a_pos <- va == positive_class
    b_pos <- vb == positive_class
    intersection <- sum(a_pos & b_pos)
    union <- sum(a_pos | b_pos)
    jaccard <- if (union > 0) intersection / union else NA

    data.frame(
      scenario_a = id_a, scenario_b = id_b,
      exact_agreement = exact_agreement,
      cohens_kappa = kappa,
      jaccard = jaccard,
      n_cells = length(va)
    )
  }

  pairs <- utils::combn(ids, 2, simplify = FALSE)
  rows <- lapply(pairs, function(p) agree_pair(p[1], p[2]))
  agree_table <- do.call(rbind, Filter(Negate(is.null), rows))

  x$results$agreement <- agree_table

  x <- log_step(x, "scenario_agreement", details = list(
    n_comparisons = nrow(agree_table)
  ))

  x
}

#' Calculate Cohen's Kappa between two classified vectors (internal)
#' @keywords internal
calculate_kappa <- function(a, b) {
  levels_all <- sort(unique(c(a, b)))
  tab <- table(factor(a, levels = levels_all), factor(b, levels = levels_all))
  n <- sum(tab)
  po <- sum(diag(tab)) / n
  pe <- sum((rowSums(tab) / n) * (colSums(tab) / n))
  if (pe == 1) return(NA)  # avoid divide-by-zero when perfect agreement expected by chance
  (po - pe) / (1 - pe)
}

#' Calculate consensus across all scenario outputs
#'
#' Identifies locations that are consistently classified as the
#' outcome of interest across multiple scenarios, using counts,
#' proportions, or weighted consensus.
#'
#' @param x a cystimap object with scenarios already executed via
#'   `run_scenarios()`
#' @param outcome_value the raster value representing the outcome of
#'   interest (same meaning as in `scenario_sensitivity()`)
#' @param min_scenarios minimum number of scenarios in which a cell
#'   must show the outcome to be flagged as "consensus" (used for the
#'   binary consensus flag layer; the proportion/count layers are
#'   always returned in full regardless of this threshold)
#' @param weights optional named numeric vector of scenario weights
#'   (names must match scenario IDs). If supplied, a weighted
#'   consensus proportion is also calculated.
#'
#' @return the cystimap object `x`, with consensus rasters and a
#'   summary table stored in `x$results$consensus`
#' @export
scenario_consensus <- function(x, outcome_value = 1, min_scenarios = NULL, weights = NULL) {

  validate_cystimap(x)

  if (is.null(x$scenarios$outputs) || length(x$scenarios$outputs) == 0) {
    stop("No scenario outputs found. Run run_scenarios() first.", call. = FALSE)
  }

  outputs <- x$scenarios$outputs
  ids <- names(outputs)
  n_scenarios <- length(ids)

  if (n_scenarios < 2) {
    stop("At least 2 scenarios are required for consensus analysis.", call. = FALSE)
  }

  ref <- outputs[[1]]$combined_raster
  aligned <- vapply(outputs[-1], function(o) terra::compareGeom(ref, o$combined_raster, stopOnError = FALSE), logical(1))
  if (!all(aligned)) {
    stop("Not all scenario rasters are spatially aligned — cannot calculate consensus.", call. = FALSE)
  }

  # binary "outcome present" stack across scenarios
  bin_stack <- terra::rast(lapply(outputs, function(o) o$combined_raster == outcome_value))
  names(bin_stack) <- ids

  # consensus count: how many scenarios agree per cell
  consensus_count <- sum(bin_stack, na.rm = FALSE)

  # consensus proportion
  consensus_proportion <- consensus_count / n_scenarios

  # weighted consensus (optional)
  weighted_consensus <- NULL
  if (!is.null(weights)) {
    if (!all(ids %in% names(weights))) {
      stop("`weights` must be a named vector covering all scenario IDs: ",
           paste(ids, collapse = ", "), call. = FALSE)
    }
    w <- weights[ids]
    if (abs(sum(w) - 1) > 1e-6) {
      warning("Consensus weights sum to ", round(sum(w), 3), ", not 1. Proceeding anyway.", call. = FALSE)
    }
    weighted_layers <- lapply(ids, function(id) bin_stack[[id]] * w[[id]])
    weighted_consensus <- sum(terra::rast(weighted_layers), na.rm = FALSE)
  }

  # binary consensus flag using min_scenarios threshold (default: majority)
  if (is.null(min_scenarios)) min_scenarios <- ceiling(n_scenarios / 2)
  consensus_flag <- consensus_count >= min_scenarios

  # summary table
  cell_area <- prod(terra::res(ref))
  freq_flag <- terra::freq(consensus_flag)
  freq_flag$area <- freq_flag$count * cell_area
  freq_flag$percentage <- 100 * freq_flag$count / sum(freq_flag$count)

  result <- list(
    consensus_count      = consensus_count,
    consensus_proportion = consensus_proportion,
    weighted_consensus   = weighted_consensus,
    consensus_flag       = consensus_flag,
    min_scenarios_used   = min_scenarios,
    outcome_value         = outcome_value,
    n_scenarios           = n_scenarios,
    summary_table          = freq_flag
  )

  x$results$consensus <- result

  x <- log_step(x, "scenario_consensus", details = list(
    n_scenarios = n_scenarios,
    outcome_value = outcome_value,
    min_scenarios_used = min_scenarios,
    weighted = !is.null(weights)
  ))

  x
}
