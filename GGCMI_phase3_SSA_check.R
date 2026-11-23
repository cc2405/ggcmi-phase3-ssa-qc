# GGCMI Phase 3 SSA / RiskAssess: MAIN SCRIPT
# Modified 2026-11-05; configuration moved to config.yaml and run log added 2026-11-22.
# SPDX-License-Identifier: AGPL-3.0-only
# Based on https://github.com/AgMIP-GGCMI/phase3_quality_check
# Full license: see the LICENSE file.
#
# Usage:
# 1. Place both R scripts in the same directory.
# 2. Install the dependencies: install.packages(c("ncdf4","yaml"))
# 3. Copy config.example.yaml to config.yaml (same directory); set paths and models there.
# 4. Run this script. In RStudio, click Source; or run Rscript GGCMI_phase3_SSA_check.R
# Each run appends to GGCMI_phase3_SSA_check.log in the script directory:
# start and end time, steps, warnings and errors.
#
# Input directory structure:
# output_root / model / climate-forcing / N-level / file.nc
# Report directory structure (set report_root in config.yaml):
# report_root / model / YYYY-MM-DD / run_HHMMSS_summary.txt
# report_root / model / YYYY-MM-DD / run_HHMMSS_issues.csv
# report_root / model / YYYY-MM-DD / details / run_HHMMSS_supporting-file
# Repeated runs on the same day use unique prefixes to preserve earlier reports.
# Read the summary first; it groups findings by type and counts affected files.
#
# Filenames contain 11 underscore-separated fields:
# model_forcing_scenario_soc_variable_crop_irrigation_region_timestep_start_end.nc
# Example: apsimx-pik_gswp-w5e5_obsclim_soc-ssacld-n20_yield_mai_firr_ssa_annual_1978_2019.nc

ssa_script_directory <- function() {
  # source() / RStudio Source: use the path of this script, not current working dir.
  paths <- vapply(sys.frames(),function(f) {
    p <- f$ofile
    if (is.null(p)) "" else as.character(p)[1]
  },character(1))
  paths <- paths[nzchar(paths)]
  if (length(paths)) return(dirname(normalizePath(tail(paths,1),winslash="/")))
  arg <- grep("^--file=",commandArgs(trailingOnly=FALSE),value=TRUE)
  if (length(arg)) return(dirname(normalizePath(sub("^--file=","",arg[1]),winslash="/")))
  getwd()
}
script_dir <- ssa_script_directory()

# ---------------------- Run log ----------------------
# Appends to GGCMI_phase3_SSA_check.log in the script directory; earlier runs are kept.
log_file <- file.path(script_dir,"GGCMI_phase3_SSA_check.log")
run_start_time <- Sys.time()
seconds_since_start <- function() as.numeric(difftime(Sys.time(),run_start_time,units="secs"))
write_log <- function(level,message_text) {
  cat(sprintf("%s [%s] %s\n",format(Sys.time(),"%Y-%m-%d %H:%M:%S"),level,message_text),
      file=log_file,append=TRUE)
}
stop_with_log <- function(message_text) {
  message_text <- trimws(message_text)
  write_log("ERROR",message_text)
  write_log("END",sprintf("Run failed after %.1f seconds.",seconds_since_start()))
  stop(message_text,call.=FALSE)
}
write_log("START",paste0("script directory: ",script_dir,"; ",R.version.string))

write_log("STEP","Loading check functions from GGCMI_phase3_SSA_check_functions.R")
tryCatch(
  source(file.path(script_dir,"GGCMI_phase3_SSA_check_functions.R"),encoding="UTF-8"),
  error=function(e) stop_with_log(paste("Cannot load check functions:",conditionMessage(e))))
cfg <- ssa_default_config()

# ---------------------- Read local configuration ----------------------
# Paths and models come from config.yaml (copy config.example.yaml and edit it).
# config.yaml is listed in .gitignore. Relative paths are resolved against the script directory.
config_file <- file.path(script_dir,"config.yaml")
if (!file.exists(config_file))
  stop_with_log(paste("Configuration file not found:",config_file,
                      "- copy config.example.yaml to config.yaml and edit it."))
if (!requireNamespace("yaml",quietly=TRUE))
  stop_with_log("Install the yaml package first: install.packages('yaml')")
write_log("STEP",paste("Reading configuration from",config_file))
local_config <- tryCatch(yaml::read_yaml(config_file),
  error=function(e) stop_with_log(paste("Cannot parse config.yaml:",conditionMessage(e))))

for (setting in c("output_root","calendar_file","report_root")) {
  value <- local_config[[setting]]
  if (!is.character(value) || length(value)!=1L || !nzchar(value))
    stop_with_log(paste0("config.yaml: '",setting,"' is missing or is not a single non-empty text value."))
}

resolve_against_script_dir <- function(path) {
  is_absolute <- grepl("^(/|~|[A-Za-z]:)",path) || startsWith(path,"\\\\")
  if (is_absolute) path else file.path(script_dir,path)
}
cfg$output_root <- resolve_against_script_dir(local_config$output_root)
cfg$calendar_file <- resolve_against_script_dir(local_config$calendar_file)
cfg$report_root <- resolve_against_script_dir(local_config$report_root)

# Enter model folder names in config.yaml; an empty list checks every top-level folder of output_root.
cfg$models <- as.character(unlist(local_config$models))
if (!length(cfg$models)) {
  cfg$models <- NULL
  write_log("WARNING","No models listed in config.yaml: every top-level folder of output_root is treated as a model.")
}
write_log("STEP",paste("output_root:",cfg$output_root))
write_log("STEP",paste("models:",if (is.null(cfg$models)) "(all folders)" else paste(cfg$models,collapse=", ")))
write_log("STEP",paste("calendar_file:",cfg$calendar_file))
write_log("STEP",paste("report_root:",cfg$report_root))

# Automatically derive expected prefixes from model names.
cfg$expected_prefixes <- setNames(tolower(cfg$models), cfg$models)

# ---------------------- Project experiment settings ----------------------
cfg$n_levels <- c("nc","n0","n20","n40","n60","n90","n120","n180")
cfg$core_forcing <- "gswp-w5e5"  # obsclim; firr + noirr
cfg$core_climate_scenario <- "obsclim"
cfg$gcm_climate_scenario <- "historical"
cfg$optional_gcms <- c("ukesm1-0-ll","mri-esm2-0","mpi-esm1-2-hr",
                      "ipsl-cm6a-lr","gfdl-esm4") # historical; noirr only
# This controls which optional GCM submissions receive completeness checks.
# It is NOT a climate scenario: scenarios remain obsclim and historical above.
# submitted_only: check all 8 N levels for optional GCMs found in folders or filenames.
# all: check all 5 optional GCMs; none: omit optional GCMs from completeness checks.
# All discovered NetCDF files receive filename and content checks in every mode.
cfg$gcm_check_scope <- "submitted_only"
cfg$calendar_code <- "ssacld"
cfg$crop <- "mai"
cfg$region <- "ssa"
cfg$start_year <- 1978L
cfg$end_year <- 2019L

# ---------------------- Output variables and thresholds ----------------------
# Core variables: missing applicable files generate ERROR (or WARNING, if selected).
cfg$core_vars <- c(
  "yield",       # Yield
  "biom",        # Total above-ground biomass
  "cnyield",     # C:N ratio of yield
  "plantday",    # Actual planting day
  "plantyear",   # Actual planting year
  "anthday",     # Anthesis date: days from planting
  "matyday",     # Maturity date: days from planting
  "harvyear",    # Actual harvest year
  "pirnreq",     # Potential irrigation requirements: firr only
  "aet",         # Actual evapotranspiration
  "soilmoist1m", # Soil moisture of the upper 1 m: monthly
  "tnrup",       # Nitrogen uptake
  "n2oemis",     # N2O emissions
  "nleach")      # Nitrogen leaching
cfg$core_missing_severity <- "ERROR" # Change to "WARNING" if preferred.

# All remaining configured variables are optional. Their absence generates INFO only.
# If provided, optional variables receive the same content checks as core variables.
cfg$optional_vars <- setdiff(cfg$allowed_vars,cfg$core_vars)

# Annual output for all configured variables except monthly soilmoist1m.
cfg$variable_timesteps <- setNames(rep("annual",length(cfg$allowed_vars)),cfg$allowed_vars)
cfg$variable_timesteps[["soilmoist1m"]] <- "monthly"
# No pirnreq file is expected for rain-fed simulations, including optional GCM runs.
cfg$firr_only_vars <- "pirnreq"
#
# ranges and units retain the upstream rules and are indexed by variable name, not position.
# Thresholds flag potential anomalies; they are not universal agronomic plausibility limits.
# Override examples: use protocol-defined values, rather than relaxing limits to fit one file.
# cfg$ranges[["yield"]] <- c(0, 100)
# cfg$units[["soilmoist1m"]] <- "kg m-3"

# ---------------------- QC and report settings ----------------------
# FIRST SEASON ONLY: derive the mask from optimal_sowing_doy.
# Cross-year diagnostics use optimal_sowing_mat only. No season2 variables are used.
# Recognize each file's declared missing values; markers other than 1e20 generate a WARNING.
# Count the -1 failure marker separately and exclude it from valid-data and range statistics.
cfg$fail_value <- -1
cfg$expected_fill <- 1e20
cfg$coordinate_tolerance <- 1e-6
cfg$resolution <- 0.5
cfg$max_examples_per_file <- 100L
cfg$save_reports <- TRUE
cfg$report_date <- format(Sys.Date(),"%Y-%m-%d")

# Compression level is not evaluated or reported as an issue.
# Check every file and time step, reading one step at a time to limit memory use.
# Annual variables require 42 steps (1978-2019); soilmoist1m requires 504 months.
# Supported time encodings include:
# growing seasons since ..., months since ..., and date-based units such as days since ...
# Monthly checks verify year-month continuity, duplicates and missing months.
# yearly_coverage.csv retains its name but includes timestep/month/period columns.
# Handle reversed latitude or reordered dimensions by matching names and coordinates.
# No interpolation or input rewriting is performed. Coordinate checks alone cannot detect
# every prior data flip with unchanged labels; compare source outputs, known sites or sowing dates.

write_log("STEP","Running quality check (per-file progress is printed to the console)")
qc_results <- withCallingHandlers(
  tryCatch(run_ssa_qc(cfg),
           error=function(e) stop_with_log(paste("Quality check failed:",conditionMessage(e)))),
  warning=function(w) write_log("WARNING",conditionMessage(w)))

for (model in names(qc_results)) {
  model_issues <- qc_results[[model]]$issues
  report_folder <- qc_results[[model]]$reports
  write_log("RESULT",sprintf("%s: %d errors, %d warnings, %d information messages; reports: %s",
    model,sum(model_issues$severity=="ERROR"),sum(model_issues$severity=="WARNING"),
    sum(model_issues$severity=="INFO"),if (is.null(report_folder)) "not saved" else report_folder))
}
write_log("END",sprintf("Run finished in %.1f seconds.",seconds_since_start()))

