# GGCMI SSA QC: Report Reading Guide

Start with the two main reports. Supporting files are in `details/` under the same model/date folder. Filenames below omit the run prefix; always use files with the same prefix.

## Main reports

| File | Contents |
|---|---|
| `summary.txt` | File completeness and one row per severity/stage/code. `affected_files` counts unique submitted files (`-` means model-level); `messages` counts issue rows. Do not sum file counts across categories. |
| `issues.csv` | All findings, identified by input `file`, check `stage`, `severity`, category `code` and explanatory `message`. |

## Supporting files in `details/`

| File | Contents |
|---|---|
| `filename_issues.txt` | Filename structure, model name, experiment fields, time span and agreement with directory names. |
| `simulations_missing.txt` | Missing core files, optional files not provided, and duplicate combinations. |
| `expected_files.csv` | Expected experiment-variable combinations, their `variable_class`, `status` and matching `found_files`. |
| `file_inventory.csv` | Discovered files and filename fields; `parsed = FALSE` means parsing failed. |
| `data_issues.txt` | Content findings: variable names, units, dimensions, coordinates, time, coverage and numerical values. |
| `yearly_coverage.csv` | Per-file, per-period coverage, including monthly records. `timestep`, `year`, `month` and `period` identify the interval. |
| `file_statistics.csv` | File-wide counts of valid, missing, failed, infinite and out-of-range values, plus minimum and maximum valid values. |
| `coordinate_examples.csv` | Limited location samples; `type` is `target_missing_or_failed`, `outside_mask` or `out_of_range`. |
| `configuration.R` | Paths, variable requirements, thresholds and other settings used for the run. |
| `session_info.txt` | R version and package environment. |

## Labels and categories

**Severity:** `ERROR` = requirement/check failed; `WARNING` = investigate; `INFO` = context. Partial missingness remains WARNING regardless of percentage.

**Stage:** `filename` = naming and directory checks; `completeness` = expected-file availability; `data` = NetCDF contents.

**Variable class:** `core` = required where applicable; `optional` = absence is INFO; `unconfigured` = not listed.

**File status:** `present` = matched; `present_with_naming_issues` = matched with naming findings; `missing` = unmatched; `duplicate` = multiple matches. Presence does not establish data quality.

## Main issue-code groups

| Code or group | Meaning |
|---|---|
| `FILE_*`, `FILENAME_*`, `MODEL_*`, `*_FOLDER`, `DIRECTORY_STRUCTURE` | Naming, model or directory inconsistencies. |
| `CLIMATE_*`, `SOC_SCENARIO`, `VARIABLE`, `CROP`, `IRRIGATION`, `REGION`, `TIMESTEP` | Experiment-field mismatches. |
| `MISSING_CORE_FILES`, `OPTIONAL_FILES_NOT_PROVIDED`, `DUPLICATE_OUTPUTS` | Required files absent, optional files absent, or repeated combinations. |
| `GCM_CHECK_SCOPE`, `UNREQUESTED_OUTPUTS` | Scope of completeness checks, or files outside those combinations. |
| `NO_NETCDF_FILES`, `READ_OR_CHECK_FAILURE`, `CONTENT_SKIPPED` | No inputs found, reading/checking failed, or content could not be checked. |
| `DATA_VARIABLE`, `DATA_DIMENSIONS`, `VARIABLE_NAME_SUFFIX`, `VARIABLE_SUFFIX_MISMATCH` | Data-variable identification, dimensions or naming suffixes. |
| `UNITS`, `DRY_MATTER_UNSPECIFIED`, `FILL_VALUE` | Unit mismatch, unstated dry-matter basis, or a nonstandard missing-value marker. |
| `COORDINATE_*`, `GRID_*`, `LATITUDE_DESCENDING`, `LONGITUDE_CONVENTION` | Coordinate validity, units, spacing, extent or recognized conventions; differences need not mean misalignment. |
| `TARGET_COORDINATES_MISSING` | Target cells have no matching output coordinates. |
| `TIME_*` | Time encoding, decoding, order, count or agreement with expected years/months. |
| `MASK_COVERAGE_MISSING`, `MASK_COVERAGE_EXTRA` | Missing/failed target cells, or valid values outside the reference mask. |
| `NO_VALID_DATA`, `EMPTY_TARGET_PERIODS` | No valid values in the whole variable, or no valid target-cell data in a time step. |
| `DATA_RANGE`, `FAILURE_VALUES`, `INFINITE_VALUES` | Values outside configured limits, failure markers, or infinite values. |
| `FINAL_YEAR_CROSS_YEAR_DIAGNOSTIC` | Missing final-year cells overlap potential cross-year calendar cells; cause remains unconfirmed. |

## Coverage columns

| Column | Meaning |
|---|---|
| `expected_cells` | Number of target cells in the first-season mask. |
| `valid_inside` | Target cells with valid data. |
| `missing_inside` | Target cells without valid data, including failed cells and absent coordinates. |
| `failed_inside` | Failure-marker cells, included in `missing_inside`. |
| `unrepresented_target_cells` | Cells without matching coordinates, included in `missing_inside`. |
| `valid_outside` | Valid observations outside the target mask. |
| `out_of_range` | Values outside configured limits across the output grid. |
| `missing_crossyear_reference` | Missing cells with potential cross-year reference-calendar flags. |
