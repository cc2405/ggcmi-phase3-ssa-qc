# GGCMI Phase 3 SSA / RiskAssess output quality check

- [Overview](#overview)
- [Requirements](#requirements)
- [Input data layout](#input-data-layout)
  - [Folder structure](#folder-structure)
  - [File names](#file-names)
  - [Calendar file](#calendar-file)
- [Configuration](#configuration)
- [Running the check](#running-the-check)
- [Reading the reports](#reading-the-reports)
- [What is checked](#what-is-checked)
- [Known limitations](#known-limitations)
- [License and attribution](#license-and-attribution)

## Overview

This repository contains R scripts that check the quality of crop-model output submitted to the GGCMI Phase 3 SSA / RiskAssess study. For each model folder the scripts check file names, completeness against the expected set of simulations, and the content of every NetCDF file (variable, units, grid, time axis, spatial coverage, value ranges).

The check is read-only: no input file is renamed, rewritten, interpolated or repaired. Passing the check does not validate model accuracy.

| File | Purpose |
|---|---|
| `GGCMI_phase3_SSA_check.R` | Main script: paths, experiment settings, variables and thresholds. Run this file. |
| `GGCMI_phase3_SSA_check_functions.R` | Check functions and report writer. Must stay in the same directory as the main script. |
| `GGCMI_phase3_SSA_QC_guidance_for_modelers.md` | Guide to the report files and issue codes. |
| `config.example.yaml` | Template for `config.yaml`, the local configuration (paths and models). |
| `LICENSE`, `NOTICE.md` | License text and attribution. |

## Requirements

- R. A test run used R 4.4.1.
- The R packages `ncdf4` and `yaml`:

```r
install.packages(c("ncdf4","yaml"))
```

- A first-season crop calendar NetCDF file (see [Calendar file](#calendar-file)).

## Input data layout

### Folder structure

```text
<output_root>/
└── <model>/                      e.g. one folder per modelling group
    └── <climate_forcing>/        gswp-w5e5, ukesm1-0-ll, mri-esm2-0, mpi-esm1-2-hr, ipsl-cm6a-lr, gfdl-esm4
        └── <N-level>/            nc, n0, n20, n40, n60, n90, n120, n180
            └── <file>.nc
```

The climate forcing and N-level in the file name must match the names of the folders that contain the file.

### File names

File names have 11 underscore-separated fields, all lowercase:

```text
model_forcing_scenario_soc_variable_crop_irrigation_region_timestep_start_end.nc
```

Example:

```text
apsimx-pik_gswp-w5e5_obsclim_soc-ssacld-n20_yield_mai_firr_ssa_annual_1978_2019.nc
```

| Field | Expected value |
|---|---|
| `model` | Lowercase model folder name |
| `forcing` | `gswp-w5e5` or one of the five GCMs listed above |
| `scenario` | `obsclim` for `gswp-w5e5`, `historical` for the GCMs |
| `soc` | `soc-ssacld-<N-level>` |
| `variable` | A configured output variable (see [What is checked](#what-is-checked)) |
| `crop` | `mai` |
| `irrigation` | `firr` and `noirr` for `gswp-w5e5`; `noirr` only for the GCMs |
| `region` | `ssa` |
| `timestep` | `annual`; `monthly` for `soilmoist1m` |
| `start`, `end` | `1978`, `2019` |

Irrigation requirements (`pirnreq`) are expected for `firr` only.

### Calendar file

The check needs a NetCDF file with the first-season optimal sowing calendar on the project's 0.5 degree grid. Model output is matched to it by coordinate value. The file must contain:

- `optimal_sowing_doy`: sowing day of year; its valid cells define the target mask.
- `optimal_sowing_mat`: optional; cells with a value above 365 are flagged as possible cross-year seasons in diagnostics.

Only the first season is used. The calendar file is not included in this repository; obtain it from the project coordinators.

## Configuration

Copy `config.example.yaml` to `config.yaml` in the same directory as the scripts and edit it. `config.yaml` is not tracked by git. Use forward slashes in paths, also on Windows. Relative paths are resolved against the directory of `GGCMI_phase3_SSA_check.R`.

| Key | Meaning |
|---|---|
| `output_root` | Folder that contains one sub-folder per model |
| `models` | List of model folder names to check. List them explicitly; with an empty list every top-level folder in `output_root` is treated as a model, except folders whose name starts with `.` or `_`. |
| `calendar_file` | Path to the calendar NetCDF file |
| `report_root` | Folder for the reports. Use a folder outside `output_root`; the default in the template is `output`. |

The script stops with an error message if `config.yaml` is missing or a key is invalid.

Further run options are set in `GGCMI_phase3_SSA_check.R`:

- `cfg$gcm_check_scope`: `submitted_only` (default) checks completeness for the GCMs found in the model folder; `all` checks all five GCMs; `none` skips GCM completeness. All discovered files receive the filename and content checks in every mode.
- `cfg$core_missing_severity`: `ERROR` (default) or `WARNING` for missing core-variable files.

## Running the check

Keep both R scripts in the same directory, then run the main script from a terminal:

```sh
Rscript GGCMI_phase3_SSA_check.R
```

or open `GGCMI_phase3_SSA_check.R` in RStudio and click Source.

On Windows, `Rscript` is often not on the PATH. In that case call it with its full path, for example `"C:\Program Files\R\R-4.4.1\bin\Rscript.exe" GGCMI_phase3_SSA_check.R`, or add the `bin` folder of the R installation to the PATH and open a new terminal.

The script prints one progress line per file and, for each model, the path of the summary report and the number of errors, warnings and information messages.

Every run appends to `GGCMI_phase3_SSA_check.log`, which is created next to the main script and is not tracked by git. The log records the start and end time, the configuration steps, the result per model, and any warning or error. A failed run ends with a non-zero exit status.

## Reading the reports

For each model the reports are written to:

```text
<report_root>/<model>/<YYYY-MM-DD>/run_<HHMMSS>_summary.txt
<report_root>/<model>/<YYYY-MM-DD>/run_<HHMMSS>_issues.csv
<report_root>/<model>/<YYYY-MM-DD>/details/run_<HHMMSS>_<supporting-file>
```

Each run has its own `run_<HHMMSS>` prefix, so earlier reports from the same day are kept. Read `summary.txt` first, then filter `issues.csv` by severity and code. The supporting files in `details/` hold the missing-file list, per-period coverage, file statistics, coordinate examples, the configuration and the R session information.

[GGCMI_phase3_SSA_QC_guidance_for_modelers.md](GGCMI_phase3_SSA_QC_guidance_for_modelers.md) describes every report file, severity level and issue code.

## What is checked

| Stage | What is checked |
|---|---|
| `filename` | Extension, 11-field structure, model, forcing, scenario, N-level, variable, crop, irrigation, region, timestep, years, and agreement with the folder names |
| `completeness` | Expected experiment-variable combinations against the files found: present, missing, duplicate |
| `data` | One identifiable data variable, units, missing-value marker, dimensions, coordinates and 0.5 degree grid, time axis against the file name and the project years, coverage against the calendar mask, value ranges, failure markers (-1), infinite values |

Experiment settings:

- Years 1978 to 2019: 42 annual steps, or 504 monthly steps for `soilmoist1m`.
- Missing-value marker `1e20`; any other marker gives a warning.
- Core variables (missing files are errors by default): `yield`, `biom`, `cnyield`, `plantday`, `plantyear`, `anthday`, `matyday`, `harvyear`, `pirnreq`, `aet`, `soilmoist1m`, `tnrup`, `n2oemis`, `nleach`.
- Optional variables (missing files are information only; provided files receive the same content checks): `soilmoistmat`, `transp`, `soilevap`, `runoff`, `rootm`, `tnrin`, `tnrloss`, `n2emis`, `tcemis`, `ch4emis`, `maturitystatus`, `maturityindex`.

Value ranges flag potential anomalies; they are not universal agronomic limits.

## Known limitations

- On Windows, a full file path of 260 characters or more can trigger spurious `DIRECTORY_STRUCTURE` warnings. Use a shorter `output_root`.
- The expected experiment set is defined in the main script. Output that deviates from it, for example a different GCM scenario name or `firr` files for a GCM, is reported as errors, and files with a deviating scenario are also counted as missing in the completeness check.
- Missing target cells in any time step are reported as a warning (`MASK_COVERAGE_MISSING`). For the final year, a diagnostic message notes how many of them coincide with cells whose reference maturity is above 365 days; no exemption is applied.
- Coordinate checks cannot detect every earlier data flip when the coordinate labels are unchanged. Compare with source output, known sites or sowing dates.

## License and attribution

Distributed under the GNU Affero General Public License, version 3 only. See [LICENSE](LICENSE).

The scripts are adapted from [AgMIP-GGCMI/phase3_quality_check](https://github.com/AgMIP-GGCMI/phase3_quality_check). See [NOTICE.md](NOTICE.md) for the reference commit and the list of modifications.
