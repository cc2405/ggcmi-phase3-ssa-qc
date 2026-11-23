# Notice

- [Origin](#origin)
- [Modifications](#modifications)
- [License](#license)
- [Copyright](#copyright)

## Origin

This repository contains quality-check scripts for GGCMI Phase 3 SSA / RiskAssess model output. The scripts are adapted from:

- Upstream repository: [AgMIP-GGCMI/phase3_quality_check](https://github.com/AgMIP-GGCMI/phase3_quality_check)
- Reference commit: `ae30290a89f2e330d7983aa69ede6fc6c8380a7c` (2024-01-05, "adjust for 3a_attrici formats")
- Upstream license: AGPL-3.0

## Modifications

The file headers record the following changes relative to the upstream scripts:

- `GGCMI_phase3_SSA_check_functions.R`: adapted and substantially rewritten on 2026-11-03. Revisions cover core and optional variables, monthly soil moisture, irrigation-specific requirements, and removal of compression-level checks. The report layout was revised on 2026-11-05 into two main reports with supporting files in `details/`.
- `GGCMI_phase3_SSA_check.R`: modified on 2026-11-05. On 2026-11-22 the local paths and model selection moved to `config.yaml`, a run log was added, and line endings were converted to LF.
- The checks are read-only: no NetCDF file is renamed, rewritten, interpolated or repaired.

## License

This work is a modified version of the upstream scripts and is distributed under the GNU Affero General Public License, version 3 only (`AGPL-3.0-only`). The full license text is in [LICENSE](LICENSE). Each R script carries an `SPDX-License-Identifier` line in its header.

## Copyright

The upstream authors keep the copyright of the upstream code (see [Origin](#origin)). The upstream files carry no copyright line.

Copyright (C) 2026 Jiali Cheng
