# News for the SIACS package

## SIACS 0.2.0 (2026-02-27)

### New Features
- **Wizard Mode GUI**: Added a step-by-step guided Shiny interface
  (`wizard_module.R`, `wizard_helpers.R`, `wizard_defaults.R`) as an
  alternative to the Advanced input tab. The wizard walks users through
  location, time, building, windows, lighting, physical environment, outdoor
  concentrations, emissions, and output configuration.
- **Pre-populated defaults**: The Wizard pre-fills all inputs from the
  bundled example input files, making it easy to explore the model with
  representative data before customising.

### Improvements
- Updated `advanced_module.R` with additional input handling and UI
  improvements.
- `SIACSLights.R`: improved robustness of indoor light flux calculations;
  duration mismatch now raises a `warning()` instead of stopping execution,
  allowing simulations to proceed with available data.
- `shared.r`: replaced all hardcoded file paths and `library()` calls with
  CRAN-compliant `system.file()` path resolution and package-level imports.

### Documentation
- All three exported functions (`SIACS`, `SIACS.batch`, `runSIACSApp`) now
  have complete documentation including parameter descriptions with units,
  return value structure, and runnable examples.
- Added `README.md`, `NEWS.md`, `CITATION`, and introductory vignette
  (`vignettes/SIACS_intro.Rmd`).
- `DESCRIPTION` updated with full `Authors@R` field and all package
  dependencies listed in `Imports`.

---

## SIACS 0.1.0 (initial release)

- Initial CRAN submission.
- Core simulation engine (`SIACS()`) using the LSODE ODE solver with
  full user-supplied Jacobian.
- SAPRC99 and SAPRC07T chemical mechanisms.
- TUV-based indoor photolysis rate computation from outdoor solar flux,
  window geometry, glass transmission, and artificial lighting spectra.
- ASHRAE-based ventilation model (infiltration, balanced, unbalanced,
  natural ventilation).
- Surface deposition with activity-dependent adjustment for ozone/skin
  deposition.
- Batch processing mode (`SIACS.batch()`) with optional parallel execution.
- Interactive Shiny GUI (`runSIACSApp()`) in Advanced mode.
- Optional analyses: time derivatives (dy/dt), mass balance component
  separation, sensitivity coefficients, uncertainty propagation.
