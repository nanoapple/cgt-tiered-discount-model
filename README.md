# CGT Tiered Discount Model

This repository contains a reproducible modelling pipeline for exploring a potential reform to Australia's Capital Gains Tax (CGT) discount. In Australia, CGT is paid on capital gains realised when certain assets are sold (for example, shares or investment property), and the CGT "discount" refers to the long-standing rule that allows eligible individuals to include only part of a capital gain in taxable income after meeting specific conditions (such as holding the asset for a minimum period).

The model in this repository simulates a synthetic population and calibrates a tiered CGT discount schedule using a **four-tier banded taper** mechanism with **smoothstep boundary transitions**, with the aim of making policy trade-offs and distributional effects more transparent and testable under clearly stated assumptions.

---

## What this is (and is not)

### Purpose
- Demonstrate a transparent calibration and monitoring scaffold for a tiered CGT discount design.
- Provide auditable outputs (tier summaries, band compliance metrics, diagnostics) from a fully reproducible run.
- Support policy discussion by making the consequences of design choices explicit under clearly stated assumptions.

### Not an administrative tool
- This repository is **not** used to determine any individual taxpayer outcome.
- The population data are **synthetic** and generated for modelling and demonstration purposes only.
- Outputs are exploratory and illustrative and should not be treated as definitive forecasting or operational policy advice without further expert review, legislative drafting, and administrative feasibility assessment.

---

## Policy architecture

A calibration framework for Australia's CGT discount reform, implementing a **four-tier banded taper** system with smoothstep boundary transitions.

The key design idea is to treat each tier as a **discount band** (a target range), rather than a single point target, and to evaluate success using **band compliance** (the share of cases landing within the intended band).

| Tier | Name | Discount Band (target range) | Target Share (illustrative) |
|------|------|------------------------------|-----------------------------|
| 0 | Compassionate | 62.5% (flat rule) | 5% |
| 1 | Long holding / lowest distortion | 25–30% | 55% |
| 2 | Transitional | 15–20% | 25% |
| 3 | Higher distortion / no discount | 0–5% | 15% |

> Concrete tier thresholds, discount bands, and target shares are parameterised in the scripts and can be documented per "submission build".

---

## Repository structure

```
cgt-tiered-discount-model/
├── run_all.R                        # Single entry point (start here)
├── cgt-tiered-discount-model.Rproj  # RStudio project file
├── R/
│   ├── generate_data.R              # Synthetic ATO data generator (copula + archetypes)
│   ├── calibration_taper.R          # Baseline calibration (proposal weights)
│   ├── calibration_behavioural.R    # Behavioural weight variant (optional)
│   ├── taper_functions.R            # Core: smoothstep, banded taper, band penalty
│   ├── metrics.R                    # Tier summary, calibration quality, loss decomposition
│   └── plots.R                      # Diagnostic visualisations (incl. envelope curve)
├── data/
│   └── README.md                    # Data strategy documentation
├── outputs/                         # Generated artefacts (tables, figures)
├── docs/                            # Governance and parameter documentation
├── .gitignore
└── LICENSE
```

---

## Quick start: clone and run

> [!TIP]
> **New to GitHub?** Follow the steps below to download and run this project on your machine. You only need [R](https://cran.r-project.org/) and (optionally) [RStudio](https://posit.co/download/rstudio-desktop/).

### Step 1 — Install dependencies

Open R or RStudio and run:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "scales", "MASS"))
```

### Step 2 — Clone the repository

**Option A: via RStudio (recommended)**

1. `File → New Project → Version Control → Git`
2. Repository URL:
   ```
   https://github.com/nanoapple/cgt-tiered-discount-model.git
   ```
3. Choose a local directory, then click **Create Project**

**Option B: via command line**

```bash
git clone https://github.com/nanoapple/cgt-tiered-discount-model.git
cd cgt-tiered-discount-model
```

Then open `cgt-tiered-discount-model.Rproj` in RStudio.

### Step 3 — Run the full pipeline

In the RStudio Console:

```r
source("run_all.R")
```

### What to expect

1. **Data generation** (~10–15 seconds) — creates 200,000 synthetic family groups
2. **Calibration** (~1–2 minutes) — runs 10 multi-start Nelder-Mead optimisations
3. **Output** — prints tier summary, band compliance metrics, and produces 6 diagnostic plots

If everything runs without error, the repository is fully self-contained and reproducible.

### Configuration

Edit the flags at the top of `run_all.R` to customise:

```r
GENERATE_DATA   <- TRUE      # FALSE to reuse existing data
RUN_BASELINE    <- TRUE      # Proposal-weight calibration
RUN_BEHAVIOURAL <- FALSE     # Behavioural weight variant (set TRUE to enable)
N_POPULATION    <- 200000    # Population size
SEED            <- 2026
```

---

### Expected outputs (typical)

The pipeline is designed to write standard outputs into `outputs/` (file names may vary by version):

- `tier_summary.csv` — tier counts, summary statistics
- `band_compliance.csv` — share landing within each tier's target band
- `calibration_diagnostics.csv` — targets vs achieved, error metrics
- Optional figures (e.g. `fig_*.png`)

---

## Data policy (synthetic only)

- This repository uses **synthetic data** generated by included scripts for demonstration and calibration.
- **No real taxpayer, administrative, or identifiable data** is included or required.
- Large generated artefacts (for example, full synthetic CSV files) are typically not tracked in git history and should be regenerated via `run_all.R`, or provided separately via Releases/Zenodo for a fixed "submission build".

---

## How to cite

If you are referencing a frozen submission build, cite the archived release (for example, a Zenodo DOI linked to the `vX.Y-submission` tag).

Suggested wording (replace DOI):

> The reproducible modelling pipeline and submission build are archived on Zenodo (DOI: `10.5281/zenodo.XXXXXXX`), corresponding to the GitHub release tag `vX.Y-submission`.

---

## Licence

MIT

---

## Contact

Maintainer: **Shen Yang** — shen.yang.au@hotmail.com

---

## Note

I have a background in data analytics and the mental health sector, and I am making this submission independently in good faith and with the sole intention of offering constructive input into Australia's CGT reform discussion. The analysis and any accompanying modelling are exploratory, based on stated assumptions, and are not guaranteed to be fully valid, complete, or directly implementable without further expert review, legislative drafting, and operational assessment.

Context for the inquiry: [Senate Committee on the Operation of the Capital Gains Tax Discount](https://www.aph.gov.au/Parliamentary_Business/Committees/Senate/Operation_of_the_Capital_Gains_Tax_Discount/CapitalGainsTaxDiscount)
