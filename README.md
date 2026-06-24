# MAGPIE analyses

Analysis code for *Understanding online influence using a custom social media
platform* (Magpie Social). Participants interacted on a bespoke Mastodon
instance over a week in one of three between-subject conditions (`Control`,
`Left`, `Right` troll campaigns); we measured beliefs, perceptions, trust, and
experience before and after.

This repository contains everything needed to reproduce the analyses and figures
in the paper.

## Where to start

The four numbered documents in [`analysis/`](analysis/) are the analyses. Each is
a self-contained Quarto document; reading them top to bottom walks through every
result in the paper. They are also rendered in html/pdf. 

This project uses [Renv](https://rstudio.github.io/renv/articles/renv.html) to enable a reproduce R environment. Type `renv::restore()` into the r console after opening the `.Rproj` file (`MAGPIE-analyses.Rproj`) to install all packages and dependencies. 

| Document | Covers | Paper |
|---|---|---|
| [`analysis/01_demographics.qmd`](analysis/01_demographics.qmd) | Sample demographics, political identification, initial beliefs | Figure 1 |
| [`analysis/02_engagement.qmd`](analysis/02_engagement.qmd) | Engagement/activity on the platform + chi-square tests | Figure 2 |
| [`analysis/03_linear-modelling.qmd`](analysis/03_linear-modelling.qmd) | Bayesian models of belief/perception/trust/experience change, and the posts-seen models | Figures 3–5 |
| [`analysis/04_supplementary-analyses.qmd`](analysis/04_supplementary-analyses.qmd) | Chi-square standardised residuals, sub-measure breakdowns, sample comparison, correlation-based posts-seen analysis | Supplementary |

Render them with `quarto render` (configured via [`_quarto.yml`](_quarto.yml) to
run from the project root and render only `analysis/`), or open the project in
RStudio (`MAGPIE-analyses.Rproj`) and render individually.

By default the analysis documents **load pre-fitted model output**
(`run_models <- FALSE`) from [`output/models/`](output/models/), so they render
quickly. Set `run_models <- TRUE` in `03`/`04` to refit the Bayesian models from
scratch (requires `brms`/Stan).

## Repository layout

```
analysis/          The four analysis documents (start here)
R/
  functions/       Helpers sourced by the analysis documents
  preprocess/      Data cleaning: raw data -> analysis-ready data
  models/          Model fitting (posts-seen models)
data/              Input data the analyses load
output/
  figures/         Figures written by the analysis documents
  models/          Fitted-model output the documents load
_quarto.yml        Project config (execute from root; render analysis/)
```

### `R/functions/` — sourced by the analysis documents

| Script | Role |
|---|---|
| `colour-palettes.R` | Shared condition/topic/polarity colour scales |
| `lm-functions.R` | Bayesian beta / GLM model fitting + coefficient and bar plotting helpers |
| `engagement-chisq.R` | Engagement data prep and the three chi-square tests (used by `02` and `04`) |
| `posts-seen-correlations-prep.R` | Computes the posts-seen correlations for the supplementary figure (`04`) |
| `posts-seen-grid-plots.R` | Builds the Figure 5 coefficient/correlation grids (`03` and `04`) |
| `plot-linear-relationships.R` | Plotting helper used by preprocessing |

## Requirements

R (≥ 4.5) with `quarto`, and the packages used across the documents:
`here`, `tidyverse`, `brms`, `ggpubr`, `patchwork`, `cowplot`, `correlation`,
`dotwhisker`, `broom`, `see`, `sf`, `rnaturalearth`, `rnaturalearthdata`.
