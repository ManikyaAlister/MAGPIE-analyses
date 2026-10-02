# MAGPIE analyses

Analysis code and data for *Understanding online influence using a custom social
media platform*. Participants (N = 311) interacted
on a bespoke Mastodon instance ("Magpie Social") for three days in one of three
between-subject conditions (`Control`, `Left`, `Right` troll campaigns). Their
beliefs, perceptions, trust, and experience were measured before and after.

This repository reproduces every figure and statistic in the manuscript and the
Supplementary Analyses that can be computed from the shared, de-identified data.
The few exceptions are listed under [What is not reproduced here](#what-is-not-reproduced-here).
Materials and data are also on the OSF: <https://osf.io/mu9wy>.

---

## Quick start: reproduce the paper in about 10 minutes

### 1. Install the software

| Software | Version used | Notes |
|---|---|---|
| [R](https://cran.r-project.org/) | 4.5.3 (any 4.5.x should work) | |
| [Quarto](https://quarto.org/docs/get-started/) | 1.8 (≥ 1.4 should work) | Bundled with recent RStudio. |
| A LaTeX distribution | any | Only needed for `04_supplementary-analyses` (PDF output). Easiest: `quarto install tinytex`. |
| C/C++ compiler toolchain | | Needed by `renv::restore()` to build two Stan dependencies from source, and to refit any model. macOS: `xcode-select --install`. Windows: [Rtools45](https://cran.r-project.org/bin/windows/Rtools/). Linux: `build-essential`, plus the system libraries for `sf` (GDAL, GEOS, PROJ, UDUNITS). |

RStudio is optional but convenient.

### 2. Restore the R package environment

The exact package versions are recorded in [`renv.lock`](renv.lock) with
[renv](https://rstudio.github.io/renv/). Open `MAGPIE-analyses.Rproj` in
RStudio, or start R in this folder (renv then bootstraps itself from
`.Rprofile`), and run:

```r
renv::restore()
```

This installs about 200 packages into a project-local library and leaves your
other R libraries untouched. It takes 5–20 minutes the first time, depending on
how many packages are already cached and how many must be built from source.

> **Why `StanHeaders` and `RcppParallel` are pinned to older versions.**
> `rstan` 2.32.7, the current CRAN release used by `brms`, cannot compile
> models against `StanHeaders` ≥ 2.33 or `RcppParallel` ≥ 6. You would see
> `symbol not found ... tbb::task_scheduler_init`. The lockfile therefore pins
> `StanHeaders` 2.32.10 and `RcppParallel` 5.1.11-2. Do not run
> `renv::update()` on these two packages.

### 3. Render the analyses

From a terminal in the project folder:

```bash
quarto render
```

Or open each `.qmd` in `analysis/` in RStudio and click **Render**. The four
documents render in 1–2 minutes in total. They load the pre-fitted Bayesian
models in [`output/models/`](output/models/) (see [Refitting the models](#refitting-the-models-from-scratch))
and write:

- `analysis/01_demographics.html`, `02_engagement.html`, `03_linear-modelling.html`
- `analysis/04_supplementary-analyses.pdf`, the Supplementary Analyses document
- every manuscript figure, in [`output/figures/`](output/figures/)

`quarto render` is configured in [`_quarto.yml`](_quarto.yml) to run every
document from the project root and to render only `analysis/`.

You may see harmless `Sys.setlocale()` warnings if your shell has no UTF-8
locale set.

---

## Where each result comes from

| Manuscript result | Document → section | Output file |
|---|---|---|
| Sample size per condition; gender, trans, sexual orientation, age, race, education, religion, political ID percentages | `01_demographics` → Demographics tables | |
| "No significant differences between conditions" (demographics, political ID, initial beliefs) | `01_demographics` → *Balance across conditions* | |
| **Figure 1** (sample characteristics) | `01_demographics` → *Combined demographics plot* | `output/figures/combined-demographics.png` |
| Time spent on the platform per participant-day (share of days with 45+ minutes) | `02_engagement` → *Time spent on the platform* | |
| Posts/replies per condition; troll share of content | `02_engagement` (table after time on platform) | |
| χ²(6) = 278.11, composition of engagement types | `02_engagement`, test defined in `R/functions/engagement-chisq.R` | |
| χ²(4) polarity tests for posts, replies, reblogs, favourites | `02_engagement` (table after Fig 2B) | |
| χ²(2) = 64.86, friendly vs non-friendly | `02_engagement` | |
| Troll posts' extra engagement and extra replies | `02_engagement` (printed sentences) | |
| Correlation between initial beliefs and the political slant of each participant's posts, replies, and favourites | `02_engagement` → *Initial beliefs and the political slant of each participant's activity* | |
| **Figure 2** (engagement) | `02_engagement` → *Combine* | `output/figures/combined-engagement.png` |
| Before-survey means/SDs (consensus, relative belief, trust) and experience means/SDs | `03_linear-modelling` → *Initial values of measures* | |
| **Figure 3** (condition effects on beliefs, consensus, relative belief, trust, affective polarisation) | `03_linear-modelling` → *Before/After Change variables* | `output/figures/combined-bar-lm-change-vars-Bayes-TRUE.png` |
| **Figure 4** (experience measures) | `03_linear-modelling` → *Post experiment experience questions* | `output/figures/combined-bar-lm-post-survey-Bayes-TRUE.png` |
| Correlation between initial beliefs and enjoyment, by condition | `03_linear-modelling` → *Did enjoyment … correlate …* | |
| Posts-seen sample (n analysed per condition) and share of troll-initiated content | `03_linear-modelling` → posts-seen section | |
| **Figure 5** (posts-seen coefficients) | `03_linear-modelling` → posts-seen section | `output/figures/posts-seen-coefficients-grid_visibility50.png` |
| Supplementary Analysis 2 (χ² standardised residuals) | `04_supplementary-analyses` §2 | |
| Supplementary Analysis 3 (trust, affective-polarisation, relative-belief sub-measures) | `04_supplementary-analyses` §3 | `output/figures/combined-bar-lm-aff-trust-Bayes-TRUE.png`, `…-relative-Bayes-TRUE.png` |
| Supplementary Analysis 4 (posts-seen sample vs full sample) | `04_supplementary-analyses` §4 | |
| Supplementary Analysis 5 (correlation version of Figure 5; trust sub-measure grids) | `04_supplementary-analyses` §5 | `output/figures/posts-seen-correlations-grid*_visibility50.png`, `posts-seen-coefficients-grid-trust-subscales_visibility50.png` |

---

## Refitting the models from scratch

By default the documents **load saved model output** (`run_models <- FALSE`),
because refitting takes a long time. All saved output can be regenerated from
the shared data:

| Models | How to refit | Time (10-core laptop) |
|---|---|---|
| Condition effects (Figures 3, 4; Supplementary Analysis 3) | Set `run_models <- TRUE` in the setup chunk of `analysis/03_linear-modelling.qmd` and `analysis/04_supplementary-analyses.qmd`, then render. | ~2 h (each model compiles its own Stan program) |
| Posts-seen data (input to Figure 5) | `Rscript R/preprocess/summarise-posts-seen.R`: rebuilds `data/magpie/processed/combined/posts-seen-by-user_visibility50.Rdata` from the raw visibility logs in `data/analytics/`. | < 1 min |
| Posts-seen models (Figure 5, Supplementary Analysis 5) | `Rscript R/models/run-posts-seen-models.R`: fits 189 beta regressions and writes `output/models/posts-seen/`. Delete `output/models/posts-seen/models/*_visibility50.rdata` first; already-fitted models are skipped (`skip_fitted <- TRUE`). These models use 4 chains × 20,000 post-warmup draws, because several interval endpoints lie close to zero. | ~25 min |

All `brm()` calls use a fixed seed (`seed = 2024`), so a refit on the same
machine and package versions gives identical draws.

**Model specification.** Every condition-effect model is a Bayesian beta
regression (`brms`, default priors, 4 chains × 2000 iterations) of the form
`measure_After ~ measure_Before + Condition`. For after-only measures it is
`measure ~ Condition`. Outcomes are rescaled to (0, 1) with the Smithson &
Verkuilen transformation (`scale_to_01()` in `R/functions/lm-functions.R`).
The posts-seen models replace `Condition` with the proportion of posts seen of
one type, are restricted to `Left`/`Right` participants with at least 50 posts
seen, use 4 chains × 20,000 post-warmup draws, and define a post as "seen" when at least 50% of it was on screen for at
least 1000 ms (`_visibility50`). Reported estimates are posterior means with
95% credible intervals.

---

## Data

All files the analyses read are in [`data/`](data/). They are de-identified,
analysis-ready versions of the raw survey, platform, and analytics exports.

| File | Contents |
|---|---|
| `data/magpie/original/combined/before.csv` / `before.Rdata` (`d_before`) | Before survey, one row per participant (N = 311): demographics, beliefs, consensus, relative beliefs, trust, affective polarisation. |
| `data/magpie/original/combined/after.csv` / `after.Rdata` (`d_after`) | After survey (same measures, plus the experience questions). |
| `data/magpie/original/combined/demographics.csv` | Full demographics, including sexual orientation (rows for Before and After). |
| `data/magpie/processed/combined/survey-before-after-change.Rdata` (`d_before_after_change`) | Before, After, and Change (After − Before) for every measure, joined. This is what the models use. |
| `data/magpie/processed/combined/combined_statuses_with_original_annotations.csv` | Every post, reply, and reblog on the platform, with topic and LLM-coded political polarity (`category`). |
| `data/magpie/processed/combined/favourites.csv` | Every favourite: who gave it (`FavouriterID`), to which status (`StatusID`), and when. |
| `data/analytics/processed/{control,left,right}/visibility_events.csv` | Client-side viewport logs: one row per 1000 ms window in which a status was on screen, with the percentage visible. |
| `data/magpie/processed/combined/posts-seen-by-user_visibility50.Rdata` | Per-participant counts and proportions of posts seen (all threads, troll-initiated threads, other threads), built by `R/preprocess/summarise-posts-seen.R`. |

The variable code books are in the preprocessing notebooks
[`R/preprocess/cleanPriorData.Rmd`](R/preprocess/cleanPriorData.Rmd) (Before
survey) and [`R/preprocess/cleanPosteriorData.Rmd`](R/preprocess/cleanPosteriorData.Rmd)
(After survey).

**Preprocessing.** The notebooks in `R/preprocess/` (`clean*Data.Rmd`,
`mergeGlobalData.Rmd`, `data-preprocessing.R`, `fix-corrupted-IDs.R`) document
how the raw exports were cleaned. They read raw, identifiable files that are not
shared, so they are included for transparency and cannot be run from this
repository. `summarise-posts-seen.R` is the exception: it runs from the shared
visibility logs.

---

## Repository layout

```
analysis/          The four analysis documents (start here)
R/
  functions/       Helpers sourced by the analysis documents
  models/          Stand-alone model-fitting scripts (posts-seen models)
  preprocess/      Data cleaning (raw -> analysis-ready); see "Preprocessing" above
data/              Analysis-ready data the documents load
output/
  figures/         Figures written by the analysis documents
  models/          Saved model output the documents load
renv.lock, renv/   Package environment (restore with renv::restore())
_quarto.yml        Project config (execute from root; render analysis/)
```

### `R/functions/`: sourced by the analysis documents

| Script | Role |
|---|---|
| `colour-palettes.R` | Shared condition/topic/polarity colour scales |
| `lm-functions.R` | Smithson rescaling, Bayesian beta-regression fitting (`runMultipleLMs`, `runModelComparisons`), and coefficient/bar plotting helpers |
| `engagement-chisq.R` | Engagement data prep and the chi-square tests (used by `02` and `04`) |
| `posts-seen-correlations-prep.R` | Computes the posts-seen correlations for Supplementary Analysis 5 (`04`) |
| `posts-seen-grid-plots.R` | Builds the Figure 5 coefficient grid and the supplementary correlation grids (`03` and `04`) |
| `plot-linear-relationships.R` | Plotting helper used by preprocessing |

### `R/models/`

| Script | Role |
|---|---|
| `run-posts-seen-models.R` | Fits the Figure 5 posts-seen models and saves their coefficients and LOO output to `output/models/posts-seen/` |
