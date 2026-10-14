# EPRC DID Workshop

October 22, 2026: Lecture 12-1 PM; Hands-on R lab 1-1:30 PM;
Questions & discussion 1:30-2 PM Eastern. Data/lab revised October 14.

Temporary public site:

```text
https://katjanestrickland.github.io/did-eprc-workshop/
```

## Current Contents

- Static pages: Home, Slides, Workshop, Code, Data, Resources, and Codebook.
- `.qmd` sources are rendered by Pandoc through `build-site.cjs`.
- `workshop.Rmd` is the executable source for the guided lab and downloaded R script.
- Synthetic workshop data in `assets/data/`
- Figures generated from the simulated data in `assets/figures/`

## Rebuild and Preview

Install R packages listed in the lab, plus Node and Pandoc. From this folder:

```bash
Rscript build-lab.R
node build-site.cjs
```

Then open `index.html` in a browser. No development server is required.
The build executes all lab chunks and verifies manual calculations against TWFE/CSA.
The R script is extracted from the R Markdown, so download code and page code agree.

Both teaching datasets contain 160 districts over seven years, without adjustment
covariates. Models and figures use `outcome_raw`. See `codebook.qmd` for generation
details, treatment schedules, missing values, and the exact outcome conversion.

## Later GitHub Pages Path

Publish the generated HTML and assets directly from this repository. The existing
`_quarto.yml` is retained as a future migration scaffold; the current build uses
R/knitr plus Pandoc and does not require Quarto. Historical data and instructor
validation files are archived outside this public repository.

See `GITHUB_PAGES_SETUP.md` for the recommended `lcds-lab/did-eprc-workshop` repository setup.
