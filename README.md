# DID EPRC Workshop Site Mockup

This folder is a practice GitHub Pages-style workshop site for the DID EPRC workshop.

Temporary public site:

```text
https://katjanestrickland.github.io/did-eprc-workshop/
```

## Current Contents

- Static HTML mockup: `index.html`, `slides.html`, `code.html`, `data.html`, `resources.html`
- Quarto source skeleton: `_quarto.yml` plus matching `.qmd` pages
- Student-facing starter scripts in `code/`
- Synthetic workshop data in `assets/data/`
- Figures generated from the simulated data in `assets/figures/`

## Local Preview

From this folder, run:

```bash
python3 -m http.server 8765
```

Then open:

```text
http://localhost:8765/index.html
```

## Later GitHub Pages Path

For a quick static deployment, publish the HTML files and assets directly from this folder.
For a Quarto deployment, install Quarto and render the `.qmd` pages before publishing.

See `GITHUB_PAGES_SETUP.md` for the recommended `lcds-lab/did-eprc-workshop` repository setup.
