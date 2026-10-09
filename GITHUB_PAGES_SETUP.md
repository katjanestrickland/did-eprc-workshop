# GitHub Pages Setup

Current temporary public URL:

```text
https://katjanestrickland.github.io/did-eprc-workshop/
```

Current temporary repository:

```text
katjanestrickland/did-eprc-workshop
```

Eventual LCDS public URL:

```text
https://lcds-lab.github.io/did-eprc-workshop/
```

Eventual LCDS repository:

```text
lcds-lab/did-eprc-workshop
```

## Current Temporary Setup

The site has been published from the personal repository `katjanestrickland/did-eprc-workshop` using GitHub Pages from:

- Branch: `main`
- Folder: `/root`

## Later LCDS Setup

1. Create a new public repository under the `lcds-lab` organization named `did-eprc-workshop`.
2. Upload or push the contents of this `workshop-site` folder to the root of that repository.
3. In the repository, go to `Settings` > `Pages`.
4. Under `Build and deployment`, choose:
   - Source: `Deploy from a branch`
   - Branch: `main`
   - Folder: `/root`
5. Save the Pages settings.

After GitHub Pages finishes building, the site should be available at:

```text
https://lcds-lab.github.io/did-eprc-workshop/
```

## Later LCDS Training Tab Integration

Once the standalone workshop site is live, add a Training-tab link in the main LCDS site:

```text
https://lcds-lab.github.io/did-eprc-workshop/
```

The main LCDS site repository is:

```text
lcds-lab/lcds-lab.github.io
```

Current local check: this Codex session can read that repository but does not have push access to it.

## Notes

- This mock site currently uses static HTML, so it can be deployed directly without installing Quarto.
- The `.qmd` files are included as a future Quarto version of the same content.
- The `.nojekyll` file is included so GitHub Pages serves files and folders exactly as written.
