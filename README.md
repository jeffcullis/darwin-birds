# Darwin Core and BMDE

This repo is for investigation of the Bird Monitoring Data Exchange (BMDE) standard, and how it relates to Darwin Core (DwC) data standards.

## Initial Setup

All required packages are stored in the `renv.lock` file. To install these dependencies, after cloning and cd to the repo, run:

```
> install.packages("renv")
> renv::restore()
```

This may prompt that the project hasn't yet been activated, in which case choose option 1: Activate the project and use the project library.

## Rendered Notebooks

The quarto `*.qmd` files in the `notebooks/` subfolder are rendered to `*.html` pages in the `docs/` subfolder. The docs directory is used as the source for serving these files up in  GitHub pages, [here](https://jeffcullis.github.io/darwin-birds)

For viewing the rendered pages locally, either render each one in RStudio, or via the command line:
```
cd /path/to/darwin-birds
quarto render docs/
cd docs/
python3 -m http.server 8000
```
Then open localhost:8000 in your browser.

## Directory Layout

The basic folder structure used here is copied from another repository. For more information see the [R Data Science Template](https://github.com/paulovillarroel/r-data-science-template/) source repository.
