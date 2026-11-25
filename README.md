# Darwin Core and BMDE

This repo is for investigation of the Bird Monitoring Data Exchange (BMDE) standard, and how it relates to Darwin Core (DwC) data standards.

## Rendered Notebooks

The quarto `*.qmd` files in the `notebooks/` subfolder are rendered to `*.html` pages in the `docs/` subfolder. The docs directory is used as the source for serving these files up in  GitHub pages, [here](https://jeffcullis.github.io/darwin-birds)

For viewing the rendered pages locally, either render each one in RStudio, or via the command line:
```
cd /path/to/darwin-birds
quarto render docs/
cd docs/
python3 -m http.server 8000
```

## Directory Layout

Note that the basic folder structure used here is copied from another repository. For more information see the [R Data Science Template](https://github.com/paulovillarroel/r-data-science-template/) source repository.
