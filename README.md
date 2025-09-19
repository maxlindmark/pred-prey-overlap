## Weak effects of local prey density and spatial overlap on predation intensity in a temperate marine ecosystem
In this paper we fit spatiotemporal models to stomach content data and biomass density of cod to estimate biomass weighted indices of predation by cod on sprat, herring and the isopod *Saduria entomon*. Next we correlated these indices with spatial overlap, to see if predation (per capita and total), is higher in years when predators and prey overlap more.

### Reproducing Results

To reproduce our results you can either:

1. Fork the repository, clone it, open a new RStudio project with version control, and paste the repo url

2. Download a zip and work locally on your computer

We use [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage package versions. Once you've downloaded the project, run `renv::restore()` in your current working directory. This will install the package versions we used when this repository was archived. Note that packages are installed in a stand-alone project library for this paper, and will not affect your installed R packages anywhere else! `renv` does *not* help with different versions of R. We used R version 4.3.2, and ran the analysis on a 24 GB Apple M2 Sequoia 15.6.1 laptop.

### Repository structure

`R`: code to prepare data, fit models, code meant to be sourced, calculate indices, and make figures.

`data`: trawl survey data, prediction grid and stomach content data.

`figures`: figures for paper are saved here

`output`: sdmTMB model objects (.rds files). The cod density model is omitted because it's too large and needs to be refitted. On my machine it takes 18 minutes.
