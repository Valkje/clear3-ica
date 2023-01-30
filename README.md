# Understanding statistical properties of typing and EMA data

For preprocessing of the BiAffect typing data, take a look at `src/preproc_all.R`, which preprocesses all data and puts them into a BIDS-like format, and `src/preproc.R`, which contains the functions that `src/preproc_all.R` uses.

`src/load_dependencies.R` loads all the dependencies that any R code in this repository might need. If you only do preprocessing, you won't need all of them (e.g. you can easily comment out everything from `library(nlme)` to `library(rhdf5)`).

If you are worried about the relatively large portion of incomplete cases, you can check out `imputation.Rmd`. It is less streamlined than anything you will find in the `src` directory, but can generate imputations that make the application of e.g. mixed models more feasible. If you go down this route, be sure to check out Stef van Buuren's [book on multiple imputation](https://stefvanbuuren.name/fimd/) as well.
