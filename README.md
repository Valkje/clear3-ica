# Data fusion through independent component analysis

Analysis source code accompanying the manuscript 'Smartphone keyboard dynamics predict affect in suicidal ideation'. For a high-level overview of the analysis pipeline, please refer to one of the `pipeline.*` files. Once downloaded and opened in a browser, `pipeline.nb.html` is the easiest to interpret; `pipeline.pdf` contains the same content but in a different format. `pipeline.Rmd` contains the corresponding source code.

## Set-up

Install R (we used version 4.2.2) and the dependencies listed in `src/load_dependencies.R`. When running R markdown files, make sure to change the working (`wd`), data (`dat_dir`), image (`man_img_dir`) and any other directory paths at the top of the file.

## Code structure

The analysis pipeline progresses through three main stages: Preprocessing, temporal independent component analysis (ICA), and linear mixed-effects modelling. The development of these stages has taken place within several different R markdown files, which are still present in the `dev` directory for archival purposes, but all relevant functions have been moved into the `src` directory and are being called from `pipeline.Rmd`.

At some point, we created some visual aids to help us assess data quality and quantity and determine appropriate exclusion criteria. These aids have taken the form of a Shiny app, which can be found as `shiny/missingness/app.R`. Running this app will open a dashboard that allows you to play with certain exclusion thresholds (the default values are the settings that we have used for the manuscript). At the bottom of the page, you will find the option to save the resulting data set and some figures to files. File paths can be set within `app.R`.

For the sensitivity analyses in the Supplement, we made use of a helper script that allowed us to cycle through all ICA solutions, called `src/sens_helper.R`.
