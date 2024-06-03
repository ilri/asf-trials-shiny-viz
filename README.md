# asf-trials-shiny-viz
A Shiny app for the visualization of ASF vaccine trials, running online [here](https://jean-baka.shinyapps.io/ASF_trials).

This app fetches data collected through an ODK tool (programmed as per the published XLSForm) via an OnaData repository whose API key one must enter in the app. The folder named ODK-tools provides the XLSForm as well as the ancillary media files.

Currently, disease severity indices shown in this app are calculated according to [the scoring scheme](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3120964/bin/mmc1.pdf) developed by [Katherine King and co-authors](https://doi.org/10.1016%2Fj.vaccine.2011.04.052).
