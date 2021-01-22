WPROACM
==========

Welcome to the All Cause of mortality and Excess Death Monitoring calculator!

This calculator has been developed by the World Health Organization, Western Pacific Region in conjunction with the Department of Statistics at
UCLA .

This web application is written with the Shiny framework and development is via GitHub. More information on Shiny and our GitHub repository can
be found in the resource links on the right.

The app can be run in two ways:  

* Through the shinyapps server with this link:   https://handcock.shinyapps.io/WPROACM  

* On your local machine using the WPROACM R package. First install it for `R`:
```r
install.packages("devtools")
devtools::install_github("handcock/WPROACM")
```
then run it from `R`:
```r
WPROACM::run()
```

More info on the WPROACM wiki:   
https://github.com/handcock/WPROACM/wiki

Please use the GitHub repository to report bugs or request features:
https://https://github.com/handcock/WPROACM

<!-- badges: start -->
[![R-CMD-check](https://github.com/handcock/WPROACM/workflows/R-CMD-check/badge.svg)](https://github.com/handcock/WPROACM/actions)
<!-- badges: end -->
