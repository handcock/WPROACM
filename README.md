WPROACM
==========

Welcome to the All Cause of mortality and Excess Death Monitoring calculator!

This calculator has been developed by the World Health Organization, Western Pacific Region in conjunction with the Department of Statistics at
UCLA .

This web application is written with the Shiny framework and development is via GitHub. More information on Shiny and our GitHub repository can
be found in the resource links on the right.

The app can be run in two ways:  

* Online, through the shinyapps server with this link:   https://handcock.shinyapps.io/WPROACM  

* Offline by first installing it on your local machine using the WPROACM R package. First instal `R` (if you have not already). Then open `R` and type in the `R` Console:

```r
source("http://www.stat.ucla.edu/~handcock/WPROACM.R")`
```

then `R` should install the app and close. There will then be an app call "WPROACM" in your "Documents" folder. If you double-click on that, it will run the calculator in the browser (no R/Rstudio involved).

* Alternatively, you can install it on your local machine using the WPROACM R package. First install it for `R`:
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
