require(utils)
require(grDevices)
require(graphics)
require(stats)
require(methods)

require("readxl")
require("writexl")
require("RColorBrewer")
require("ggplot2")
require("lattice")
require("latticeExtra")

#data(ExampleCountries, package = "WPROACM")
load("./data/ExampleCountries.RData")
BRGcol <- "darkred"
CUGcol <- "darkorange"
obsblue <- "#076EC3"
histblue <- "#83B6E1"
tgray3 <- adjustcolor("gray", alpha.f = 0.3)
tgray7 <- adjustcolor("gray", alpha.f = 0.7)

shinyServer(
  function(input, output, session) {
    oldoptions <- options()
    on.exit(options(oldoptions))
    options(digits = 3)


    # Reactive Expressions ----------------------------------------------------
    # These expressions contain most of the code from the ergm package that we will
    # be using. Objects created with a reactive expression can be accessed from any
    # other reactive expression or render functions and they only get re-run when
    # their values are outdated. Since many of our render functions will be calling
    # the same ergm objects, using reactive expressions will help the app run much
    # faster.


    values <- reactiveValues()

    # when two options are available to the user, or when we need to know if one
    # variable is outdated this reactive value will keep track of the state
    state <- reactiveValues(
      symmdir = FALSE, plotperc_dd = FALSE,
      plotperc_gd = FALSE, gof = 0
    )

    # move to Help page when user clicks Help link button
    observe({
      if (input$helpLink == 0) {
        return()
      }
      isolate({
        updateTabsetPanel(session, "navbar", selected = "tab6")
      })
    })

    #move to Data panel when user clicks Get Started button
    observe({
      if(input$startButton == 0) {return()}
      isolate({
        updateTabsetPanel(session, 'navbar', selected='tab2')
      })
    })

    #update active tab in navbar when arrows are clicked
    leftarrowclicks <- reactive({
      input$dataleft+input$plotleft
    })
    rightarrowclicks <- reactive({
      input$dataright+input$plotright
    })
    observe({
      if(leftarrowclicks() == 0) {return()}
      tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6')
      current <- isolate(which(input$navbar==tabOptions))
      updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
    })
    observe({
      if(rightarrowclicks() == 0) {return()}
      tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6')
      current <- isolate(which(input$navbar==tabOptions))
      updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
    })


    ## Data Selection ------------------------------------------------------

     output$selectsheet <- renderUI({
       # input$rawdatafile comes as a dataframe with name, size, type and datapath
       # datapath is stored in 4th column of dataframe
         filepath <- input$rawdatafile[1, 4]
         filename <- input$rawdatafile[1, 1]
         fileext <- substr(filename, nchar(filename) - 3, nchar(filename))
 
         sheets <- ""
         if (input$filetype == 1) {
           validate(
             need(
               fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
               "Upload an Excel file"
             )
           )
           try({
             ACM_sheets <- readxl::excel_sheets(path=paste(filepath))
           })
           sheets <- c()
           for(i in seq_along(ACM_sheets)){
            if(ACM_sheets[i] != "Instructions"){
             ACM_all <- readxl::read_excel(path=paste(filepath), sheet = ACM_sheets[i])
             is.data <- apply(!is.na(as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])),1,sum)
             skip <- max(is.data) < 24
             if(!skip){
               sheets <- c(sheets, ACM_sheets[i])
             }
            }
           }
         }
       selectizeInput('chosesheet', label=NULL,
         choices=c("Choose a region (if there is more than one)" = '', sheets ))
     })

     output$age <- renderUI({
       selectizeInput('age_list', label=NULL,
         choices=output_age() )
     })

     output$EDage <- renderUI({
       selectizeInput('age_list', label=NULL,
         choices=output_age() )
     })

#     output$genderlabels <- renderUI({
#      if (is.null(input$rawdatafile)) {
#        genderlabels <- NULL
#      } else {
#        filepath <- input$rawdatafile[1, 4]
#        filename <- input$rawdatafile[1, 1]
#        fileext <- substr(filename, nchar(filename) - 3, nchar(filename))
#
#         validate(
#            need(
#              fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
#              "Upload an Excel file"
#            )
#          )
#          try({
#            ACM_all <- readxl::read_excel(path=paste(filepath), sheet = input$chosesheet)
#          })
#          max.types <- dim(ACM_all)[1]
#          max.times <- dim(ACM_all)[2]
#          is.data <- apply(!is.na(as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])),1,sum)
#          skip <- max(is.data) < 24
#          a <- as.matrix(ACM_all[5:nrow(ACM_all),3:ncol(ACM_all)])[is.data > 24,]
#          mode(a) <- "numeric"
#          a <- round(a)
#          age <- as.data.frame(ACM_all[,1])[seq(5,nrow(a)+2,by=3),1]
#          sex <- as.data.frame(ACM_all[5:nrow(ACM_all), 2])[is.data > 24,]
#          genderlabels <- sort(unique(age))
#       }
#       selectizeInput('gender', label=NULL,
#         choices=c("Select Sex" = '', genderlabels ))
#     })

    ACMinit <- reactive({
      # input$rawdatafile comes as a dataframe with name, size, type and datapath
      # datapath is stored in 4th column of dataframe
      if (is.null(input$rawdatafile)) {
        ACM_var <- NULL
      } else {
        filepath <- input$rawdatafile[1, 4]
        filename <- input$rawdatafile[1, 1]
        fileext <- substr(filename, nchar(filename) - 3, nchar(filename))

        ACM_var <- NULL
        if (input$filetype == 1 && !is.null(input$chosesheet) && input$chosesheet != "") {
          validate(
            need(
              fileext %in% c("xls", "xlsx", "XLS", "XLSX"),
              "Upload an Excel file"
            )
          )
          try({
            ACM_all <- readxl::read_excel(path=paste(filepath), sheet = input$chosesheet)
          })
          if(ACM_all[4,3]=="WEEKS"){
            len.header <- 6
          }else{
            len.header <- 5
          }
          max.types <- dim(ACM_all)[1]
          max.times <- dim(ACM_all)[2]
          is.data <- apply(!is.na(as.matrix(ACM_all[len.header:nrow(ACM_all),3:ncol(ACM_all)])),1,sum)
          skip <- max(is.data) < 24
          a <- as.matrix(ACM_all[len.header:nrow(ACM_all),3:ncol(ACM_all)])[is.data > 24,]
          mode(a) <- "numeric"
          a <- round(a)
          age <- as.data.frame(ACM_all[,1])[seq(len.header,nrow(a)+3,by=3),1]
          ACM_var <- data.frame(
              REGION=rep(input$chosesheet,length(a)),
               AGE_GROUP=rep(rep(age,3),rep(ncol(a),3*length(age)))[1:length(a)],
               SEX=rep(as.data.frame(ACM_all[len.header:nrow(ACM_all), 2])[is.data > 24,],rep(ncol(a),nrow(a))),
               YEAR=rep(as.numeric(ACM_all[1, 3:ncol(ACM_all)]),nrow(a)),
               PERIOD=rep(as.numeric(ACM_all[ifelse(len.header==6,5,3), 3:ncol(ACM_all)]),nrow(a)),
               NO_DEATHS=as.vector(t(a))
                                )
        # ACM_var <- ACM_var[ACM_var$SEX !="",]
        }
      }
      if (input$filetype == 2) {
        if (input$samplecountry == "") {
          ACM_var <- NULL
        } else {
          country_name <- c("Australia", "Japan", "Republic_of_Korea", "New_Zealand", "Philippines")[
            match(input$samplecountry, c(
              "Australia", "Japan",
              "Republic of Korea", "New Zealand", "Philippines"
            ))
          ]
          ACM_var <- eval(parse(text = country_name))
        }
      }
      return(ACM_var)
    })

    iso3 <- reactive({
      iso3 <- NULL
      if (input$filetype == 2) {
        if (input$samplecountry != "") {
          iso3 <- c("AUD", "JPN", "KOR", "NZL", "PHL")[
            match(input$samplecountry, c(
              "Australia", "Japan",
              "Republic of Korea", "New Zealand", "Philippines"
            ))
          ]
        }
      }
      return(iso3)
    })

    Countryname <- reactive({
      name <- input$rawdatafile[1, 1]
      if (input$filetype == 2) {
        name <- input$samplecountry
      }
      name
    })

    # compute the spline model  for the expected deaths
    output_spline <- reactive({
      if (!is.data.frame(ACMinit())) {
        return()
      }
      calculate_spline(ACMinit())
    })

    output_age <- reactive({
      if (!is.data.frame(ACMinit())) {
        return()
      }
      calculate_age(ACMinit())
    })

    output$download_t <- renderUI({
      if(input$template_country != ""){
      filename = 
       paste0(c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
              match(input$template_country, c(
              "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx") 
       file.copy(paste0("./XLSX/",c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
              match(input$template_country, c(
              "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx"),
              to = paste0(path.expand("~"),"/Downloads/",
               c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
              match(input$template_country, c(
              "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx"))
      return(paste0(filename," downloaded to",path.expand("~"),"/Downloads")) 
      }else{
      return(paste0("The file will be downloaded to ",path.expand("~"),"/Downloads")) 
      }
    })

    output$download_t2 <- downloadHandler(
      filename = function(){ 
       paste0(c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
              match(input$template_country, c(
              "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx") 
      },
      content = function(file) {
       file.copy(from=paste0("./XLSX/",c("AUS", "PHL", "PYF", "Data Entry Template - monthly", "Data Entry Template - weekly")[
              match(input$template_country, c(
              "Australia", "Philippines", "French Polynesia", "Generic Monthly", "Generic Weekly"))],".xlsx"),
              to = file)
      }
    )

    output$EDdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_ED.xlsx", sep = "")
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      content = function(file) {
        writexl::write_xlsx(x=output_spline(), path = file)
      }
    )

    output$ACMplotdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_plot.pdf", sep = "")
      },
      content = function(file) {
        pdf(file = file, height = 10, width = 10)
        ACM_var <- output_spline() 
        #     c_data <- ACM_var %>% filter(ACM_var$COUNTRY == Countryname() & ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age)
        c_data <- ACM_var[ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age,]
        if(nrow(c_data) < 2) {
          if (ACM_var$WM_IDENTIFIER[1] == "Month") {
            p <- ACM_var[1:12,] %>%
              ggplot() +
              geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
              geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
              geom_line(aes(x = PERIOD, y = EXPECTED, colour = "expected")) +
              scale_colour_manual(name="",
                                  values=c(recorded="black", expected="indianred")) +
              scale_x_continuous(name = "Month in 2020 through 2021",
                                 labels = c("JAN", "FEB", "MAR", "APR",
                                            "MAY", "JUN", "JUL", "AUG",
                                            "SEP", "OCT", "NOV", "DEC"),
                                 breaks = 1:12) +
              scale_y_continuous(name = "Monthly Deaths") +
              labs(
                title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
                subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
              ) +
              theme_bw()
        } else {
          p <- ACM_var[1:52,] %>%
            ggplot() +
            geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
            geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            geom_line(aes(x = PERIOD, y = EXPECTED, colour = "expected")) +
            scale_colour_manual(name="",
                                values=c(recorded="black", expected="indianred")) +
            scale_x_continuous(name = "Week in 2020 through 2021") +
            scale_y_continuous(name = "Weekly Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="darkred"))    
        print(p)
        dev.off()
      }else{
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month in 2020 through 2021", "Week in 2020 through 2021")
      # Spline Regression
      if (input$check_spline & !input$check_avg) {
        subtitle <- paste0("deaths in ", bquote(2020), " compared to negative binomial regression on 2015-19")
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>%
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
          geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          geom_line(aes(x = PERIOD, y = EXPECTED, colour = "expected")) +
          scale_colour_manual(name="",
                              values=c(recorded="black", expected="indianred")) +
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # historical average
      if (input$check_avg & !input$check_spline) {
        subtitle <- paste0("deaths in ", bquote(2020), " compared to historical average on 2015-19")
        p <- c_data[c_data$SERIES == "Historical average",] %>%
          ggplot() +
          geom_line(aes(x = PERIOD, y = EXPECTED, colour = "average")) +
          geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
          scale_colour_manual(name="",
                              values=c(recorded="black", average="cyan2")) +
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Historical average",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # Both neg binom and hist avg
      if (input$check_avg & input$check_spline) {
        subtitle <- paste0("deaths in 2020 compared to negative binomial regression and historical average on 2015-19")
        p <- c_data %>%
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
          geom_line(aes(x = PERIOD, y = EXPECTED, group = SERIES, colour = SERIES)) +
          scale_colour_manual(name="", labels = c("expected", "average", "recorded"),
                              values=c("indianred", "cyan2", recorded="black")) + 
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 16
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Historical average",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      #neither box checked, just show actual
      if (!input$check_avg & !input$check_spline){
        subtitle = paste0("deaths in 2020")
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>% 
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Historical average",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
        print(p)
        dev.off()
      }
      }
    )

    output$EDplotdownload <- downloadHandler(
      filename = function() {
        paste(Countryname(), "_plot.pdf", sep = "")
      },
      content = function(file) {
        pdf(file = file, height = 10, width = 10)
      ACM_var <- output_spline() 
#     c_data <- ACM_var %>% filter(ACM_var$COUNTRY == Countryname() & ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age)
      c_data <- ACM_var[ACM_var$SEX == input$EDgender & ACM_var$AGE_GROUP == input$EDage,]
      if(nrow(c_data) < 2) {
        if (ACM_var$WM_IDENTIFIER[1] == "Month") {
          #lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          #upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          lower <- ACM_var[ACM_var$SERIES == "Cyclical spline","LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          upper <- ACM_var[ACM_var$SERIES == "Cyclical spline","UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          p <- ACM_var[1:12,] %>%
            ggplot() +
            #geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            #geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            #geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                     fill = "indianred", alpha= 0.1) +
            geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower[1:12], ymax = -1*upper[1:12]), 
                          linetype = 1, colour = "indianred") +
            scale_colour_manual(name="",
                                values=c(excess_from_expected="indianred")) +
            geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
            scale_x_continuous(name = "Month in 2020 through 2021",
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:12) +
            scale_y_continuous(name = "Monthly Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        } else {
          #lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          #upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          lower <- ACM_var[ACM_var$SERIES == "Cyclical spline","LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          upper <- ACM_var[ACM_var$SERIES == "Cyclical spline","UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          p <- ACM_var[1:52,] %>%
            ggplot() +
            #geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            #geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            #geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                     fill = "indianred", alpha= 0.1) +
            geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower[1:52], ymax = -1*upper[1:52]), 
                          linetype = 1, colour = "indianred") +
            scale_colour_manual(name="",
                                values=c(excess_from_expected="indianred")) +
            geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
            scale_x_continuous(name = "Week in 2020 through 2021") +
            scale_y_continuous(name = "Weekly Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="darkred"))    
        print(p)
        dev.off()
      }else{
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month in 2020 through 2021", "Week in 2020 through 2021")
      # Spline Regression
      if (input$EDcheck_spline & !input$EDcheck_avg) {
        subtitle <- paste0("excess deaths in ", bquote(2020), " compared to negative binomial regression on 2015-19")
        lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
        upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>%
          ggplot() +
          #geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          #geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected")) +
          geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                   fill = "indianred", alpha= 0.1) +
          geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), 
                        linetype = 1, colour = "indianred") +
          scale_colour_manual(name="",
                              values=c(excess_from_expected="indianred")) +
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # historical average
      if (input$EDcheck_avg & !input$EDcheck_spline) {
        subtitle <- paste0("excess deaths in ", bquote(2020), " compared to historical average on 2015-19")
        lower <- c_data[c_data$SERIES == "Historical average","LOWER_LIMIT"] - c_data[c_data$SERIES == "Historical average","NO_DEATHS"]
        upper <- c_data[c_data$SERIES == "Historical average","UPPER_LIMIT"] - c_data[c_data$SERIES == "Historical average","NO_DEATHS"]
        p <- c_data[c_data$SERIES == "Historical average",] %>%
          ggplot() +
          #geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          #geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_average")) +
          geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_average"),
                   fill = "cyan2", alpha= 0.1) +
          geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), 
                        linetype = 1, colour = "cyan2") +
          scale_colour_manual(name="",
                              values=c(excess_from_average="cyan2")) +
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # Both neg binom and hist avg
      if (input$EDcheck_avg & input$EDcheck_spline) {
        subtitle <- paste0("excess deaths in 2020 compared to negative binomial regression and historical average on 2015-19")
        p <- c_data %>%
          ggplot() +
          #geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES)) +
          geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES),
                   fill = c(rep("indianred", nrow(c_data)/2 ),
                            rep("cyan2", nrow(c_data)/2) ), 
                   position = "dodge", alpha = 0.1) +
          scale_colour_manual(name= "", values=c("indianred", "cyan2"),
                              labels = c("excess from expected", "excess from average")) + 
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      #neither box checked, just show actual
      if (!input$EDcheck_avg & !input$EDcheck_spline){
        subtitle = paste0("recorded deaths in 2020")
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>% 
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "All Cause Deaths") +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
        print(p)
        dev.off()
      }
     }
    )

    ## Data Descriptives (Plots) ------------------------------------------------------

    # Output Expressions -------------------------------------------------------

    # Every piece of content that gets displayed in the app has to be
    # rendered by the appropriate `render*` function, e.g. `renderPrint` for text
    # and `renderPlot` for plots. Most of the render functions here call
    # reactive objects that were created above. I have divided the output objects
    # into sections depending on what tab of the app they are called from.


    ## Data Upload -------------------------------------------------------------



    output$datadesc <- renderUI({
      country <- input$samplecountry
      text <- div()
      if (country == "Australia") {
        text <- div(
          p("This is the data from Australia."),br(),
           p("It is weekly data from January 1, 2015 through week 30 of 2020. Each row corresponds to an All Cause Mortality count for the",
             "year in 'YEAR' and the week number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."),
           p("The All Cause Mortality counts are disaggregated by 'SEX' ('Female', 'Male' and 'Total' (i.e., both combined)).",
             "Similarly, they are also disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'Australia' as this",
             "data is not disaggregated by sub-national regions (e.g., states).")
        )
      }
      if (country == "Japan") {
        text <- div(
          p("This is the data from Japan."),br(),
          p("It is monthly data from January 2015 through June 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the month number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."),
          p("The All Cause Mortality counts are disaggregated by 'SEX' ('Female', 'Male' and 'Total' (i.e., both combined)).",
            "Similarly, they are also disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'Japan' as this",
            "data is not disaggregated by sub-national regions.")
        )
      }
      if (country == "Republic of Korea") {
        text <- div(
          p("This is the data from Republic of Korea."),br(),
           p("It is monthly data from January 2015 through September 2020. Each row corresponds to an All Cause Mortality count for the",
             "year in 'YEAR' and the month number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."),
           p("The All Cause Mortality counts are not disaggregated by 'SEX' (i.e., 'Total' is all sexes combined).",
             "Similarly, they are not disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'Republic of Korea' as this",
             "data is not disaggregated by sub-national regions.")
        )
      }
      if (country == "New Zealand") {
        text <- div(
          p("This is the data from New Zealand."),br(),
          p("It is weekly data from the beginning of January 2015 through week 46 of 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the week number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."),
          p("The All Cause Mortality counts are disaggregated by 'SEX' ('Female', 'Male' and 'Total' (i.e., both combined)).",
            "Similarly, they are also disaggregated by 'AGE_GROUP'. The 'REGION' variable is just set to 'New Zealand' as this",
            "data is not disaggregated by sub-national regions.")
        )
      }
      if (country == "Philippines") {
        text <- div(
          p("This is the data from Philippines."),br(),
          p("It is monthly data from January 2015 through August 2020. Each row corresponds to an All Cause Mortality count for the",
            "year in 'YEAR' and the month number in that year given by 'PERIOD'. The column variables are",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'."),
          p("The All Cause Mortality counts are not disaggregated by 'SEX' (i.e., 'Total' is all sexes combined).",
            "Similarly, they are not disaggregated by 'AGE_GROUP'.")
        )
      }
      
      text
    })

    # output$iristbl = DT::renderDT(
    #      iris, options = list(lengthChange = FALSE)
    #      )
    # output$iris_table = DT::renderDataTable({
    #      datatable(iris(),  extensions = 'Responsive')
    #      })
    output$ACM_table <- shiny::renderDataTable({
      #ACMinit()
      acmtable <- ACMinit()
      acmtable$ISO3 <- NULL
      acmtable
    })
    output$spline_table <- shiny::renderDataTable({
      #output_spline()
      acmtable <- output_spline()
      acmtable$ISO3 <- NULL
      names(acmtable)[names(acmtable) == "WM_IDENTIFIER"] <- "WEEK/MONTH"
      names(acmtable)[names(acmtable) == "AREA"] <- "REGION/AREA"
      names(acmtable)[names(acmtable) == "NO_DEATHS"] <- "DEATHS"
      names(acmtable)[names(acmtable) == "EXPECTED"] <- "EXPECTED_DEATHS"
      names(acmtable)[names(acmtable) == "LOWER_LIMIT"] <- "95%_CI_LOWER"
      names(acmtable)[names(acmtable) == "UPPER_LIMIT"] <- "95%_CI_UPPER"
      acmtable
    })
    output$iris_table <- shiny::renderDataTable(
      {
        iris
      },
      options = list(pageLength = 10)
    )

    output$rawdatafile <- renderPrint({
      raw <- matrix(nrow = 2, ncol = 1)
      rownames(raw) <- c("name:", "size:")
      if (!is.null(input$rawdatafile)) {
        raw[1, 1] <- input$rawdatafile[1, 1]
        raw[2, 1] <- paste(input$rawdatafile[1, 2], " bytes")
      }
      write.table(raw, quote = FALSE, col.names = FALSE)
    })

    # data summary panel under data tab
    output$ACMsum <- renderPrint({
      if (is.null(ACMinit())) {
        return(cat("Please load the data using the pull-down menu on the left to\n select an Excel file and then 'Choose a region'."))
      }
      #ACM_var <- ACMinit()
      acmtable <- ACMinit()
      acmtable$ISO3 <- NULL
      names(acmtable)[names(acmtable) == "WM_IDENTIFIER"] <- "WEEK/MONTH"
      names(acmtable)[names(acmtable) == "AREA"] <- "REGION/AREA"
#     names(acmtable)[names(acmtable) == "NO_DEATHS"] <- "DEATHS_IN_2020"
      names(acmtable)[names(acmtable) == "WM_IDENTIFIER"] <- "WEEK/MONTH"
      names(acmtable)[names(acmtable) == "EXPECTED"] <- "EXPECTED_DEATHS"
      if (class(acmtable) != "data.frame") {
        return(str(acmtable))
      }
      return(str(acmtable))
    })

    ## Network Descriptives ------------------------------------------------------


    output$ACMplot <- renderPlot({
      ACM_var <- output_spline() 
#     c_data <- ACM_var %>% filter(ACM_var$COUNTRY == Countryname() & ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age)
      validate(
        need(
           (input$age %in% output_age()),
          "Please enter an Age Group exactly as it appears from the pull-down list."
        )
      )
      c_data <- ACM_var[ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age,]
      if(nrow(c_data) < 2) {
        if (ACM_var$WM_IDENTIFIER[1] == "Month") {
          p <- ACM_var[1:12,] %>%
            ggplot() +
            geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
            geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            geom_line(aes(x = PERIOD, y = EXPECTED, colour = "expected")) +
            scale_colour_manual(name="",
                                values=c(recorded="black", expected="indianred")) +
            scale_x_continuous(name = "Month in 2020 through 2021",
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:12) +
            scale_y_continuous(name = "Monthly Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        } else {
          p <- ACM_var[1:52,] %>%
            ggplot() +
            geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
            geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            geom_line(aes(x = PERIOD, y = EXPECTED, colour = "expected")) +
            scale_colour_manual(name="",
                                values=c(recorded="black", expected="indianred")) +
            scale_x_continuous(name = "Week in 2020 through 2021") +
            scale_y_continuous(name = "Weekly Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="darkred"))    
        return(p)
      }
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month in 2020 through 2021", "Week in 2020 through 2021")
      # Spline Regression
      if (input$check_spline & !input$check_avg) {
        subtitle <- paste0("deaths in ", bquote(2020), " compared to negative binomial regression on 2015-19")
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>%
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
          geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          geom_line(aes(x = PERIOD, y = EXPECTED, colour = "expected")) +
          scale_colour_manual(name="",
                              values=c(recorded="black", expected="indianred")) +
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() +
            if(name_PERIOD == "Month in 2020 through 2021"){
              scale_x_continuous(name = name_PERIOD, 
                                 labels = c("JAN", "FEB", "MAR", "APR",
                                            "MAY", "JUN", "JUL", "AUG",
                                            "SEP", "OCT", "NOV", "DEC"),
                                 breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
            } else {
              scale_x_continuous(name = name_PERIOD)
            }
      }
      
      # historical average
      if (input$check_avg & !input$check_spline) {
        subtitle <- paste0("deaths in ", bquote(2020), " compared to historical average on 2015-19")
        p <- c_data[c_data$SERIES == "Historical average",] %>%
          ggplot() +
          geom_line(aes(x = PERIOD, y = EXPECTED, colour = "average")) +
          geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
          scale_colour_manual(name="",
                              values=c(recorded="black", average="cyan2")) +
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() +
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Historical average",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # Both neg binom and hist avg
      if (input$check_avg & input$check_spline) {
        subtitle <- paste0("deaths in 2020 compared to negative binomial regression and historical average on 2015-19")
        p <- c_data %>%
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS, colour = "recorded")) +
          geom_line(aes(x = PERIOD, y = EXPECTED, group = SERIES, colour = SERIES)) +
          scale_colour_manual(name="", labels = c("expected", "average", "recorded"),
            values=c("indianred", "cyan2", recorded="black")) + 
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 16
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Historical average",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      #neither box checked, just show actual
      if (!input$check_avg & !input$check_spline){
        subtitle = paste0("deaths in 2020")
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>% 
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
          scale_x_continuous(name = name_PERIOD) +
          scale_y_continuous(name = "Deaths") + #, limits = c(0, NA)) +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() +
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Historical average",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      p
    })

    output$EDplot <- renderPlot({
      ACM_var <- output_spline() 
#     c_data <- ACM_var %>% filter(ACM_var$COUNTRY == Countryname() & ACM_var$SEX == input$gender & ACM_var$AGE_GROUP == input$age)
      validate(
        need(
          (input$EDage %in% output_age()),
          "Please enter an Age Group exactly as it appears from the pull-down list."
        )
      )
      c_data <- ACM_var[ACM_var$SEX == input$EDgender & ACM_var$AGE_GROUP == input$EDage,]
      if(nrow(c_data) < 2) {
        if (ACM_var$WM_IDENTIFIER[1] == "Month") {
          #lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          #upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          lower <- ACM_var[ACM_var$SERIES == "Cyclical spline","LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          upper <- ACM_var[ACM_var$SERIES == "Cyclical spline","UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          p <- ACM_var[1:12,] %>%
            ggplot() +
            #geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            #geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            #geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                     fill = "indianred", alpha= 0.1) +
            geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower[1:12], ymax = -1*upper[1:12]), 
                          linetype = 1, colour = "indianred") +
            scale_colour_manual(name="",
                                values=c(excess_from_expected="indianred")) +
            geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
            scale_x_continuous(name = "Month in 2020 through 2021",
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:12) +
            scale_y_continuous(name = "Monthly Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        } else {
          #lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          #upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
          lower <- ACM_var[ACM_var$SERIES == "Cyclical spline","LOWER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          upper <- ACM_var[ACM_var$SERIES == "Cyclical spline","UPPER_LIMIT"] - ACM_var[ACM_var$SERIES == "Cyclical spline","NO_DEATHS"]
          p <- ACM_var[1:52,] %>%
            ggplot() +
            #geom_line(aes(x = PERIOD, y = EXPECTED), colour = "indianred") +
            #geom_ribbon(aes(x = PERIOD, y = EXPECTED, ymin = LOWER_LIMIT, ymax = UPPER_LIMIT), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
            #geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
            geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                     fill = "indianred", alpha= 0.1) +
            geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower[1:52], ymax = -1*upper[1:52]), 
                          linetype = 1, colour = "indianred") +
            scale_colour_manual(name="",
                                values=c(excess_from_expected="indianred")) +
            geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
            scale_x_continuous(name = "Week in 2020 through 2021") +
            scale_y_continuous(name = "Weekly Excess Deaths") +
            labs(
              title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
              subtitle = "There are no data on this Sex and Age Group. This is a plot of the first group in the data."
            ) +
            theme_bw()
        }
        p <- p + theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="darkred"))    
        return(p)
      }
      name_PERIOD <- ifelse(ACM_var$WM_IDENTIFIER[1] == "Month", "Month in 2020 through 2021", "Week in 2020 through 2021")
      # Spline Regression
      if (input$EDcheck_spline & !input$EDcheck_avg) {
        subtitle <- paste0("excess deaths in ", bquote(2020), " compared to negative binomial regression on 2015-19")
        lower <- c_data[c_data$SERIES == "Cyclical spline","LOWER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
        upper <- c_data[c_data$SERIES == "Cyclical spline","UPPER_LIMIT"] - c_data[c_data$SERIES == "Cyclical spline","NO_DEATHS"]
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>%
          ggplot() +
          #geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "indianred", colour = "indianred") +
          #geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected")) +
          geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_expected"),
                   fill = "indianred", alpha= 0.1) +
          geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), 
                        linetype = 1, colour = "indianred") +
          scale_colour_manual(name="",
                              values=c(excess_from_expected="indianred")) +
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # historical average
      if (input$EDcheck_avg & !input$EDcheck_spline) {
        subtitle <- paste0("excess deaths in ", bquote(2020), " compared to historical average on 2015-19")
        lower <- c_data[c_data$SERIES == "Historical average","LOWER_LIMIT"] - c_data[c_data$SERIES == "Historical average","NO_DEATHS"]
        upper <- c_data[c_data$SERIES == "Historical average","UPPER_LIMIT"] - c_data[c_data$SERIES == "Historical average","NO_DEATHS"]
        p <- c_data[c_data$SERIES == "Historical average",] %>%
          ggplot() +
          #geom_ribbon(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), linetype = 2, alpha = 0.1, fill = "cyan2", colour = "cyan2") +
          #geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_average")) +
          geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, colour = "excess_from_average"),
                   fill = "cyan2", alpha= 0.1) +
          geom_errorbar(aes(x = PERIOD, y = EXCESS_DEATHS, ymin = -1*lower, ymax = -1*upper), 
                        linetype = 1, colour = "cyan2") +
          scale_colour_manual(name="",
                              values=c(excess_from_average="cyan2")) +
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      # Both neg binom and hist avg
      if (input$EDcheck_avg & input$EDcheck_spline) {
        subtitle <- paste0("excess deaths in 2020 compared to negative binomial regression and historical average on 2015-19")
        p <- c_data %>%
          ggplot() +
          #geom_line(aes(x = PERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES)) +
          geom_col(aes(x = PERIOD, y = EXCESS_DEATHS, group = SERIES, colour = SERIES),
                       fill = c(rep("indianred", nrow(c_data)/2 ),
                                rep("cyan2", nrow(c_data)/2) ), 
                   position = "dodge", alpha = 0.1) +
          scale_colour_manual(name= "", values=c("indianred", "cyan2"),
                              labels = c("excess from expected", "excess from average")) + 
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "Excess Deaths") +
          labs(
            title = paste0("Excess Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      
      #neither box checked, just show actual
      if (!input$EDcheck_avg & !input$EDcheck_spline){
        subtitle = paste0("recorded deaths in 2020")
        p <- c_data[c_data$SERIES == "Cyclical spline",] %>% 
          ggplot() +
          geom_line(aes(x = PERIOD, y = NO_DEATHS), colour = "black") +
          geom_hline(aes(yintercept=0), linetype="dashed", color="black") +
          scale_y_continuous(name = "All Cause Deaths") +
          labs(
            title = paste0("All Cause Mortality in ", Countryname(), " during the Pandemic"),
            subtitle = subtitle, size = 12
          ) +
          theme_bw() + 
          if(name_PERIOD == "Month in 2020 through 2021"){
            scale_x_continuous(name = name_PERIOD, 
                               labels = c("JAN", "FEB", "MAR", "APR",
                                          "MAY", "JUN", "JUL", "AUG",
                                          "SEP", "OCT", "NOV", "DEC"),
                               breaks = 1:nrow(c_data[c_data$SERIES == "Cyclical spline",]))
          } else {
            scale_x_continuous(name = name_PERIOD)
          }
      }
      p
    })
  }
)
