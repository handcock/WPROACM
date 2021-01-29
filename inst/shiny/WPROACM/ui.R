
# Everything that gets displayed inside the app is enclosed in a call to `shinyUI`.
# The first thing to be specified is the type of page to display. The `navbarPage`
# includes a navigation bar at the top of the page and each tab leads to different
# pages of content.

shinyUI(
  navbarPage(
    #theme="mycosmo.css",
    title=NULL,
    id= 'navbar', windowTitle = 'WPROACM', collapsible=TRUE,



# Front Page (About) ------------------------------------------------------


tabPanel(title=span('WPROACM', id="sWtitle"),
         value='tab1',
         fluidRow(
          column(2,
                 actionButton("aboutButton", label = "About WPROACM",
                              class = "btn active"),
    #            actionButton("citeButton", label = "Citing WPROACM",
    #                         class = "btn"),
                 actionButton('startButton', label='Get Started',
                              class="btn btn-primary"),
                 fluidRow(column(1,
                  a(img(src = 'WHO-WPRO_Logo_PMS_2925.png', width = 175),
                    href = 'https://www.who.int/westernpacific/', target = '_blank') ))
          ),
   column(6, style="padding: 0 30px 0 0;",
          div(id="aboutbox",
            p("Welcome to the",strong("WPRO online-calculator for excess deaths in countries"),"!"),

            p("This calculator has been developed by the", a('World Health Organization, Western Pacific Region',
                href='https://www.who.int/westernpacific/',
               target='_blank'), "in conjunction with the", a('Department of Statistics at UCLA',
                href='http://statistics.ucla.edu/', target='_blank'),"."),

            p("This tool aims to estimate the",em("expected all-cause mortality"),"counts for each week or month starting at",
              "January 1, 2020 onward in the counter-factual situation where there had not been a pandemic.",
              "Monitoring the all-cause mortality trends is an important component of multisource surveillance for COVID-19.",
              "The",em("excess mortality"),"is defined to be the difference between the reported counts and expected counts for that week or month.",
              "in the Western Pacific region"),

            p("This interface is useful as part of the dialogue between the WHO WPRO and countries about the",
              "impact of the COVID-19 pandemic on all-cause mortality (ACM) in individual Member countries and territories",
              "in the Western Pacific region"),

            p("Tracking all-cause mortality trends is an important component of multisource surveillance for COVID-19.",
              "Excess deaths have been observed in several countries during the COVID19 pandemic.",
              "Evidence is needed to support timely and dynamic decision-making and policy development."),

            p("This tool will allow easy tracking and analysis of ACM and excess deaths.",
              "It is for use by member countries and does not require the data to be seen by the WHO WPRO.",
              "In fact, all analysis is done on the computer where it is run. An internet connection is only required",
              "to install the software (already done if you are reading this message!)."),

            p("A typical analysis will move sequentially through the tabs at the top of the page",
              "(starting with", actionLink("startButton", "Get Started"),".",
              "Click on the help icon at the top of any page for guidance."),
            p("Bug Reports/comments/suggestions/requests? Please share them with us.",
              "They are best submitted through our", a('GitHub site,',
                                                       href='https://github.com/handcock/WPROACM',
                                                       target='_blank'),
              "or by email to us (see", actionLink("helpLink", "Help"), "tab)."),
            p("This web application",
              "is written with the Shiny framework and development is via GitHub.  More information",
              "on Shiny and our GitHub repository can be found in the",
              "resource links on the right.")
          ),
          div(id="citebox",
            tabsetPanel(
              tabPanel("BibTeX",
p(strong("WPROACM")),
tags$pre(id='scitation','@Manual{handcock:WPROACM,
  title = {WPROACM: Software tools for the Statistical Analysis of Excess Mortality from All Cause Mortality Data
  author = {Mark S. Handcock},
  year = {2021},
  address = {Los Angeles, CA},
  url = {http://hpmrg.org/}
}'),

p(strong("WPROACM")),
tags$pre(id='swcitation',"@Manual{beylerian:WPROACM,
  title = {\\pkg{WPROACM}: A Graphical User Interface for Analysing Excess Mortality from All Cause Mortality Data},
  author = {Mark S. Handcock},
  year = {2021},
  note = {\\proglang{R}~package version~0.1},
  address = {Los Angeles, CA},
  url = {https://cran.r-project.org/web/packages/WPROACM/}
}")
                       ),
              tabPanel("Other",
p(strong("WPROACM")),
tags$pre("Mark S. Handcock (2021). WPROACM: A Graphical User Interface for Analysing Excess Mortality from All Cause Mortality Data. URL http://hpmrg.org"),

p(strong("WPROACM")),
tags$pre("Mark S. Handcock (2021).
WPROACM: A Graphical User Interface for Analysing Excess Mortality from All Cause Mortality Data.")
                       )
            ),

            p('If you use WPROACM, please cite it'),
            )
          ),
   column(4,
          wellPanel(
              h5(tags$u('Resources')),
              div(a("WPROACM on GitHub", href="https://github.com/handcock/WPROACM",
                    target="_blank")),
              div(a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                    target="_blank"))
   ),
   fluidRow(a(img(src = 'UCLADepartmentofStatisticsSmall.png', width = 400),
             href = 'http://statistics.ucla.edu/', target = '_blank'),
             style="margin-left:15px;")
   )
   )
 ),


# Data Upload -------------------------------------------------------------


# Before the code for what is displayed on the Data Upload page,
# various javaScript and CSS files that will be useful later in the
# script are linked. For example, since network plotting and model
# fitting do not happen instantly (especially for large networks),
# a loading icon will help to assure users that the app is still working
# on producing output. The file busy.js controls the behavior of the
# loading message and style.css controls the appearance. To display
# the loading message on subsequent tabs, we only need to include the
# div statement within those tabs.

tabPanel(title='Data', value='tab2',
         #busy.js is for calculation in progress boxes
         #alert.js is for popup boxes,
         #jquery libraries are loaded from google cdn, needed for autocomplete
         #this tagList command has to go inside a tabPanel
         tagList(
           tags$head(
             tags$link(rel="stylesheet", type="text/css",href="style.css"),
             #tags$link(rel="stylesheet", type="text/css",href="autocomplete.css"),
             tags$link(rel="stylesheet", type="text/css",
                       href="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/themes/smoothness/jquery-ui.css"),
             tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"),
             #tags$script(type="text/javascript", src="autocomplete.js"),
             tags$script(type="text/javascript", src="//ajax.googleapis.com/ajax/libs/jqueryui/1.11.1/jquery-ui.min.js"),
             tags$script(type="text/javascript", src="busy.js"),
             tags$script(type="text/javascript", src="alert.js"),
             tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                              function(message) {
                              console.log(message)
                              eval(message.code);
                              });'))
           )
         ),

# Conditional panels are only displayed when a specified condition is true.
# The condition is a javascript expression that can refer to the current
# values of input or output objects. When the condition is false, the panel
# does not take up any space in the UI.


fluidRow(
  column(7,
    tabsetPanel(id='datatabs',
      tabPanel('Upload All Cause Mortality data', br(),
         wellPanel(
           fluidRow(
column(4,
  tabPanel('All cause mortality in 2020', br(),
          div(id="viewdata",
            p("This tool is designed to estimate the weekly or monthly excess deaths in countries in the Western Pacific Region",
              "during the COVID-19 pandemic, using all-cause of mortality data."),
            p("The", strong("5-years historical average"),"and", strong("expected deaths forecasted by negative-binomial regression"),
              ", and their 95% confidence interval (95% CI) are calculated from the deaths observed in 2015-2019.")
             )
          )
  ),
             column(8,
                    selectInput('filetype',label='File type',
                                 choices=c(
                                           'Excel spreadsheet of All Cause Mortality data (*.xls or *.xlsx)' = 1,
                                           'built-in example All Cause Mortality data'= 2
                                          )
                    ),
             conditionalPanel(condition = 'input.filetype < 2',
               column(6,
                    br(),
                    fileInput(inputId='rawdatafile', label=NULL, accept='text'),
                    verbatimTextOutput('rawdatafile'))
                ),
             conditionalPanel(condition = 'input.filetype == 2',
                column(12,
                    br(style="line-height:26px;"),
                    selectizeInput('samplecountry', label=NULL,
                                choices=c("Choose a country" = '', 
                                          'Australia','Japan',
                                          'Republic of Korea', 'New Zealand', 'Philippines')),
                              p(class="helper", id="BIhelp", icon("question-circle"),
                                span("These are example Excel files from some countries with data from January 1, 2015 through August, 2020.", style="font-size:0.85em;"),
                                 br(),'You try select a country in the drop-down menu', em("Choose a country"),
                                 'above this message and view the data in the above', em("View Data"),' tab.',
                                 'You can then try the calculator out on this data to see an analysis similar to that for your own country.')
                )
               ),
             conditionalPanel(condition = 'input.filetype == 1',
                column(12,
                    br(style="line-height:26px;"),
                    uiOutput('selectsheet'),
                    p(class="helper", id="Excelhelp", icon("question-circle"),
                      span("What format does the Excel file need to be in?", style="font-size:0.85em;"),
                       br(),'Upload a *.xls or *.xlsx file of all-cause mortality data.',
                       'The file should be saved from the WHO standardized Excel template.',
                       'Select the template you want from this dropdown list:'),
                    selectizeInput('template_country', label=NULL,
                                choices=c("Choose a country" = '', 
                 'Australia', 'Philippines', 'French Polynesia', 'Generic Monthly', 'Generic Weekly')),
                 uiOutput('download_t')
                )
             )
           #   downloadButton("download_template", label = "Download the WHO standardized Excel template", class = "btn-sm")
           )),
         conditionalPanel(
           condition="input.filetype == 1 & input.samplecountry != ''",
           wellPanel(uiOutput("datadesc"))
           )
         )),
    tabPanel('View Data', br(),
          div(id="viewdata",
            p("Below is displayed the all cause of mortality data as read in. It should display column variables calle",
            "'REGION', 'AGE_GROUP', 'SEX'",
            "'YEAR', 'PERIOD', 'NO_DEATHS'",
            "and few values should be missing (denoted NA). If your files does not look like this, try to load the built-in",
            "example All Cause Mortality data under the 'Upload All Cause Mortality data' tab on the top left. If you view that",
            "it will show what to expect."),
            p("If your data does not look correct, try to correct it by using the WHO standardized Excel template under the",
               em("Data"),"tab on the top left."), 
            p("A typical analysis will move sequentially through the tabs at the top of the page",
              "Click on the help icon at the top of any page for guidance."),
            p("If you are having trouble getting the data in",
              "email us (see", actionLink("helpLink", "Help"), "tab)."),
            ),
           wellPanel(
             dataTableOutput("ACM_table")
           )
         )
# tabPanel('Modify Attributes', br(),
#          wellPanel(
#            p('In the future we will build in functions that will ',
#              'allow you to modify the attributes of your network.',
#              'This will include options like:'),
#            tags$ul(
#              tags$li('Applying a function (e.g.', code('sqrt()'), ') to an attribute'),
#              tags$li('Recoding (mapping a set of attributes onto a new set)'),
#              tags$li('Conditional transformations (', code('do...if...'),')'))
#            #uiOutput('modifyattrchooser')
#            )
#          )
  )
),

column(4,
tabsetPanel(
  tabPanel('Data Summary', br(),
           verbatimTextOutput('ACMsum')
           ))
  )
),

icon('question-circle', class='fa-2x helper-btn'),
div(class="helper-box", style="display:none",
    p('Upload a file of all-cause mortality data. The data should be a *.xls or *xlsx file',
    'saved from the WHO standardized Excel template. Click', a("here",
      href = "https://www.who.int/westernpacific/", target = "_blank"), ' to download.')),
actionLink('dataleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
actionLink('dataright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
),


# Network Descriptives ----------------------------------------------------

# There are no calls to selectInput for the options to color code or size the nodes,
# even though they appear in the app. Most widget functions are called in ui.R, but
# this means that all the options passed to them must be static. If the options depend
# on user input (the coloring and sizing menus depend on which network the user
# selects), the widget must be rendered in server.R and output in ui.R with
# iuOutput.

    tabPanel(
      title = "Plots", value = "tab3",
      # include progress box when this tab is loading
      div(
        class = "busy",
        p("Calculation in progress... This might take a few minutes."),
        img(src = "ajax-loader.gif")
      ),
      fluidRow(
        column(9,
          tabsetPanel(id = "plottabs",
            tabPanel(
              "All Cause Mortality Plot", br(),
              wellPanel(
                fluidRow(
                  selectInput("gender", "Select Sex", gender_labels),
                  #selectInput("gender", "Select Sex", sort(as.character(unique(ACM_var$SEX)))),
                  #selectInput("age", "Select Age Group", output_age),
                 #div(id="agebox", strong("Select Age Group")),
                  textInput("age", "Type in the Age Group (choosing from the list below)", age_group_labels),
                 #selectInput("age", "Select Age Group", age_group_labels),
                  uiOutput('age'),
                  #selectInput("age", "Select Age Group", sort(as.character(unique(ACM_var$AGE_GROUP)))),
                  checkboxInput('check_avg', '5-year Average'),
                  checkboxInput('check_spline', "Negative Binomial Regression"),
                  plotOutput('ACMplot'),
                  downloadButton("ACMplotdownload", label = "Download plot as image", class = "btn-sm")
                ) ),
            ),
            tabPanel(
              "Excess Mortality Plot", br(),
              wellPanel(
                fluidRow(
                  div(id="barplotbox",
                      "The excess mortality is displayed here using a bar graph.",
                      "The height of each bar represents the excess death for that month/week.",
                      "The lines above and below the end of each bar indicate what the height ",
                      "of the bar would be using the 95% confidence interval upper bound and lower bound, respectively."),
                  selectInput("EDgender", "Select Sex", gender_labels),
               #  selectInput("EDage", "Select Age Group", age_group_labels),
                  textInput("EDage", "Type in the Age Group (choosing from the list below)", age_group_labels),
                  uiOutput('EDage'),
                  checkboxInput('EDcheck_avg', '5-year Average'),
                  checkboxInput('EDcheck_spline', "Negative Binomial Regression"),
                  plotOutput('EDplot'),
                  downloadButton("EDplotdownload", label = "Download Excess mortality plot as image", class = "btn-sm")
                ) ),
            )
          )
        ) ),
        div(id = "plottabhelp", class = "helper-btn", icon("question-circle", "fa-2x")),
        div(
          class = "helper-box", style = "display:none",
          p(
            "Use the plots to gain insight to the expected and excess deaths.",
            "Click the boxes on the left to get different models for the expected deaths and download a PDF of any of the plots."
          )
        ),
        actionLink("plotleft", icon = icon("arrow-left", class = "fa-2x"), label = NULL),
        actionLink("plotright", icon = icon("arrow-right", class = "fa-2x"), label = NULL)
      ),
# Expected Deaths --------------------------------------------------------------------

tabPanel(title='Expected Deaths',  value='tab4',

         #include progress bar when this tab is loading
         div(class = "busy",
             p("Calculation in progress... This might take a few minutes."),
             img(src="ajax-loader.gif")
         ),

  fluidRow(
    dataTableOutput("spline_table"),
##   DT::dataTableOutput("iris_table")

column(2,
    downloadButton('EDdownload', label = "Download data, including expected and excess deaths", class = "btn-sm"))

) ),

# Methods --------------------------------------------------------------------

tabPanel(title='Methods', value='tab5',
   column(6, style="padding: 0 30px 0 0;",
          div(id="methodsbox",
            p("This page has a description of the statistical methods used in the calculator to compute the expected and excess deaths in countries"),
            
            p(strong("All-cause mortality"),
              "is defined as the total number of recorded deaths across all causes."),
            
            p(strong("Excess death"),
              "is defined as the difference between the number of all-cause deaths during 2020 and the expected number of deaths."),
            
            p(strong("Expected death"), "is defined as the expected number of deaths in 2020 if no pandemic had occured.",
              "The expected number of deaths is calculated in two different ways, using either a",
              strong("negative binomial regression"), "or the", strong("historical five year average"),
              ", both of which are based on the years 2015-2019."),

            p(strong("Negative-binomial regression")),

            p("This particular negative-binomial regression model is a generalized additive model (GAM) in that it uses smoothing functions",
              "for the predictor variables. Since the date and period are input as discrete values, they are smoothed using cubic splines,",
              "a common smoothing technique."),

            p(strong("Historical 5-year average")),

            p("The 5-years historical average is based on, and the 95% confidence interval (95% CI) are calculated",
              "from, the deaths observed in 2015-2019."),

            p("Below is a detailed description of the methods in statistical language. It is in PDF format and can be saved for separate
study."),
          ),
         tags$iframe(style="height:600px; width:100%; scrolling=yes", 
                   src="ACMnotes_201230.pdf")
         )
         ),

tabPanel(title='Help and Resources', value='tab6',
         sidebarLayout(position = 'right',
                       sidebarPanel(
                         h5(tags$u('Resources')),
                         div(title = "Wiki page for WPROACM",
                             a("About WPROACM",
                               href = "https://github.com/handcock/WPROACM/wiki",
                               target = "_blank")),
                         div(title="WPRO dashboard",
                             a("WPRO dashboard",
                               href = "https://who.maps.arcgis.com/apps/opsdashboard/index.html#/345dfdc82b5c4f6a815f1d54a05d18e", 
                               target = "_blank")
                         ),
                         div(title=paste("Information on the methodology used",
                                         "in the tool"),
                             a("About the methodology used in the tool.",
                               href = "https://github.com/handcock/WPROACM/wiki/Methodology-used-in-WPROACM/", target = "_blank")
                         ),
                         br(),
                         div(a("WPROACM on GitHub", href="https://github.com/handcock/WPROACM",
                               target="_blank")),
                         div(a("Shiny: a web application framework for R", href="http://shiny.rstudio.com/",
                               target="_blank"))
                       ),
                       mainPanel(
                         h5(tags$u('Help with WPROACM')),
                         p("This app is maintained on GitHub. To request new features or report a bug,",
                           "please interact with the",
                           a("repository", href='https://github.com/handcock/WPROACM',
                             target="_blank"),
                           "or email us at", 
                             a(actionButton(inputId = "email1", label = "handcock@stat.ucla.edu", 
                               icon = icon("envelope", lib = "font-awesome")),
                               href="mailto:handcock@stat.ucla.edu"),
                           a(actionButton(inputId = "email2", label = "wproncovinfoplan@who.int", 
                                          icon = icon("envelope", lib = "font-awesome")),
                             href="mailto:wproncovinfoplan@who.int"))
                         ))
         )


  )
)
