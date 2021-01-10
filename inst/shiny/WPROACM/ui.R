
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
                  a(img(src = 'WHOWPRO.png', width = 175),
                    href = 'https://www.who.int/westernpacific/', target = '_blank') ))
          ),
   column(6, style="padding: 0 30px 0 0;",
          div(id="aboutbox",
            p("Welcome to the all cause of mortality and Excess Death Monitoring interactive interface!"),

            p("This interface has been developed by the", a('World Health Organization, Western Pacific Region',
                href='https://www.who.int/westernpacific/',
               target='_blank'), "in conjunction with the", a('Department of Statistics at UCLA',
                href='http://statistics.ucla.edu/', target='_blank'),"."),

            p("This web application",
              "is written with the Shiny framework and development is via GitHub.  More information",
              "on Shiny and our GitHub repository can be found in the",
              "resource links on the right."),

            p("This interface is useful as part of the dialogue bwtween the WHO WPRO and countries about the",
              "impact of the COVID-19 pandemic on all-cause mortality (ACM) in individual Member countries and territories",
              "in the Western Pacific region"),

            p("Tracking all-cause mortality trends is an important component of multisource surveillance for COVID-19.",
              "Excess deaths have been observed in several countries during the COVID19 pandemic.",
              "Evidence is needed to support timely and dynamic decision-making and policy development."),

            p("The purpose of this tool is to allow easy tracking and analysis of ACM and excess deaths.",
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
              "or by email to us (see", actionLink("helpLink", "Help"), "tab).")
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
             column(6,
                    selectInput('filetype',label='File type',
                                 choices=c(
                                           'CSV spreadsheet of All Cause Mortality data (*.csv)' = 1,
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
                column(6,
                    br(style="line-height:26px;"),
                    selectizeInput('samplenet', label=NULL,
                                choices=c("Choose a country" = '', 
                                          'Australia','Japan',
                                          'South Korea', 'New Zealand', 'Philippines'))
                )
               )
             ),
             conditionalPanel(condition='input.filetype == 1',
                              p(class="helper", id="CSVhelp", icon("question-circle"), span("What format does the CSV file need to be in?", style="font-size:0.85em;")),
                              div(class="mischelperbox", id="CSVbox", 'When working in R, an object in your environment',
                                  'can be saved to a .rds file from the command line in the following way:',
                                  code('saveRDS(objectname, file="newfilename.rds")'),br(),'By default the file will be saved',
                                  'into the current working directory. The full path to a new location can be',
                                  'specified in the ', code('file='), 'argument, or set', code('file=file.choose(new=TRUE)'),
                                  'to use a save dialog box.')
                              )
           )),
         conditionalPanel(
           condition="input.filetype == 1 & input.samplenet != ''",
           wellPanel(uiOutput("datadesc"))
           )
         ),
    tabPanel('View Data', br(),
           wellPanel(
 #h3('Grocery List'),
 #uiOutput('Grocery_List')
#shiny::dataTableOutput("ACM_table")
dataTableOutput("ACM_table")
#DT::dataTableOutput('iris_table')
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
           verbatimTextOutput('nwsum')
           ))
  )
),

icon('question-circle', class='fa-2x helper-btn'),
div(class="helper-box", style="display:none",
    p('Upload a file of observed network data (must be of a supported type).',
    'Add custom attributes or symmetrize on the "View Data" tab.')),
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

tabPanel(title='Plots', value='tab3',
 #include progress box when this tab is loading
 div(class = "busy",
     p("Calculation in progress..."),
     img(src="ajax-loader.gif")
 ),

fluidRow(
 column(2,
    tabsetPanel(id='plottabs',
      tabPanel('All Cause Mortality Plot',
               p(class='helper', id='ACMplothelper', icon('question-circle')),
               div(class='mischelperbox', id='ACMplothelperbox',
                   "Degrees are a node level measure for the number of edges incident on each node.",
                   "The degree distribution shows how the edges in a network are distributed among the",
                   "nodes. The amount (or proportion) of nodes with low, medium",
                   "or high degrees contribute to the overall structure of the",
                   "network. The degree distributions of directed graphs can",
                   "be subset by in-degree or out-degree."),
                plotOutput('ACMplot', click = "plot_click",
                           dblclick = dblclickOpts(id = "plot_dblclick"),
                           hover = hoverOpts(id = "plot_hover", delay = 100,
                                             delayType = "throttle"),
                           brush = brushOpts(id = "plot_brush")
                           )
        )
#     tabPanel('Excess Cause Mortality Plot',
#              p(class='helper', id='EMplothelper', icon('question-circle')),
#              div(class='mischelperbox', id='EMplothelperbox',
#                  "Degrees are a node level measure for the number of edges incident on each node.",
#                  "The degree distribution shows how the edges in a network are distributed among the",
#                  "nodes. The amount (or proportion) of nodes with low, medium",
#                  "or high degrees contribute to the overall structure of the",
#                  "network. The degree distributions of directed graphs can",
#                  "be subset by in-degree or out-degree."),
#               plotOutput('ACMplot', click = "plot_click",
#                          dblclick = dblclickOpts(id = "plot_dblclick"),
#                          hover = hoverOpts(id = "plot_hover", delay = 100,
#                                            delayType = "throttle"),
#                          brush = brushOpts(id = "plot_brush")
#                          )
#       )
      ),br(),br()
),
 column(4,
     tabsetPanel(id='displaytabs',
       tabPanel(title='Display Options', br(),
          wellPanel(
                conditionalPanel(condition='input.plottabs == "All Cause Mortality Plot"',
                   selectInput('activeplot', label = NULL,
                               choices = c("Static Plot", "Interactive Plot")),
                   checkboxInput('iso',
                                 label = 'Display isolates',
                                 value = TRUE),
                   checkboxInput('vnames',
                                 label = 'Display vertex names',
                                 value = FALSE),
                   br(),
                   sliderInput('transp',
                               label = 'Vertex opacity',
                               min = 0, max = 1, value = 1),
                   br(),
                   uiOutput("dynamiccolor"),
                   conditionalPanel(condition="Number(output.attrlevels) > 9",
                     column(10,
                            p(id = "closewarning1", icon(name = "remove"), class = "warning"),
                            div(class = "warning", id = "colorwarning1",
                                span(tags$u("Note:"),
                                     "Color palette becomes a gradient for attributes with more than nine levels.")
                            )
                     )),
                   #span(bsAlert(inputId = 'colorwarning'), style='font-size: 0.82em;'),
                   uiOutput('dynamicsize'),
                   br(),
                   actionButton("refreshplot", icon = icon("refresh"),
                                label = "Refresh Plot", class = "btn-sm"),
                   downloadButton('nwplotdownload',
                                  label = "Download Plot", class = "btn-sm")),
                conditionalPanel(condition='input.plottabs == "Attributes"',
                                 selectInput("attrview", label = "View attributes in:",
                                             choices = c("Large table",
                                                         "Small tables",
                                                         "Plot summaries")),
                                 br(),
                                 uiOutput("attrcheck")
                ),
                conditionalPanel(condition='input.plottabs == "Degree Distribution"',
                   uiOutput("dynamiccmode_dd"),
                   uiOutput("dynamiccolor_dd"),
                   tags$label("Y-axis units:"), br(),
                   actionButton("countButton_dd", label="Count of vertices", class="btn-sm active"),
                   actionButton("percButton_dd", label="Percent of vertices", class="btn-sm"),
                   br(), br(),
                   tags$label('Expected values of null models:'), br(),
                   fluidRow(
                     column(10,
                            checkboxInput('uniformoverlay_dd',
                                   label='Conditional uniform graphs (CUG)',
                                   value=FALSE)
                            ),

                      span(icon('question-circle'), id="cughelper_dd", class="helper",
                           div(id="cughelperbox_dd", class="mischelperbox",
                               "Draws from the distribution of simple random graphs with the same",
                               "fixed density as the observed network. The mean and 95% confidence",
                               "intervals for each degree are plotted."))),
                   fluidRow(
                     column(10,
                            checkboxInput('bernoullioverlay_dd',
                                   label='Bernoulli random graphs (BRG)',
                                   value=FALSE)
                            ),
                     span(icon('question-circle'), id="brghelper_dd", class="helper",
                          div(id="brghelperbox_dd", class="mischelperbox",
                              "Draws from the distribution of simple random graphs with the same",
                              "stochastic tie probability as the observed network.",
                              "The mean and 95% confidence intervals for each degree are plotted."))),
                   br(),
                   downloadButton('degreedistdownload', label = "Download Plot", class="btn-sm")
                  ),
                conditionalPanel(condition='input.plottabs == "Geodesic Distribution"',
                                 tags$label("Y-axis units:"), br(),
                                 actionButton("countButton_gd", "Count of vertex pairs", class="btn-sm active"),
                                 actionButton("percButton_gd", "Percent of vertex pairs", class="btn-sm"),
                                 br(), br(),
                                 tags$label('Expected values of null models:'), br(),
                                 fluidRow(
                                   column(10,
                                     checkboxInput('uniformoverlay_gd',
                                                 label='Conditional uniform graphs (CUG)',
                                                 value=FALSE)
                                          ),
                                   span(icon('question-circle'), id="cughelper_gd", class="helper",
                                        div(id="cughelperbox_gd", class="mischelperbox",
                                            "Draws from the distribution of simple random graphs with the same",
                                            "fixed density as the observed network. The mean and 95% confidence",
                                            "intervals for each degree are plotted."))),
                                 fluidRow(
                                   column(10,
                                     checkboxInput('bernoullioverlay_gd',
                                               label='Bernoulli random graphs (BRG)',
                                               value=FALSE)
                                          ),
                                   span(icon('question-circle'), id="brghelper_gd", class="helper",
                                        div(id="brghelperbox_gd", class="mischelperbox",
                                            "Draws from the distribution of simple random graphs with the same",
                                            "stochastic tie probability as the observed network.",
                                            "The mean and 95% confidence intervals for each degree are plotted."))),
                                 br(),
                                 verbatimTextOutput('infsummary'),
                                 fluidRow(
                                   column(10,
                                          checkboxInput('excludeInfs',
                                                        label=span('Exclude "inf"s from plot'),
                                                        value=FALSE)),
                                   span(icon('question-circle'), id="infhelper_gd", class="helper",
                                        div(id="infhelperbox_gd", class="mischelperbox",
                                            "A pair of nodes without any path connecting",
                                            'it has a geodesic distance of "inf".'))),
                                 br(),
                                 downloadButton('geodistdownload', label= 'Download Plot', class="btn-sm")
                  ),
                conditionalPanel(condition='input.plottabs == "More"',
                                 p("No display options at this time,",
                                   "stay tuned for updates!")
                                 )
                )),
       tabPanel(title='Data Summary', br(),
        verbatimTextOutput('attr2'))
        )
     )
 ),
div(id='plottabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
 div(class="helper-box", style="display:none",
     p('Use the network plots to gain insight to the observed network.',
       'Edit the display options in the panel on the right and download a PDF of any of the plots.')),
actionLink('plotleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
actionLink('plotright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
),



# Fit Model ---------------------------------------------------------------

# The output objects for the current dataset and current formula have default values
# specified in server.R to prevent errors from NULL values and so that there are
# helpful messages for the user before they begin entering data.

      tabPanel(title='Fit Model',value='tab4',

          #include progress bar when this tab is loading
           div(class = "busy",
               p("Calculation in progress..."),
               img(src="ajax-loader.gif")
           ),

          fluidRow(
            column(2,
               p('Network:', class="nwlabel"),
               verbatimTextOutput('currentdataset1')
              ),

            column(4,
                   p("ERGM terms:"),
                   div(textInput(inputId="terms", label=NULL, value="edges"),
                       title=paste("Type in term(s) and their arguments.",
                                   "For multiple terms, separate with '+'. ")
                   ),
                   actionButton('addtermButton', 'Add Term(s)', class="btn-primary btn-sm"),
                   actionButton('resetformulaButton', 'Reset Formula', class="btn-sm")


            ),
            column(5,
               tabsetPanel(
                 tabPanel("Term Documentation",
                  br(),
                  div(class="placeholder",
                      fluidRow(
                        column(12,
                               a("Commonly used ergm terms",
                                 href = "https://github.com/statnet/statnetWeb/blob/master/inst/html/d2-sX-ergmterms.html",
                                 target = "_blank"), br(),
                               a("Term cross-reference tables",
                                 href = "http://cran.r-project.org/web/packages/ergm/vignettes/ergm-term-crossRef.html",
                                 target = "_blank"), br(), br()
                               ),
                        column(6,
                               actionButton("matchingButton", "Compatible terms",
                                            class="btn-sm active"),
                               actionButton("allButton", "All terms",
                                            class="btn-sm")
                               ),
                        column(4, uiOutput("listofterms"))
                      ),
                      fluidRow(
                        column(12,
                               div(id="termdocbox",
                                    uiOutput("termdoc")
                                ),
                                div(id = "termexpand",
                                    icon(name = "angle-double-up"))
                               )

                      )

                  )
                 ),
                 tabPanel("Control Options",
                    div(class = "placeholder",
                    fluidRow(class = "shiftright",
                      column(3, style = "padding-left: 0;",
                        selectInput('controltype',label = NULL,
                                          choices = c("MCMC"))),
                      column(5,
                        checkboxInput('controldefault', 'Use default options', value = TRUE))
                    ),
                        conditionalPanel(condition = "input.controltype == 'MCMC'",
                                         class = "shiftright gray", id = "mcmcopt1",
                          fluidRow(
                            column(4,
                                   span("Interval:"),
                                   customNumericInput('MCMCinterval', label = NULL, value = 1024,
                                                      class = "mcmcopt input-mini round"),
                                   title = paste("Number of proposals between sampled statistics.")
                                   ),

                            column(4,
                                   span("Burn-in:"),
                                   customNumericInput('MCMCburnin', label = NULL, value = 16384,
                                                      class = "mcmcopt input-mini round"),
                                   title = paste("Number of proposals before any MCMC sampling is done.",
                                                 "Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.")
                                   ),

                            column(4,
                                   span("Sample size:"),
                                   customNumericInput('MCMCsamplesize', label = NULL, value = 1024,
                                                      class = "mcmcopt input-mini round"),
                                   title = paste("Number of network statistics, randomly drawn from a given distribution",
                                                 "on the set of all networks, returned by the Metropolis-Hastings algorithm.")
                                   )
                          ),

                          fluidRow(
                              div(span("Other controls:", class = "shiftright"),
                                  customTextInput("customMCMCcontrol", label = NULL, value = "",
                                                  class = "input-small round"),
                                  title = paste("Other arguments to be passed to",
                                       "control.ergm, e.g. MCMC.burnin.retries = 1")
                                  )
                            )),
                        conditionalPanel(condition = "input.controltype == 'MCMLE'",
                                         p("Coming soon"))
                        )))
                     )
            ),
          br(),
         fluidRow(
           column(2,
                  p('Current ergm formula:')),
           column(10,
                  verbatimTextOutput('checkterms_fit'))),
        fluidRow(
           column(2,
                  p('Summary statistics:')),
           column(10,
                  verbatimTextOutput('prefitsum'))),
         fluidRow(column(12,
                         actionButton("fitButton", "Fit Model", class="btn-primary btn-sm"),
                         uiOutput("savemodel"),
                         actionButton("clearmodelButton", label="Clear All Models", class="btn-sm")
         )),
         br(),
         tabsetPanel(id = 'fittingTabs',
           tabPanel('Current Model Summary', br(),
                    verbatimTextOutput('modelfitsum'),
                    downloadButton("modelfitdownload", "Download Summary (.txt)", class="btn-sm")),
           tabPanel('Current Model Fit Report', br(),
                    verbatimTextOutput('modelfit')),
           tabPanel('Model Comparison', br(),
                    verbatimTextOutput('modelcomparison'),
                    downloadButton("modelcompdownload", "Download Comparison (.txt)", class="btn-sm"))
          ), br(),br(),
  div(id='fittabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
    div(class="helper-box", style="display:none",
      p('Create an ergm formula by typing terms into the text box.',
        'Notice the summary statistics populate for each term added to the formula. ',
        'After fitting the model, the "Fitting" tab will show MCMC iterations (if any) and MLE coefficients,',
        'while the "Summary" tab shows a comprehensive summary of the model fit.',br(),
        'Find more help in our online training materials in the', a('ergm tutorial.',
                        href='https://github.com/statnet/Workshops/wiki',
                        target="_blank"))),
actionLink('fitleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
actionLink('fitright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
          ),

# MCMC Diagnostics --------------------------------------------------------


tabPanel(title='MCMC Diagnostics', value='tab5',
         #include progress bar when this tab is loading
         div(class = "busy",
             p("Calculation in progress..."),
             img(src="ajax-loader.gif")
         ),

         fluidRow(
           column(2,
                  p('Network:', class="nwlabel"),
                  verbatimTextOutput('currentdataset_mcmc')),
           column(10,
                  div(p('ergm formula:',style="display:inline;"),
                    uiOutput('uichoosemodel_mcmc'), class="nwlabel"),
                  verbatimTextOutput('checkterms_mcmc'))
         ),
         br(),
         tags$hr(),
         tabsetPanel(id='mcmctabs',
           tabPanel('Plot', br(),
                    wellPanel(
                      class = "mcmcwarning",
                      p("Recent changes in the ergm estimation algorithm mean that these plots",
                        "can no longer be used to ensure that the mean statistics from the model match the",
                        "observed network statistics. For that functionality, please use the GOF page.")
                      #p(icon("close"), class = "topright")
                       ),
                    #intercept error and give friendly message when MCMC doesn't run
                    conditionalPanel(condition="output.diagnostics == 'MCMC was not run or MCMC sample was not stored.'",
                                column(1,span(class='helper', id='mcmchelper', icon('question-circle')),
                                       style='width:20px; margin-left:0px'),
                                column(11,pre('MCMC was not run or MCMC sample was not stored.'),
                                       style='margin-left:0px;'),
                                column(3,
                                       div(class='mischelperbox', id='mcmchelpbox',
                                        "MCMC is only run when at least one of the terms in the model represents",
                                        "dyad dependence (e.g., degree terms, or triad related terms).  For",
                                        "models with only dyadic independent terms, estimation relies on",
                                        "traditional maximum likelihood algorithms used for generalized linear",
                                        "models."))
                                     ),
                    uiOutput('diagnosticsplotspace'),
                    downloadButton('mcmcplotdownload',label = 'Download Plots', class="btn-sm")),
           tabPanel('Summary', br(),
                    wellPanel(
                      class = "mcmcwarning",
                      p("Recent changes in the ergm estimation algorithm mean that these plots",
                        "can no longer be used to ensure that the mean statistics from the model match the",
                        "observed network statistics. For that functionality, please use the GOF page.")
                      #p(icon("close"), class = "topright")
                    ),
                    verbatimTextOutput('diagnostics'))
         ),
         div(id='mcmctabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
         div(class="helper-box", style="display:none",
             p('Check for model degeneracy. When a model converges properly',
               'the MCMC sample statistics should vary randomly around the',
               'observed values at each step, and the difference between the',
               'observed and simulated values of the sample statistics should',
               'have a roughly bell shaped distribution, centered at 0.')),
         actionLink('mcmcleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
         actionLink('mcmcright', icon=icon('arrow-right', class='fa-2x'), label=NULL)

),

# Goodness of Fit ---------------------------------------------------------


tabPanel(title='Goodness of Fit',value='tab6',

         #include progress bar when this tab is loading
         div(class = "busy",
             p("Calculation in progress..."),
             img(src="ajax-loader.gif")
         ),

         fluidRow(
           column(2,
                  p('Network:', class="nwlabel"),
                  verbatimTextOutput('currentdataset_gof')),
           column(10,
                  div(p('ergm formula:',style="display:inline;"),
                  uiOutput('uichoosemodel_gof'), class="nwlabel"),
                  verbatimTextOutput('checkterms_gof'))
          ),
         p('GOF is based on 100 simulated networks from your fitted model.  If you do not specify a term the default formula for undirected
           networks is ', code('~ degree + espartners + distance + model'), 'and for
           directed networks is ', code('~ idegree + odegree + espartners +
                                        distance + model'), '.  ',
           'The "model" plot shows how well the fitted model reproduces the observed values for the 
           terms in the model (the sufficient statistics), and can be used to assess convergence.'),
         fluidRow(
            column(2,
                   p("Goodness of fit term:"),
                   selectInput('gofterm', label = NULL,
                               c('Default', 'degree','idegree','odegree',
                                 'distance', 'espartners','dspartners', 'triadcensus',
                                 'model')
                                 )),
            column(1, actionButton('gofButton', 'Run', class="shiftdown"))
            ),
         br(),
     tabsetPanel(
       tabPanel("Current Model", br(),
                fluidRow(
                  column(5,
                         verbatimTextOutput('gofsummary')),
                  column(7,
                         uiOutput('gofplotspace'),
                         downloadButton('gofplotdownload', label = 'Download Plots', class="btn-sm")))
                ),
       tabPanel("Compare Saved Models",align="center", br(),
                uiOutput('gofplotcompspace'),
                fluidRow(align="left",
                         downloadButton('gofplotcompdownload',
                                        label='Download Plots', class="btn-sm"),
                         br())
                )
       ),

   div(id='goftabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
   div(class="helper-box", style="display:none",
       p('Test how well your model fits the original data by choosing a network',
          'statistic that is not in the model, and comparing the value of this',
          'statistic observed in the original network to the distribution of values',
          'you get in simulated networks from your model.')),
   actionLink('gofleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
   actionLink('gofright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
),

# Simulations -------------------------------------------------------------


tabPanel(title='Simulations', value='tab7',
         fluidRow(
           column(7,
              fluidRow(
                column(4,
                    p('Network:', verbatimTextOutput('currentdataset_sim'))),
                column(4,
                       p("Number of simulations:"),
                       numericInput('nsims', label = NULL,
                                    min = 1, value = 1)
                       ),
                column(1,
                       actionButton('simButton', 'Simulate', class = "shiftdown")
                       )
                ),
              div(p('ergm formula:',style="display:inline;"),
              uiOutput('uichoosemodel_sim'), class="nwlabel"),
              verbatimTextOutput('checkterms_sim')
              ),
           column(5,
                tabsetPanel(
                  tabPanel("Control Options",
                           fluidRow(
                             column(3, class = "shiftright",
                                    selectInput('simcontroltype',label=NULL,
                                                      choices=c("MCMC"))),
                             column(7,
                                    checkboxInput('simcontroldefault','Use default options', value=TRUE))
                           ),
                           conditionalPanel(condition="input.simcontroltype == 'MCMC'",
                                            class="shiftright gray", id = "mcmcopt2",
                             fluidRow(
                                    column(5,
                                        span("Interval:"),
                                        customNumericInput('simMCMCinterval',label=NULL, value=1024,
                                                           class="mcmcopt input-mini round"),
                                        title=paste("Number of proposals between sampled statistics.")
                                        ),
                                    column(5,
                                        span("Burn-in:"),
                                        customNumericInput('simMCMCburnin', label=NULL, value=16384,
                                                           class="mcmcopt input-mini round"),
                                        title=paste("Number of proposals before any MCMC sampling is done.",
                                                    "Defaults to 16 times the MCMC interval, unless burn-in is specified after the interval.")
                                        )

                             ),
                             fluidRow(
                                    div(
                                        span("Other controls:", class = "shiftright"),
                                        customTextInput("simcustomMCMCcontrol", label=NULL, value="",
                                                        class = "input-mini round"),
                                        title=paste("Type in other arguments to be passed to control.simulate,",
                                                    "e.g. MCMC.init.maxedges=200")
                                        )
                             )),
                           conditionalPanel(condition="input.simcontroltype == 'Parallel'",
                                           p("Coming soon"))
                           )
                  )
           )
         ),

         tags$hr(),

         fluidRow(
           column(7,
             tabsetPanel(id="simplotpanel",
             tabPanel("Network Plots", br(),
                 column(5,
                        numericInput('thissim',
                                     label = 'Choose a simulation to plot:',
                                     min = 1, value = 1)
                        ),
                 plotOutput('simplot')
                      ),
             tabPanel("Simulation Statistics",
                      conditionalPanel("output.simnum >1",
                          div(plotOutput('simstatsplot'),
                              title=paste("Statistics from each simulation,",
                                          "plotted over horizontal \n",
                                          "lines of the corresponding target statistics."))
                       )
               )
              ), br(), br()
             ),


         column(4,
                  tabsetPanel(
                    tabPanel('Display Options', br(),
                        conditionalPanel("input.simplotpanel == 'Network Plots'",
                             wellPanel(
                               checkboxInput('iso2',
                                             label = 'Display isolates?',
                                             value = TRUE),
                               checkboxInput('vnames2',
                                             label = 'Display vertex names?',
                                             value = FALSE),
                               br(),
                               sliderInput('transp2',
                                           label = 'Vertex opacity',
                                           min = 0, max = 1, value = 1),
                               br(),
                               uiOutput('dynamiccolor2'),
#                                          span(bsAlert(inputId = 'colorwarning2'), style='font-size: 0.82em;'),
                               uiOutput('dynamicsize2'),
                               downloadButton('simplotdownload',
                                              label = 'Download Plot', class="btn-sm"))
                        ),
                        conditionalPanel("input.simplotpanel == 'Simulation Statistics'",
                               conditionalPanel("output.simnum > 1",
                                         plotOutput('simstatslegend'),
                                         downloadButton('simstatsplotdownload',
                                                        label='Download Plot', class="btn-sm")
                                         ))
                      ),
                    tabPanel('Simulation Summary', br(),
                         wellPanel(
                         conditionalPanel(condition="output.simnum != 1",
                                verbatimTextOutput('simsummary'),
                                verbatimTextOutput('simcoef'),
                                verbatimTextOutput('simstatslabel'),
                                conditionalPanel("output.simnum < 10",
                                  verbatimTextOutput('simstats')),
                                conditionalPanel("output.simnum >= 10",
                                  verbatimTextOutput('simstats2'))
                                            ),
                         conditionalPanel(condition="output.simnum == 1",
                           verbatimTextOutput('simsummary2')
                           ),
                         br(),
                         fluidRow(
                           column(7,
                                downloadButton('simstatsdownload',
                                        label = 'Download Statistics', class="btn-sm")),
                           column(4,
                              div(title=paste0(".txt: Summary of simulations",
                                              " plus full list of statistics. \n",
                                              ".csv: Full list of statistics only."),
                                radioButtons('simstatsfiletype', label=NULL,
                                             choices=c('.txt','.csv'))
                                )
                              )
                           )


                           )
                        )
                    )
           )),
         div(id='simtabhelp', class='helper-btn', icon('question-circle', 'fa-2x')),
         div(class="helper-box", style="display:none",
             p('Choose how many simulations to run and click "Simulate".',
               'Plot any individual simulation, or compare',
               'simulation statistics with target statistics.',
               'Download any of the plots or a .csv file of the',
               'simulation statistics.')),
         actionLink('simleft', icon=icon('arrow-left', class='fa-2x'), label=NULL),
         actionLink('simright', icon=icon('arrow-right', class='fa-2x'), label=NULL)
         ),

# Expected Deaths --------------------------------------------------------------------

tabPanel(title='Expected Deaths',  value='tab8',

         #include progress bar when this tab is loading
         div(class = "busy",
             p("Calculation in progress... This might take a few minutes."),
             img(src="ajax-loader.gif")
         ),

    dataTableOutput("spline_table")
##   DT::dataTableOutput("iris_table")
),
# Help --------------------------------------------------------------------


tabPanel(title='Help', value='tab9',
         sidebarLayout(position = 'right',
                       sidebarPanel(
                         h5(tags$u('Resources')),
                         div(title = "Wiki page for WPROACM",
                             a("About WPROACM",
                               href = "https://github.com/handcock/WPROACM/wiki",
                               target = "_blank")),
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
                               href="mailto:handcock@stat.ucla.edu"))
                         ))
         )


  )
)
