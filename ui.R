
library(shiny)

country <- c("Ethiopia", "Tanzania")

country <- as.data.frame(country, stringsAsFactors = FALSE)


country1 <- c("Ethiopia", "Tanzania")

country1 <- as.data.frame(country, stringsAsFactors = FALSE)

header <-
  shinydashboard::dashboardHeader(
    title = tags$ul(
      "LivWell Visualization Platform",
      type = "text",
      style = "font-size:35px;margin-top:30px;
      margin-bottom: 10px"
    ),
    
    titleWidth = 600,
    shinydashboard::dropdownMenu(
      type = "messages",
      shinydashboard::messageItem(
        from = "Email us at",
        message = "flare.network@umich.edu",
        icon = icon("question")
      )
    ),
    tags$li(
      a(
        href = 'http://www.forestlivelihoods.org/',
        img(
          src = 'http://www.forestlivelihoods.org/wp-content/uploads/2016/05/flarecolor_modified_7.png',
          title = "FLARE Home page",
          height = "95px"
        ),
        style = "padding-top:12px; padding-bottom:12px;background-color:white;"
      ),
      class = "dropdown"
    ),
    
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 105px}"),
      tags$style(".main-header .logo {height: 120px;}"),
      tags$style(".sidebar-toggle {height: 90px; padding-top: 2px !important;}")
      #tags$style(".navbar {min-height:90px !important}")
    )
    )



sidebar <- shinydashboard::dashboardSidebar(disable = TRUE)

## Tab1 - Body Univariate - START

body <- shinydashboard::dashboardBody(
  tags$head(tags$style(
    HTML(
      '
      /* logo */
      .skin-blue .main-header .logo {
      background-color: #27AE60;
      }
      /* logo when hovered */
      .skin-blue .main-header .logo:hover {
      background-color: #27AE60;
      }
      .navbar-default .navbar-nav>.active>a,
      .navbar-default .navbar-nav>.active>a:focus,
      .navbar-default .navbar-nav>.active>a:hover {
      color: #fff;background-color: #27AE60;
      }
      /* navbar (rest of the header) */
      .skin-blue .main-header .navbar {
      background-color: #27AE60;
      }
      /* toggle button when hovered  */
      .skin-blue .main-header .navbar .sidebar-toggle:hover{
      background-color: #FF6347;
      }
      '
    )
    )),
  
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tags$footer(
    title = "FLARE",
    align = "right",
    style = "
    position:absolute;
    bottom:0;
    width:100%;
    height:50px; /* Height of the footer */
    color: white;
    padding: 10px;
    z-index: 1000;"
  ),
  
  fluidPage(
    tags$style(
      type = 'text/css',
      '.navbar { background-color: #fff;
      font-family: Arial;
      font-size: 16px;
      color: #6ee584;
      }',
      '.container-fluid {
      height: auto;
      width: auto;
      margin: 0px;
      /*padding: 10px;*/
      padding: 0px;
      }',
      '.navbar-default .navbar-brand:focus {
      color: #FFFF00;
      }',
      '.navbar-dropdown { background-color: #262626;
      font-family: Arial;
      font-size: 12px;
      color: #FF6347;
      }'
    ),
    
    navbarPage(
      "",
      id = "selector",
      tabPanel(
        "Overview",
        mainPanel(
          h3("FLARE's Livelihoods and Well-being tool (LivWell)"),
          p("FLARE's Livelihoods and Well-being tool (LivWell) provides a low-cost, convenient method for estimating the household livelihood and well-being impacts of projects, policies, and other forest-related interventions"), 
          p("This tool consists of a survey instrument, a data visualization and analyses platform, and user manuals for organizations and enumerators. The LivWell Survey Instrument generates data on household demographics, income and expenditure, assets, credit and savings, household shocks and impacts, forest information, health and nutrition, and forest governance. This survey incorporates questions that are foundational to assessing well-being and are comparable with data collected from the following nationally representative surveys: Demographic Health Surveys (DHS),
            Living Standards Measurement Surveys (LSMS), and Multiple Indicator Cluster Surveys (MICS)"),
          br(),
          p("For more details , visit ",
            a("FLARE !", 
              href = "http://www.forestlivelihoods.org/resources/")),
          p("For an introduction , visit the ",
            a("Data collection platform", 
              href = "https://enketo.ona.io/x/#Ylge")),
          
          h3("Contact us"),
          p(strong("FLARE Network Secretariat,"),
            "School for Environment and Sustainability,
            University of Michigan,
            440 Church Street,
            Ann Arbor, Michigan 48109,
            Email:flare.network@umich.edu")
          
          )
          ),
      navbarMenu(
        "Category",
        tabPanel(
          "Demographic",
          fluidRow(column(width = 12,
                          fluidRow(
                            column(width = 9,
                                   
                                   wellPanel(splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plotdemo")
                                   ))),
                            column(
                              width = 3,
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                selectInput(
                                  "country",
                                  "COUNTRY",
                                  c(country$country),
                                  selected = NULL,
                                  multiple = FALSE,
                                  selectize = FALSE,
                                  width = NULL,
                                  size = NULL
                                )
                              ),
                              
                              # wellPanel(width = NULL, status = "success",
                              #
                              #           uiOutput("choose_outcomes")
                              # ),
                              wellPanel(
                                width = NULL,
                                status = "success",
                                
                                uiOutput("demo_predictors")
                              )
                              
                              # wellPanel(
                              #   width = NULL,
                              #   status = "success",
                              #   actionButton("demobutton", "Download plot", icon = icon("download-alt", lib = "glyphicon"))
                              #   
                              # )
                            )
                          ))),
          
          HTML(
            '<footer class="text-center"
            style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;">
            <p> Copyright &copy FLARE Network
            <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,
            Email: flare.network@umich.edu
            </p>
            </footer>'
          )
          
          
          ),
        tabPanel(
          "Income",
          fluidRow(column(width = 12,
                          fluidRow(
                            column(width = 9,
                                   
                                   wellPanel(splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plotincome")
                                   ))),
                            column(
                              width = 3,
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                selectInput(
                                  "country1",
                                  "COUNTRY",
                                  c(country$country),
                                  selected = NULL,
                                  multiple = FALSE,
                                  selectize = FALSE,
                                  width = NULL,
                                  size = NULL
                                )
                              ),
                              
                              # wellPanel(width = NULL, status = "success",
                              #
                              #           uiOutput("choose_outcomes")
                              # ),
                              wellPanel(
                                width = NULL,
                                status = "success",
                                
                                uiOutput("income_predictors")
                              )
                              
                              # wellPanel(
                              #   width = NULL,
                              #   status = "success",
                              #   actionButton("incomebutton", "All variables", icon = icon("cog", lib = "glyphicon"))
                              #   
                              # )
                            )
                          ))),
          
          HTML(
            '<footer class="text-center"
            style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
            <p> Copyright &copy FLARE Network
            <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
            </p>
            </footer>'
          )
          
          ),
        # tabPanel(
        #   "Consumption",
        #   
        # 
        #   
        #   
        #   HTML(
        #     '<footer class="text-center"
        #     style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
        #     <p> Copyright &copy FLARE Network
        #     <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
        #     </p>
        #     </footer>'
        #   )
        #   
        #   ),
        tabPanel(
          "Education",
          
          fluidRow(column(width = 12,
                          fluidRow(
                            column(width = 9,
                                   
                                   wellPanel(splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plotedu")
                                   ))),
                            column(
                              width = 3,
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                selectInput(
                                  "country3",
                                  "COUNTRY",
                                  c(country$country),
                                  selected = "Tanzania",
                                  multiple = FALSE,
                                  selectize = FALSE,
                                  width = NULL,
                                  size = NULL
                                )
                              ),
                              
                              # wellPanel(width = NULL, status = "success",
                              #
                              #           uiOutput("choose_outcomes")
                              # ),
                              wellPanel(
                                width = NULL,
                                status = "success",
                                
                                uiOutput("edu_predictors")
                              )
                              
                              # wellPanel(
                              #   width = NULL,
                              #   status = "success",
                              #   actionButton("edubutton", "All variables", icon = icon("cog", lib = "glyphicon"))
                              #   
                              # )
                            )
                          ))),
          HTML(
            '<footer class="text-center"
            style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
            <p> Copyright &copy FLARE Network
            <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
            </p>
            </footer>'
          )
          ),
        tabPanel(
          "Health",
          fluidRow(column(width = 12,
                          fluidRow(
                            column(width = 9,
                                   
                                   wellPanel(splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plothealth")
                                     
                                   ))),
                            column(
                              width = 3,
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                selectInput(
                                  "country4",
                                  "COUNTRY",
                                  c(country$country),
                                  selected = "Tanzania",
                                  multiple = FALSE,
                                  selectize = FALSE,
                                  width = NULL,
                                  size = NULL
                                )
                              ),
                              
                              # wellPanel(width = NULL, status = "success",
                              #
                              #           uiOutput("choose_outcomes")
                              # ),
                              wellPanel(
                                width = NULL,
                                status = "success",
                                
                                uiOutput("health_predictors")
                              )
                              
                              # wellPanel(
                              #   width = NULL,
                              #   status = "success",
                              #   actionButton("healthbutton", "All variables", icon = icon("cog", lib = "glyphicon"))
                              #   
                              # )
                            )
                          ))),
          HTML(
            '<footer class="text-center"
            style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
            <p> Copyright &copy FLARE Network
            <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
            </p>
            </footer>'
          )
          ),
        tabPanel(
          "Assets",
          
          fluidRow(column(width = 12,
                          fluidRow(
                            column(width = 9,
                                   
                                   wellPanel(splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plotasset")
                                   ))),
                            column(
                              width = 3,
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                selectInput(
                                  "country5",
                                  "COUNTRY",
                                  c(country$country),
                                  selected = FALSE,
                                  multiple = FALSE,
                                  selectize = FALSE,
                                  width = NULL,
                                  size = NULL
                                )
                              ),
                              
                              # wellPanel(width = NULL, status = "success",
                              #
                              #           uiOutput("choose_outcomes")
                              # ),
                              wellPanel(
                                width = NULL,
                                status = "success",
                                
                                uiOutput("asset_predictors")
                              )
                              
                              
                            )
                          ))),
          
          
          
          
          
          
          
          
          
          HTML(
            '<footer class="text-center"
            style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
            <p> Copyright &copy FLARE Network
            <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
            </p>
            </footer>'
          )
          ),
        tabPanel(
          "Subjective Well-Being",
          fluidRow(column(width = 12,
                          fluidRow(
                            column(width = 9,
                                   
                                   wellPanel(splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plotwellbeing")
                                   ))),
                            column(
                              width = 3,
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                selectInput(
                                  "country2",
                                  "COUNTRY",
                                  c(country$country),
                                  selected = "Tanzania",
                                  multiple = FALSE,
                                  selectize = FALSE,
                                  width = NULL,
                                  size = NULL
                                )
                              ),
                              
                              wellPanel(
                                width = NULL,
                                status = "success",
                                
                                uiOutput("wellbeing_predictors")
                              )
                              
                              # wellPanel(
                              #   width = NULL,
                              #   status = "success",
                              #   actionButton("wellbeingbutton", "All variables", icon = icon("cog", lib = "glyphicon"))
                              #   
                              # )
                            )
                          ))),
          
          HTML(
            '<footer class="text-center"
            style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
            <p> Copyright &copy FLARE Network
            <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
            </p>
            </footer>'
          )
          )
          ),
      
      
      tabPanel(
        "Cross tabulation",
        
        
        fluidRow(column(width = 12,
                        fluidRow(
                          column(width = 9,
                                 wellPanel(
                                   splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plotbi1", height = 350)
                                     #plotOutput("plotbi2", height = 350)
                                     
                                   )
                                 )
                          ),
                          
                          column(
                            width = 3,
                            wellPanel(
                              width = NULL,
                              status = "success",
                              selectInput(
                                "countrycr",
                                "COUNTRY",
                                c( country$country),
                                selected = NULL,
                                multiple = FALSE,
                                selectize = FALSE,
                                width = NULL,
                                size = NULL
                              )
                              
                            ),
                            wellPanel(
                              width = NULL,
                              status = "success",
                              uiOutput("crtb_predictors"),
                              uiOutput("crtb_predictors1")
                            )
                            # wellPanel(
                            #   width = NULL,
                            #   status = "success",
                            #   actionButton("goButton3", "Download", icon = icon("cog", lib = "glyphicon"))
                            #   
                            # )
                          )
                        ))),
        HTML(
          '<footer class="text-center"
          style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
          <p> Copyright &copy FLARE Network
          <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
          </p>
          </footer>'
        )
        
        
        ),
      tabPanel(
        "Outcome analysis",
        
        fluidRow(column(width = 12,
                        fluidRow(
                          column(width = 9,
                                 wellPanel(
                                   splitLayout(
                                     cellWidths = c("100%"),
                                     plotOutput("plot_test1",height = 250)
                                     #plotOutput("plot_test2", height = 350)
                                     
                                   )
                                 ),
                                 wellPanel(
                                   splitLayout(
                                     cellWidths = c("50%","50%"),
                                     plotOutput("plot_test2",height = 250),
                                     plotOutput("plot_test3",height = 250)
                                     #plotOutput("plot_test2", height = 350)
                                     
                                   )
                                 )
                          ),
                          
                          column(
                            width = 3,
                            wellPanel(
                              width = NULL,
                              status = "success",
                              selectInput(
                                "countryttst",
                                "COUNTRY",
                                c( country$country),
                                selected = NULL,
                                multiple = FALSE,
                                selectize = FALSE,
                                width = NULL,
                                size = NULL
                              )
                              
                            ),
                            wellPanel(
                              width = NULL,
                              status = "success",
                              uiOutput("ttest_predictors")
                              #uiOutput("ttest_predictors1")
                            )
                            # wellPanel(
                            #   width = NULL,
                            #   status = "success",
                            #   actionButton("goButton3", "Download", icon = icon("cog", lib = "glyphicon"))
                            #   
                            # )
                          )
                        ))),
        
        
        
        HTML(
          '<footer class="text-center"
          style = "padding-top:12px; padding-bottom:12px;font-size: 16px;color:white;background-color:#27AE60;z-index: 1000;height = 50;">
          <p> Copyright &copy FLARE Network
          <br>FLARE Network Secretariat,School for Environment and Sustainability,University of Michigan,440 Church Street,Ann Arbor, Michigan 48109,Email: flare.network@umich.edu
          </p>
          </footer>'
        )
        )
      
        )
    
      )
  
      )

shinyUI(
  shinydashboard::dashboardPage(title = "LivWell Visualization Platform", header, sidebar, body)
)