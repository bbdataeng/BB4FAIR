## ---------------------------
##
## Script name: ui.R
##
## Purpose of script: UI script for shinyapp
##
## Author: BBDATAENG team 
##
## Date Created: 2025-01-22
##
## Copyright (c) Strengthening BBMRI.it
## Email:
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# User Interface ---------------------------------------------------------------

dashboardPage(
  
  preloader = list(html = tagList(spin_4(), ""), color = "#001f3f"),
  
  ## header
  dashboardHeader(
    skin = "navy",
    title = div(style = "display: flex; align-items: center; justify-content: center; font-size: 18px; padding: 15px", "BB4FAIR"),
    tags$li(
      class = "dropdown",
      style = "display: flex; align-items: center; width: 100%;",
      fluidRow(
        column(
          width = 2,
          #offset = 1,
          align = "center",
          img(src = 'Italy_grande_str_bbmri_bianco.png', style = "width: 80%;")
        ),
        column(
          width = 6.5,
          offset = 0.5,
          align = "center",
          span("Digital Maturity Survey for Biobanking", style = "font-size: 30px; color: white;")
        )
      )
    )
  ),
  
  ## sidebar
  sidebar = dashboardSidebar(
    status = "navy",
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Scores",
        tabName = "Scores",
        icon = icon("table")
      ),
      menuItem(
        text = "Feature Analysis",
        tabName = "Answers",
        icon = icon("chart-simple")
      ),
      menuItem(
        text = "Visualization",
        tabName = "Visualization",
        icon = icon("chart-area")
      ),
      menuItem(
        text = "Tiering",
        tabName = "Tiering",
        icon = icon("star")
      )
    )
  ),
  
  ## body
  body = dashboardBody(tabItems(
    
    ## Scores panel
    tabItem(
      'Scores',
      titlePanel(""),
      br(),
      fluidRow(
        column(4, selectInput(
          "bb_name", "Biobank ID:", c("All", unique(as.character(scores$BB_ID)))
        )),
        column(4, selectInput("tier", "Tier:", c(
          "All", unique(as.character(scores$tier))
        ))),
        column(
          4,
          selectizeInput(
            "macroarea",
            "Macro-areas:",
            choices = c("All", unique(as.character(macroareas$area))),
            multiple = TRUE
          )
        )
      ),
      DT::dataTableOutput("scores"),
      spsDepend("pop-tip"),
    ),
    
    ## Answers Plots panel
    tabItem('Answers', titlePanel(""), fluidRow(
      br(),
      bs4Card(
        title = "Question",
        HTML(
          "<p>Explore the answers to different questions by selecting from the list below.</p>"
        ),
        selectizeInput(
          inputId = "question",
          label = NULL,
          choices = questions_labels,
          multiple = FALSE
        ),
        collapsible = FALSE
      ),
      bs4Card(
        tags$style("#graph {margin-top: 50px;}"),
        plotOutput("graph", height = "60vh", width = "100%"),
        collapsible = FALSE
      )
    )),
    
    ## Visualization Panel
    tabItem('Visualization', fluidPage(
      br(),
      fluidRow(column(3.5, plotOutput("density")), column(4, plotOutput("heatmap", height = "300px"))),
      br(),
      fluidRow(
        column(width = 2.2, class = "col-sm-3", plotOutput("radar3")),
        column(width = 2.5, class = "col-sm-3", plotOutput("radar2")),
        column(width = 2.5, class = "col-sm-3", plotOutput("radar1")),
        column(width = 3, class = "col-sm-3", plotOutput("legend"))
      )
    )),
    
    ## Tiering Results panel
    tabItem(
      'Tiering',
      titlePanel("Evaluation of your Biobank"),
      br(),
      fluidRow(
        column(
          4,
          bs4Card(
            title = "Select a Biobank ID",
            selectInput(
              inputId = "bb_name_plot",
              label = NULL,
              choices = unique(as.character(tier_BB$Biobank_ID))
            ),
            width = 10,
            headerBorder = FALSE,
            collapsible = FALSE
          ),
          
          valueBoxOutput("tiering_valueBox", width = 10),
          bs4Card(
            title = "Ranking Position",
            label = NULL,
            flexdashboard::gaugeOutput("gauge", height = "50px"),
            width = 10,
            collapsible = FALSE,
            height = 150
          )
        ),
        column(
          8,
          bs4Card(
            plotOutput("eval_plot"),
            class = "plot-box",
            width = 12,
            height = 550,
            headerBorder = FALSE,
            collapsible = FALSE
          )
        )
      ),
      fluidRow(column(
        12,
        bs4Card(
          title = "Suggestions",
          " ",
          width = 12,
          headerBorder = FALSE,
          collapsed = TRUE
        )
      ))
    )
  )),
  
  controlbar = dashboardControlbar(title = "Digital Maturity Survey"),
  footer = bs4DashFooter(
    left = "bbdataeng-team",
    right = "Strengthening BBMRI.it",
    fixed = FALSE
  )
)
