library(ggplot2)
library(readxl)
library(shinyWidgets)
library(highcharter)
library(janitor)
library(shinycssloaders)
library(shinydashboard)

SDG_Indicator_ <- read_excel("data/SDG_Indicator.xlsx")
sdgs_metadata1 <- read_excel("data/sdgs_metadata1.xlsx")
sdg_2019_index <- read_excel("data/2019GlobalIndexResults_modified.xlsx", sheet = "Overview") %>% clean_names(., "small_camel")
data <- sdgs_metadata1

goallist <- c(unique(as.character(data$Goal)))

goallogo <- c(
  "image/new sdgs/E-WEB-Goal-01.png",
  "image/new sdgs/E-WEB-Goal-02.png",
  "image/new sdgs/E-WEB-Goal-03.png",
  "image/new sdgs/E-WEB-Goal-04.png",
  "image/new sdgs/E-WEB-Goal-05.png",
  "image/new sdgs/E-WEB-Goal-06.png",
  "image/new sdgs/E-WEB-Goal-07.png",
  "image/new sdgs/E-WEB-Goal-08.png",
  "image/new sdgs/E-WEB-Goal-09.png",
  "image/new sdgs/E-WEB-Goal-10.png",
  "image/new sdgs/E-WEB-Goal-11.png",
  "image/new sdgs/E-WEB-Goal-12.png",
  "image/new sdgs/E-WEB-Goal-13.png",
  "image/new sdgs/E-WEB-Goal-14.png",
  "image/new sdgs/E-WEB-Goal-15.png",
  "image/new sdgs/E-WEB-Goal-16.png",
  "image/new sdgs/E-WEB-Goal-17.png"
)

navbarPage(
  theme = "style.css",
  tags$img(src = "image/sdg_logo.png", height = "40px"),
  tabPanel(
    "Overview",
    # SDG overview Map
    withSpinner(highchartOutput("overviewmap", height = "600px")),
    tags$br(),

    # list of action button
    tags$div(
      class = "logo-div", style = "text-align:center; margin-bottom:5px;",
      tags$h3("Sustainable Development Goals"),
      tags$p("Select one of the 17 SDGs to see it on the map"),
      tags$button(
        id = "goal1",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-01.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal2",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-02.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal3",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-03.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal4",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-04.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal5",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-05.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal6",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-06.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal7",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-07.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal8",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-08.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal9",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-09.png",
          height = "120px"
        )
      )
    ),
    tags$div(
      class = "logo-div", style = "text-align:center; margin-bottom:5px;",
      tags$button(
        id = "goal10",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-10.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal11",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-11.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal12",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-12.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal13",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-13.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal14",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-14.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal15",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-15.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal16",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-16.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal17",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/E-WEB-Goal-17.png",
          height = "120px"
        )
      ),
      tags$button(
        id = "goal18",
        class = "btn action-button",
        style = "padding:0px; border: none;",
        tags$img(
          src = "image/SDG Icons 2019_WEB/SDG Wheel_WEB.png",
          height = "120px"
        )
      ),
    ),
    tags$br(),
    # List of SDG 2019 index
    tags$div(
      tags$h3("The SDG Index", style = "text-align: center;")
    ),
    tags$div(
      style = "padding:10px; border: none; ",
      withSpinner(DT::dataTableOutput("sdg2019ConTable"))
    )
    ),
  tabPanel(
    "SDGs",
    sidebarLayout(
      sidebarPanel(
        useShinydashboard(),
        pickerInput("sdggoal",
          "GOAL",
          multiple = FALSE,
          choices = goallist,
          choicesOpt = list(
            content =
              mapply(goallist, goallogo, FUN = function(country, flagUrl) {
                HTML(paste(
                  "&nbsp;", tags$img(src = flagUrl, width = 40, height = 35), "&nbsp;", "&nbsp;", country, "&nbsp;"
                ))
              }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
          )
        ),
        imageOutput("sdg_image", height = "300px"),
        textOutput("sdg_define")
      ),
      mainPanel(
        tags$div(tags$h3("SDGs Targets"), DT::dataTableOutput("sdgtable2"))
      )
    )
  ),
  
  navbarMenu("Report",
             tabPanel("No Poverty",
                      fluidRow(valueBoxOutput("goal1_box1",width = 6),
                               valueBoxOutput("goal1_box2",width = 6)),
                      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                        highchartOutput("goal1_chart"),
                        highchartOutput("goal1_chart_trend"))),
                      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput("goal1_chart_poverty_rate_country"),
                                           highchartOutput("goal1_chart_poverty_pop_country"))),
                        highchartOutput("goal1_chart_poverty_region")
                      ),
             tabPanel("Zero Hunger",
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal2_chart"),
                                            highchartOutput("goal2_chart_trend"))),
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                           highchartOutput("goal2_chart_nourish"),
                                           highchartOutput("goal2_chart_stunt"))),
                      fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal2_chart_wast"),
                                           highchartOutput("goal2_chart_obese"))
                              )
                       ),
             tabPanel("Good health"
                      # fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                      #                      highchartOutput("goal2_chart"),
                      #                      highchartOutput("goal2_chart_trend"))),
                      # fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                      #                      highchartOutput("goal2_chart_nourish"),
                      #                      highchartOutput("goal2_chart_stunt"))),
                      # fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                      #                      highchartOutput("goal2_chart_wast"),
                      #                      highchartOutput("goal2_chart_obese")))
                      ),
             tabPanel("Quality education",
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal4_chart"),
                                            highchartOutput("goal4_chart_trend"))),
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal4_chart_primary"),
                                            highchartOutput("goal4_chart_litracy"))),
                      # fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                      #                      highchartOutput("goal2_chart_wast"),
                      #                      highchartOutput("goal2_chart_obese")))
                      ),
              tabPanel("Gender equality",
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal5_chart"),
                                            highchartOutput("goal5_chart_trend"))),
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal5_female_school_max"),
                                            highchartOutput("goal5_female_school_min"))),
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal5_female_labour_max"),
                                            highchartOutput("goal5_female_labour_min"))),
                       fluidRow(splitLayout(cellWidths = c("50%", "50%"),
                                            highchartOutput("goal5_female_parli_max"),
                                            highchartOutput("goal5_female_parli_min")))
                       )
             # tabPanel("Nepal",
             #          fluidRow(valueBoxOutput("Nepal_box_SDG_rank",width = 3),
             #                    valueBoxOutput("Nepal_box_score",width = 3),
             #                    valueBoxOutput("Nepal_box_pop",width=3),
             #                   valueBoxOutput("Nepal_box_gdp",width = 3)),
             #           fluidRow(highchartOutput("Nepal_polar_column")),
             #           fluidRow(highchartOutput("Nepalfig1"),
             #          #          highchartOutput("Nepalfig2")),
             #          # fluidRow(splitLayout(cellWidths = c("50%", "50%"),
             #          #                      highchartOutput("goal2_chart_wast"),
             #          #                      highchartOutput("goal2_chart_obese"))),
             #          # fluidRow(splitLayout(cellWidths = c("50%", "50%"),
             #          #                      highchartOutput("goal2_chart_wast"),
             #          #                      highchartOutput("goal2_chart_obese")))
             #          )
             # )
)
)
