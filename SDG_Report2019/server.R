# server part for sdg app

library(ggplot2)
library(readxl)
library(dplyr)
library(highcharter)
library(RColorBrewer)
library(shinycssloaders)
library(DT)
library(shinydashboard)

SDG_Indicator <- read_excel("data/SDG_Indicator.xlsx")
sdgs_metadata1 <- read_excel("data/sdgs_metadata1.xlsx")
sdg_2019_index <- read_excel("data/2019GlobalIndexResults_modified.xlsx", sheet = "Overview")
sdg_2019_index1 <- read_excel("data/2019GlobalIndexResults_modified.xlsx", sheet = "Overview") %>% clean_names(., "small_camel")
sdg_2019_index2 <- read_excel("data/2019GlobalIndexResults_modified.xlsx", sheet = "SDR2019 Data")
report_data <- sdg_2019_index2
data <- sdgs_metadata1
maps <- readRDS("map.rds")

# Map for the SDG Score
sdg_score_map <- highchart(type = "map") %>%
  hc_add_series_map(
    map = maps, df = select(sdg_2019_index, Country, Score, Rank),
    value = "Score", joinBy = c("name", "Country"),
    name = "SDG Global Score", borderColor = "#777"
  ) %>%
  hc_colorAxis(dataClasses = color_classes(c(0, 50, 60, 70, 80, 100),
    colors = c(brewer.pal(6, "Blues"))
  )) %>%
  hc_title(
    text = "<b>SDG Global Index 2019<b/>",
    margin = 20, align = "center",
    style = list(color = "black", useHTML = TRUE)
  ) %>%
  hc_tooltip(backgroundColor = "#FCFFC5", borderWidth = 2, valueDecimals = 2)


# Data for the sdg goal based map

series1 <- sdg_2019_index1 %>%
  group_by(name = goal1Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series2 <- sdg_2019_index1 %>%
  group_by(name = goal2Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "orange"))

series3 <- sdg_2019_index1 %>%
  group_by(name = goal3Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series4 <- sdg_2019_index1 %>%
  group_by(name = goal4Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series5 <- sdg_2019_index1 %>%
  group_by(name = goal5Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series6 <- sdg_2019_index1 %>%
  group_by(name = goal6Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series7 <- sdg_2019_index1 %>%
  group_by(name = goal7Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series8 <- sdg_2019_index1 %>%
  group_by(name = goal8Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series9 <- sdg_2019_index1 %>%
  group_by(name = goal9Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series10 <- sdg_2019_index1 %>%
  group_by(name = goal10Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))
series11 <- sdg_2019_index1 %>%
  group_by(name = goal11Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series12 <- sdg_2019_index1 %>%
  group_by(name = goal12Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series13 <- sdg_2019_index1 %>%
  group_by(name = goal13Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series14 <- sdg_2019_index1 %>%
  group_by(name = goal14Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "orange"))

series15 <- sdg_2019_index1 %>%
  group_by(name = goal15Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series16 <- sdg_2019_index1 %>%
  group_by(name = goal16Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))

series17 <- sdg_2019_index1 %>%
  group_by(name = goal17Dashboard) %>%
  do(data = list_parse(select(., country))) %>%
  ungroup() %>%
  mutate(color = c("yellow", "gray", "red", "green", "orange"))



# function for rendering the plots
hc_map_fun <- function(data, title, colorcode) {
  data <- data
  title <- title
  colorcode <- colorcode
  highchart(type = "map") %>%
    hc_plotOptions(map = list(
      borderColor = "#555", borderWidth = 0.5,
      allAreas = FALSE,
      joinBy = c("name", "country"),
      mapData = maps
    )) %>%
    hc_add_series_list(data) %>%
    hc_title(
      text = title,
      margin = 20, align = "center",
      style = list(color = colorcode, useHTML = TRUE)
    ) %>%
    hc_tooltip(pointFormat = "<b>{point.name}<b/><br/>{point.key}") %>%
    hc_legend(
      align = "center"
    ) %>%
    hc_add_theme(hc_theme_google())
}


hc_map1 <- hc_map_fun(series1, "<b> SDG 1 No Poverty<b/>", "#e5243b")
hc_map2 <- hc_map_fun(series2, "<b> SDG 2 Zero Hunger<b/>", "#DDA63A")
hc_map3 <- hc_map_fun(series3, "<b> SDG 3 Good Health And Well-Being<b/>", "#4C9F38")
hc_map4 <- hc_map_fun(series4, "<b> SDG 4 Quality Education<b/>", "#C5192D")
hc_map5 <- hc_map_fun(series5, "<b> SDG 5 Gender Equality<b/>", "#FF3A21")
hc_map6 <- hc_map_fun(series6, "<b> SDG 6 Clean Water And Sanitation<b/>", "#26BDE2")
hc_map7 <- hc_map_fun(series7, "<b> SDG 7 Affordable And Clean Energy<b/>", "#FCC30B")
hc_map8 <- hc_map_fun(series8, "<b> SDG 8 Decent Work And Economic Growth<b/>", "#A21942")
hc_map9 <- hc_map_fun(series9, "<b> SDG 9 Industry Innovation And Infrastructure<b/>", "#FD6925")
hc_map10 <- hc_map_fun(series10, "<b> SDG 10 Reduced Inequalities<b/>", "#DD1367")
hc_map11 <- hc_map_fun(series11, "<b> SDG 11 Sustainable Cities And Communities<b/>", "#FD9D24")
hc_map12 <- hc_map_fun(series12, "<b> SDG 12 Responsible Consumption And Production<b/>", "#BF8B2E")
hc_map13 <- hc_map_fun(series13, "<b> SDG 13 Climate Action<b/>", "#3F7E44")
hc_map14 <- hc_map_fun(series14, "<b> SDG 14 Life Below Water<b/>", "#0A97D9")
hc_map15 <- hc_map_fun(series15, "<b> SDG 15 Life On Land<b/>", "#56C02B")
hc_map16 <- hc_map_fun(series16, "<b> SDG 16 Peace Justice And Strong Institutions<b/>", "#00689D")
hc_map17 <- hc_map_fun(series17, "<b> SDG 17 Partnerships For Goals<b/>", "#19486A")


function(input, output) {

  # Reactive Action Button


  action_data <- reactiveValues(
    data = sdg_2019_index,
    plot = NULL,
    tabledata = NULL
  )

  observeEvent(input$goal1, {
    action_data$plot <- hc_map1
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 1 Score"),
      select(sdg_2019_index, Country, "Goal 1 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 1 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })

  observeEvent(input$goal2, {
    action_data$plot <- hc_map2
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 2 Score"),
      select(sdg_2019_index, Country, "Goal 2 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 2 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })

  observeEvent(input$goal3, {
    action_data$plot <- hc_map3
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 3 Score"),
      select(sdg_2019_index, Country, "Goal 3 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 3 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })

  observeEvent(input$goal4, {
    action_data$plot <- hc_map4
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 4 Score"),
      select(sdg_2019_index, Country, "Goal 4 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 4 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })

  observeEvent(input$goal5, {
    action_data$plot <- hc_map5
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 5 Score"),
      select(sdg_2019_index, Country, "Goal 5 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 5 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })


  observeEvent(input$goal6, {
    action_data$plot <- hc_map6
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 6 Score"),
      select(sdg_2019_index, Country, "Goal 6 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 6 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal7, {
    action_data$plot <- hc_map7
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 7 Score"),
      select(sdg_2019_index, Country, "Goal 7 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 7 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })


  observeEvent(input$goal8, {
    action_data$plot <- hc_map8
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 8 Score"),
      select(sdg_2019_index, Country, "Goal 8 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 8 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal9, {
    action_data$plot <- hc_map9
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 9 Score"),
      select(sdg_2019_index, Country, "Goal 9 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 9 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })

  observeEvent(input$goal10, {
    action_data$plot <- hc_map10
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 10 Score"),
      select(sdg_2019_index, Country, "Goal 10 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 10 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal11, {
    action_data$plot <- hc_map11
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 11 Score"),
      select(sdg_2019_index, Country, "Goal 11 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 11 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })

  observeEvent(input$goal12, {
    action_data$plot <- hc_map12
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 12 Score"),
      select(sdg_2019_index, Country, "Goal 12 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 12 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal13, {
    action_data$plot <- hc_map13
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 13 Score"),
      select(sdg_2019_index, Country, "Goal 13 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 13 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal14, {
    action_data$plot <- hc_map14
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 14 Score"),
      select(sdg_2019_index, Country, "Goal 14 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 14 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })


  observeEvent(input$goal15, {
    action_data$plot <- hc_map15
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 15 Score"),
      select(sdg_2019_index, Country, "Goal 15 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 15 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal16, {
    action_data$plot <- hc_map16
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 16 Score"),
      select(sdg_2019_index, Country, "Goal 16 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 16 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })



  observeEvent(input$goal17, {
    action_data$plot <- hc_map17
    action_data$tabledata <- DT::datatable(merge(select(sdg_2019_index2, Country, Score, "Global  Rank", Population, "GDP per capita", "Goal 17 Score"),
      select(sdg_2019_index, Country, "Goal 17 Dashboard"),
      by = "Country"
    ),
    options = list(searching = TRUE, info = FALSE), class = "cell-border stripe"
    ) %>%
      formatRound(c(2, 4, 5, 6), 2) %>%
      formatStyle("Goal 17 Dashboard", backgroundColor = styleEqual(
        c("SDG achieved", "Challenges remain", "Significant challenges remain", "information unavailable", "Major challenges remain"), c("green", "orange", "yellow", "gray", "red")
      ))
  })


  observeEvent(input$goal18, {
    action_data$plot <- sdg_score_map
    action_data$tabledata <- DT::datatable(select(sdg_2019_index, Country, Rank, Score) %>% arrange(Rank), options = list(searching = TRUE, info = FALSE), class = "cell-border stripe") %>% formatRound(c(3:5), 2)
  })


  # SDG overview Map
  output$overviewmap <- renderHighchart({
    if (is.null(action_data$plot)) {
      return(
        sdg_score_map
      )
    }
    action_data$plot
  })


  # List of SDG 2019 index
  output$sdg2019ConTable <- DT::renderDataTable({
    if (is.null(action_data$tabledata)) {
      return(
        # data <- select(sdg_2019_index, country,rank, score) %>% arrange(rank),
        DT::datatable(select(sdg_2019_index, Country, Rank, Score) %>% arrange(Rank), options = list(searching = TRUE, info = FALSE), class = "cell-border stripe") %>% formatRound(3, 2)
      )
    }
    action_data$tabledata
  })

  output$sdg_define <- renderText(({
    data <- data[data$Goal == input$sdggoal, 2]

    paste(data$Description)
  }))

  output$sdg_image <- renderImage(
    {
      # When input$n is 3, filename is ./image/image3.jpeg
      filename <- normalizePath(file.path(
        "./www/image/sdg_pic",
        paste(input$sdggoal, ".jpg", sep = "")
      ))

      # Return a list containing the filename and alt text
      list(
        src = filename,
        alt = paste("Image number", input$sdggoal)
      )
    },
    deleteFile = FALSE
  )

  output$sdgtable2 <- DT::renderDataTable(({
    data <- SDG_Indicator[SDG_Indicator$Goal == input$sdggoal, 2:3] %>% unique()

    DT::datatable(data, options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE), rownames = FALSE, colnames = "") %>% DT::formatStyle(0, target = "row", color = "black", opacity = ".8", fontWeight = "bold", lineHeight = "250%")
  }))


  # Analysis Report
  
  #Goal 1
  
  output$goal1_box1 <- renderValueBox({
    valueBox(report_data %>% mutate(x = (Population * `Poverty headcount ratio at $1.90/day (% population)`) / 100) %>% select(x) %>% sum(na.rm = T) %>% ceiling(), "Poverty headcount ratio at $1.90/day", color = "red",width = 4)
  })

  output$goal1_box2 <- renderValueBox({
    valueBox(report_data %>% mutate(x = (Population * `Poverty headcount ratio at $3.20/day (% population)`) / 100) %>% select(x) %>% sum(na.rm = T) %>% ceiling(), "Poverty headcount ratio at $3.20/day", color = "red")
  })


  
  output$goal1_chart <- renderHighchart({
    hchart(report_data$`Goal 1 Dashboard`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = "No.of Country"))%>%
      hc_xAxis(title = list(text ="Current Situation"))%>%
      hc_title(
        text = "Current Situation of Country SDG 1"
      )%>%
      hc_add_theme(hc_theme_google())
    
  })

  
  output$goal1_chart_trend <- renderHighchart({
    hchart(report_data$`Goal 1 Trend`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = "No.of Country"))%>%
      hc_xAxis(title = list(text ="Current Trend"))%>%
      hc_title(
        text = "Current Trend of Country SDG 1"
      )%>%
      hc_add_theme(hc_theme_google())
  })

  
  
  
  output$goal1_chart_poverty_rate_country <- renderHighchart({
    data <- arrange(report_data, `Poverty headcount ratio at $1.90/day (% population)`) %>%
      select(Country, `Poverty headcount ratio at $1.90/day (% population)`) %>%
      na.omit() %>%
      tail(10)

      hchart(data, "column", hcaes(x = Country, y = `Poverty headcount ratio at $1.90/day (% population)`), name = "Poverty Rate")%>%
        hc_yAxis(
          title = list(text = ""))%>%
        hc_xAxis(title = list(text =""))%>%
        hc_title(
          text = "Country with highest poverty rate %"
        )%>%
        hc_add_theme(hc_theme_google())
  })

  
  output$goal1_chart_poverty_pop_country <- renderHighchart({
    data <- report_data %>%
          mutate(population = (Population * `Poverty headcount ratio at $1.90/day (% population)`) / 100) %>%
          select(Country, population) %>%
          na.omit() %>%
          arrange(-population) %>%
          head(10)
    data$population <- ceiling(data$population)
    hchart(data,"column",hcaes(x = Country, y = population))%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with highest below poverty line population"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  # output$goal1_chart_poverty_pop_country <- renderHighchart({
  #   data <- report_data %>%
  #     mutate(x = (Population * `Poverty headcount ratio at $1.90/day (% population)`) / 100) %>%
  #     select(Country, x) %>%
  #     na.omit() %>%
  #     arrange(-x) %>%
  #     head(10)
  #   data$x <- ceiling(data$x)
  # 
  #   hchart(data, "column", hcaes(x = Country, y = x), name = "Population", colorByPoint = T)
  # })

  
  
  output$goal1_chart_poverty_region <- renderHighchart({
    data1 <- report_data %>%
      mutate(x = (Population * `Poverty headcount ratio at $3.20/day (% population)`) / 100) %>%
      select(`UN region name`, x) %>%
      na.omit() %>%
      group_by(`UN region name`) %>%
      summarise(population = sum(x)) %>%
      rename("RegionName" = "UN region name")

    data1$population <- ceiling(data1$population)
    hchart(data1, "scatter", hcaes(RegionName, population, size = population),colorByPoint = TRUE)%>%hc_tooltip(pointFormat = "<b>{point.name}<b/><br/>{point.y}")%>%
      hc_xAxis(title = list(text ="Region Name"))%>%
      hc_yAxis(title = list(text ="Population"))%>%
      hc_title(text="<b>Population below Poverty Line Based on Continents<b/>")%>%hc_add_theme(hc_theme_google())
  })
  
  
  # Goal 2 Analysis
  
  
  

  output$goal2_chart <- renderHighchart({
    hchart(report_data$`Goal 2 Dashboard`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Current Situation of Country SDG 2"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal2_chart_trend <- renderHighchart({
    hchart(report_data$`Goal 2 Trend`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Current Trend of Country SDG 2"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  
  output$goal2_chart_nourish <- renderHighchart({
    data <- arrange(report_data, `Prevalence of undernourishment (% population)`) %>%
      select(Country, `Prevalence of undernourishment (% population)`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Prevalence of undernourishment (% population)`), name = "Prevalence of undernourishment (%)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with most Prevalence of undernourishment (%)"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal2_chart_stunt <- renderHighchart({
    data <- arrange(report_data, `Prevalence of stunting (low height-for-age) in children under 5 years of age (%)`) %>%
      select(Country,  `Prevalence of stunting (low height-for-age) in children under 5 years of age (%)`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Prevalence of stunting (low height-for-age) in children under 5 years of age (%)`), name = "stunting in children under 5 years of age (%)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with most stunting in children under 5 years of age"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  output$goal2_chart_wast <- renderHighchart({
    data <- arrange(report_data, `Prevalence of wasting in children under 5 years of age (%)`) %>%
      select(Country,  `Prevalence of wasting in children under 5 years of age (%)`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Prevalence of wasting in children under 5 years of age (%)`), name = "Prevalence of wasting in children under 5 years of age (%)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with High Prevealence of wasting in children"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  
  output$goal2_chart_obese <- renderHighchart({
    data <- arrange(report_data, `Prevalence of obesity, BMI ≥ 30 (% adult population)`) %>%
      select(Country,  `Prevalence of obesity, BMI ≥ 30 (% adult population)`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Prevalence of obesity, BMI ≥ 30 (% adult population)`), name = "Adult obesity (%)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with High Adult Obesity"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
 
  
   # Goal 4
  
  
  output$goal4_chart <- renderHighchart({
    hchart(report_data$`Goal 4 Dashboard`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Current Situation of Country SDG 4"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal4_chart_trend <- renderHighchart({
    hchart(report_data$`Goal 4 Trend`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Current Trend of Country SDG 4"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  
  
  output$goal4_chart_primary <- renderHighchart({
    data <- arrange(report_data, `Net primary enrolment rate (%)`) %>%
      select(Country,  `Net primary enrolment rate (%)`) %>%
      na.omit() %>%
      head(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Net primary enrolment rate (%)`), name = "primary enrolment rate")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with most stunting in children under 5 years of age"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  

  output$goal4_chart_litracy <- renderHighchart({
    data <- arrange(report_data, `Literacy rate of 15-24 year olds, both sexes (%)`) %>%
      select(Country,  `Literacy rate of 15-24 year olds, both sexes (%)`) %>%
      na.omit() %>%
      head(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Literacy rate of 15-24 year olds, both sexes (%)`), name = "Literacy Rate %")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with most stunting in children under 5 years of age"
      )%>%
      hc_add_theme(hc_theme_google())
  })  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Goal 5
  
  output$goal5_chart <- renderHighchart({
    hchart(report_data$`Goal 5 Dashboard`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with most stunting in children under 5 years of age"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal5_chart_trend <- renderHighchart({
    hchart(report_data$`Goal 5 Trend`, colorByPoint = TRUE, name = "No.of Country")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with most stunting in children under 5 years of age"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  
  output$goal5_female_school_max <- renderHighchart({
    data <- arrange(report_data, `Ratio of female to male mean years of schooling of population age 25 and above`) %>%
      select(Country,  `Ratio of female to male mean years of schooling of population age 25 and above`) %>%
      na.omit() %>%
      head(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Ratio of female to male mean years of schooling of population age 25 and above`), name = "Female Schooling")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with low Female Schooling"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal5_female_school_min <- renderHighchart({
    data <- arrange(report_data, `Ratio of female to male mean years of schooling of population age 25 and above`) %>%
      select(Country,  `Ratio of female to male mean years of schooling of population age 25 and above`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y = `Ratio of female to male mean years of schooling of population age 25 and above`), name = "Female Schooling")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with high Female Schooling"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal5_female_labour_max <- renderHighchart({
    data <- arrange(report_data, `Ratio of female to male labour force participation rate`) %>%
      select(Country, `Ratio of female to male labour force participation rate`) %>%
      na.omit() %>%
      head(10)
    
    hchart(data, "column", hcaes(x = Country, y =`Ratio of female to male labour force participation rate`), name = "Female labor force participation (% male)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with low Female labor force participation"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  output$goal5_female_labour_min <- renderHighchart({
    data <- arrange(report_data, `Ratio of female to male labour force participation rate`) %>%
      select(Country, `Ratio of female to male labour force participation rate`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y =`Ratio of female to male labour force participation rate`), name = "Female labor force participation (% male)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with high Female labor force participation"
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
 
  output$goal5_female_parli_max <- renderHighchart({
    data <- arrange(report_data, `Seats held by women in national parliaments (%)`) %>%
      select(Country,`Seats held by women in national parliaments (%)`) %>%
      na.omit() %>%
      head(10)
    
    hchart(data, "column", hcaes(x = Country, y =`Seats held by women in national parliaments (%)`), name = "Women in national parliaments (%)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with low Women presence in national parliaments "
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  
  output$goal5_female_parli_min <- renderHighchart({
    data <- arrange(report_data,`Seats held by women in national parliaments (%)`) %>%
      select(Country, `Seats held by women in national parliaments (%)`) %>%
      na.omit() %>%
      tail(10)
    
    hchart(data, "column", hcaes(x = Country, y =`Seats held by women in national parliaments (%)`), name = "Women in national parliaments (%)")%>%
      hc_yAxis(
        title = list(text = ""))%>%
      hc_xAxis(title = list(text =""))%>%
      hc_title(
        text = "Country with High Women presence in national parliaments "
      )%>%
      hc_add_theme(hc_theme_google())
  })
  
  
  
  
  
# Nepal Analysis
# output$Nepal_box_SDG_rank <- renderValueBox({
#   valueBox(report_data %>% filter(Country == "Nepal") %>% select(`Global  Rank`), "Global Rank")
# })
# 
# output$Nepal_box_score <- renderValueBox({
#   valueBox(report_data %>% filter(Country == "Nepal") %>% select(Score), "Global Index Score")
# })
# 
# output$Nepal_box_gdp <- renderValueBox({
#   valueBox(report_data %>% filter(Country == "Nepal") %>% select(`GDP per capita`), "GDP per capita")
# })
# 
# output$Nepal_box_pop <- renderValueBox({
#   valueBox(report_data %>% filter(Country == "Nepal") %>% select(Population), "Population")
# })
# 
# output$Nepal_polar_column <- renderHighchart({
#   data<-report_data%>%filter(Country=="Nepal")%>%select(Country,`Goal 1 Score`,`Goal 2 Score`,`Goal 3 Score`,`Goal 4 Score`,`Goal 5 Score`,`Goal 6 Score`,`Goal 7 Score`,`Goal 8 Score`,`Goal 9 Score`,`Goal 10 Score`,`Goal 11 Score`,`Goal 12 Score`,`Goal 13 Score`,`Goal 14 Score`,`Goal 15 Score`,`Goal 16 Score`,`Goal 17 Score`)%>%pivot_longer(-Country,names_to = "name",values_to = "value")
#   data$y <-data$value
#   data$low<- 0
#   data$high<- 100
#   data$e<-rnorm(17,mean= 3,sd=2)
#   data$x<-c(0:16)
#   
#   create_hc <- function(t) {
#     
#     dont_rm_high_and_low <- c("arearange", "areasplinerange",
#                               "columnrange", "errorbar")
#     
#     is_polar <- str_detect(t, "polar")
#     
#     t <- str_replace(t, "polar", "")
#     
#     if(!t %in% dont_rm_high_and_low) data <- data %>% select(-e, -low, -high)
#     
#     
#     highchart() %>%
#       hc_title(text = "SDGs Index Score",
#                style = list(fontSize = "15px")) %>% 
#       hc_chart(type = t,
#                polar = is_polar) %>% 
#       hc_xAxis(categories = data$name) %>% 
#       hc_add_series(data, name = "Fruit Consumption", showInLegend = FALSE) 
#     
#   }
#   
#   hcs <- c("polarcolumn")%>%map(create_hc) 
#   hcs
# })
# 
# 
# output$goal1_chart <- renderHighchart({
#   hchart(report_data$`Goal 1 Dashboard`, colorByPoint = TRUE, name = "No.of Country")
# })


}

