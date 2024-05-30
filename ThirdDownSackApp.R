
library(shiny)
library(nflreadr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)

pbp22_s <- nflreadr::load_pbp(seasons = 2022)

teamValues <- unique(pbp22_s$posteam)
yearValues <- c(NA, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002)
fillCols_off <- c("run" = "blue", "pass" = "red", "sackPerPass" = "orange", "success" = "purple")
fillCols_def <- c("run" = "blue", "pass" = "red", "sackPerPass" = "orange", "stop" = "purple")

ui <- fluidPage(
  titlePanel("NFL Third Down Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year_off", "What NFL Season Do You Want To See For This Offense?", choices = yearValues),
      selectInput("team_off", "Which Team's Offense Do You Want To Look At?", choices = teamValues),
      selectInput("year_def", "What NFL Season Do You Want To See For This Defense?", choices = yearValues),
      selectInput("team_def", "Which Team's Defense Do You Want To Look At?", choices = teamValues),
      selected = c(teamValues[[2]], yearValues[[2]])
), 
        mainPanel(
      plotlyOutput("offPlotly"),
      plotlyOutput("defPlotly")
    )
  )
)

?nflreadr::load_pbp()

server <- function(input, output) {



  

  
  output$offPlotly <- renderPlotly({
    selected_offteam <- input$team_off
    selected_year <- as.numeric(input$year_off)
    off_plot<- nflreadr::load_pbp(seasons = selected_year) %>% 
      filter(!is.na(sack), play_type %in% c("run", "pass"), down == 3, .data$posteam == selected_offteam) %>% 
      mutate(yds_to_go_groups = case_when( ydstogo <= 4 ~ "short",
                                           ydstogo %in% (4:8) ~ "medium",
                                           ydstogo %in% (9:12) ~ "long",
                                           ydstogo %in% (13:16) ~ "super long",
                                           ydstogo > 16 ~ "good luck" )) %>%
      group_by(yds_to_go_groups) %>% 
      summarise(plays =  n(),
                run = round(sum(play_type == "run") / plays, 3),
                pass = round(sum(play_type == "pass") / plays, 3),
                passes = sum(play_type == "pass"),
                sackPerPass = round(sum(sack == 1) / passes, 3),
                epa_per_play = mean(epa), 
                success = round(sum(third_down_converted == 1) / plays, 3)) %>% 
      filter(plays >= 3) %>% 
      pivot_longer(cols = c("run", "pass", "sackPerPass", "success")) %>% 
      rename("result" = "name", 
             "percent" = "value") %>% 
      mutate(yds_to_go_groups = factor(yds_to_go_groups, levels = c("short", "medium", "long", "super long", "good luck"))) %>% 
      ggplot() + geom_col(aes(x = yds_to_go_groups, y = percent, fill = result), color = "white", position = "dodge") + 
      theme(axis.ticks = element_line(linetype = "blank"),
            panel.grid.major = element_line(linetype = "blank"),
            axis.text = element_text(family = "serif"),
            axis.text.x = element_text(family = "serif"),
            axis.text.y = element_text(family = "serif"),
            legend.text = element_text(family = "serif"),
            legend.title = element_text(family = "serif"),
            panel.background = element_rect(fill = "green4"),
            plot.background = element_rect(fill = "antiquewhite"),
            legend.key = element_rect(fill = "antiquewhite"),
            legend.background = element_rect(fill = "antiquewhite"),
            legend.position = "bottom", legend.direction = "horizontal") + labs(title = paste0(selected_offteam, "'s Third Down Offense in ", selected_year), x = "Third and ____?", y = "Percentage", fill = "Play or Result:") +
      scale_color_manual(values = fillCols_off, aesthetics = "fill") + 
      scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent_format(accuracy = 1)) 
    
    ggplotly(off_plot)

  })
  
  output$defPlotly <- renderPlotly({
    selected_defteam <- input$team_def
    selected_year <- as.numeric(input$year_def)
    

      def_plot <- nflreadr::load_pbp(seasons = selected_year) %>% 
      filter(!is.na(sack), play_type %in% c("run", "pass"), down == 3, .data$defteam == selected_defteam) %>% 
      mutate(yds_to_go_groups = case_when( ydstogo <= 4 ~ "short",
                                           ydstogo %in% (4:8) ~ "medium",
                                           ydstogo %in% (9:12) ~ "long",
                                           ydstogo %in% (13:16) ~ "super long",
                                           ydstogo > 16 ~ "good luck" )) %>%
      group_by(yds_to_go_groups) %>% 
      summarise(plays =  n(),
                run = round(sum(play_type == "run") / plays, 3),
                pass = round(sum(play_type == "pass") / plays, 3),
                passes =  sum(play_type == "pass"),
                sackPerPass = round(sum(sack == 1) / passes, 3),
                epa_per_play = mean(epa), 
                stops = 1 - (sum(third_down_converted == 1) / plays), 
                stop = round(stops, 3)) %>% 
      filter(plays >= 3) %>% 
      pivot_longer(cols = c("run", "pass", "sackPerPass", "stop")) %>% 
      rename("result" = "name",
             "percent" = "value") %>% 
       mutate(yds_to_go_groups = factor(yds_to_go_groups, levels = c("short", "medium", "long", "super long", "good luck"))) %>% 
        ggplot() + geom_col(aes(x =  yds_to_go_groups, y = percent, fill = result), color = "white", position = "dodge") + 
        theme(axis.ticks = element_line(linetype = "blank"),
              panel.grid.major = element_line(linetype = "blank"),
              axis.text = element_text(family = "serif"),
              axis.text.x = element_text(family = "serif"),
              axis.text.y = element_text(family = "serif"),
              legend.text = element_text(family = "serif"),
              legend.title = element_text(family = "serif"),
              panel.background = element_rect(fill = "green4"),
              plot.background = element_rect(fill = "antiquewhite"),
              legend.key = element_rect(fill = "antiquewhite"),
              legend.background = element_rect(fill = "antiquewhite"),
              legend.position = "bottom", legend.direction = "horizontal") + labs(title = paste0(selected_defteam, "'s Third Down Defense in ", selected_year), x = "Third and ____?", y = "Percentage", fill = "Play or Result:") +
        scale_color_manual(values = fillCols_def, aesthetics = "fill") + 
        scale_y_continuous(breaks = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent_format(accuracy = 1)) 
    
        ggplotly(def_plot)
  })
  
  
}

shinyApp(ui = ui, server = server)