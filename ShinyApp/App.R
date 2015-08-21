# Developing Data Products Shiny Project
#babynames::babynames: US baby name data for each year from 1880 to 2013, 
#the number of children of each sex given each name. 
#All names used 5 or more times are included. 
#1,792,091 rows, 5 columns (year, sex, name, n, prop). 
#(Source: Social security administration).


suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(htmlwidgets))
suppressPackageStartupMessages(library(taucharts))

library(babynames)



ui <- fluidPage(
      titlePanel("Exploring Baby Names"),
      hr(),
# Use instructions
      h5("Instructions:"),
      helpText("This Shiny app allows you to explore babynames from the Hadley Wickham´s dataset (years 1880 to 2013). Two options are provided; a chart showing the top baby name for each year or, a chart displaying the trend for a specific name."),
      fluidRow(
            column(width = 6, helpText("1.- Select the chart to display")),
            column(width = 6, 
                   conditionalPanel(condition = "input.displayme == 1",
                                    helpText("2.- Check / Uncheck name gender")),
                   conditionalPanel(condition = "input.displayme == 2",
                                    helpText("2.- Type the desired baby name and click the button"))
                  )
      ),
# 1 Provide chart options. Option 1 shows pre selected      
      fluidRow(
            column(width = 6, wellPanel(
                        radioButtons("displayme", 
                              label = "Chart to Display:",
                              choices = c("Most Popular Baby Names" = 1, "I want to type a Name" = 2),
                              selected = 1),
                        br()
                  )),
# 2 Conditional display for column 2 options based on 1 selection                 
            column(width = 6, wellPanel(       
                        conditionalPanel(condition = "input.displayme == 1",
                              checkboxGroupInput("sexoptions",
                                                label = "Sex:",
                                                choices = c("Female" = "F", "Male" = "M"),
                                                selected = c("F", "M")
                                                ), 
                              br()
                                          ),
                        conditionalPanel(condition = "input.displayme == 2" ,
                             textInput(inputId = "choosenname", 
                                       label = "Type a Name", 
                                       value = ""),
                             actionButton(inputId = "makechange", "Display Chart")
                                        )
            ))
            ),
     # hr(),
helpText("3.- Hover over chart to display tooltip, hover over legend to better see selection or, click selection to remove it"),
# Conditional display for charts in next row based on selection 1
      conditionalPanel(condition = "input.displayme == 1", 
                  h4("Most Popular Baby Names"),
                  tauchartsOutput("tautopyear")
                  ),
            
      
      conditionalPanel(condition = "input.displayme == 2", 
            h4(textOutput("chartTitle")),
            tauchartsOutput("tautline")
      )
      
)
      
#-----------------------------------------------------------
server <- function(input, output){
# Filter for option 1 chart. baby names according to selection in 2.
# Reactive to only change chart if selection in 2 changes
      topnames <- reactive({
                  babynames %>% 
                  filter(sex %in% input$sexoptions) %>% 
                  group_by(year) %>% top_n(1, wt=prop)
                  })
      
# Filter for option 2 chart. Specific baby name.
# Reactive to only change chart if button is clicked
      datatouse <- eventReactive(input$makechange, {
                  babynames %>% filter(name %in% str_to_title(input$choosenname))
                  })
# Title tor option 2 chart. 
# Reactive to only display the title with the chart (tau_title not working for shiny in this version)            
      theTitle <- reactive({
                  ifelse (input$makechange == "0", "", "Name Popularity")
                  })
            
# Make tauCharts      
      output$chartTitle <- renderText(theTitle())
      output$tautopyear <- renderTaucharts({
                              tauchart(topnames()) %>% 
                              tau_point(x="year", y="n",  "name") %>%
                              tau_guide_x( tick_format = "y", auto_scale = FALSE ) %>% 
                              tau_tooltip() %>% 
                              tau_legend()
                              
      })
      
      
      output$tautline <- renderTaucharts({
                              tauchart(datatouse()) %>% 
                              tau_line(x="year", y="n", "sex") %>% 
                              tau_guide_x( tick_format = "y", auto_scale = FALSE ) %>% 
                              tau_tooltip() %>% 
                              tau_legend()
                              
      })
      

      
}

shinyApp(ui=ui, server=server)

