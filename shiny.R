#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(plotly)
library(rsconnect)

pensiondataset<-read_excel("/cloud/project/shiny_assignment_data.xlsx")

tarih <-lubridate::parse_date_time(pensiondataset$date,"dmY")
print(tarih)
pensiondataset <- pensiondataset %>% mutate(expense_ratio = ((contribution - size_total)/contribution)*100 ) %>% mutate(profit = ((fund_size_participants-size_total)/size_total)*100 )


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Analysis of Pension Funds for Two Companies"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            dateRangeInput('dateRange2',
                           label = paste('Date'),
                           start = min(tarih), end = max(tarih) , 
                           min = min(tarih), max = max(tarih),
                           separator = " - ", format = "dd/mm/yy",
                           startview = 'year', language = 'en', weekstart = 1
            ) ,
            
            selectInput("select", label = h3("Select Company"), 
                        choices = unique(pensiondataset$pension_fund_company), 
                        selected = 1),
            selectInput("select2", label = h3("Select Company 2"), 
                        choices = unique(pensiondataset$pension_fund_company), 
                        selected = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram


server <- function(input, output) {
   
    plot_data <- reactive({
        pl_df <-
            pensiondataset  %>% filter(pension_fund_company %in% input$select) %>% filter(lubridate::parse_date_time (date,"dmy") >= input$dateRange2[1] &
                                                                                             lubridate::parse_date_time (date,"dmy") <= input$dateRange2[2]  )
        
        
    })
    
    plot_data2 <- reactive({
        pl_df2 <-
            pensiondataset  %>% filter(pension_fund_company %in% input$select2) %>% filter(lubridate::parse_date_time (date,"dmy") >= input$dateRange2[1] &
                                                                                              lubridate::parse_date_time (date,"dmy") <= input$dateRange2[2]  )
    })    
     
    # Create expense ratio plot
    output$distPlot <- renderPlot({
            ggplot() +
                geom_smooth(data=plot_data() ,aes(x=lubridate::parse_date_time (date,"dmy") ,y=expense_ratio,color=input$select) , stat = "identity" ) + 
                geom_smooth(data=plot_data2() ,aes(x=lubridate::parse_date_time (date,"dmy") ,y=expense_ratio,color=input$select2) , stat = "identity")+
                xlab(label = "Date") +
                ylab(label = "Expense Ratio Comparision") +
                labs (color = "Companies")
    })
    
    # Create Fund Size Participants plot
    output$distPlot2 <- renderPlot({
            ggplot() +
                geom_smooth(data=plot_data() ,aes(x=lubridate::parse_date_time (date,"dmy") ,y=profit,color=input$select) , stat = "identity" ) + 
                geom_smooth(data=plot_data2() ,aes(x=lubridate::parse_date_time (date,"dmy") ,y=profit,color=input$select2) , stat = "identity")+
                xlab(label = "Date") +
                ylab(label = "Fund Size Participants") +
                labs (color = "Companies")    
        
        
        
        
    })
    
 }
# Run the application 
shinyApp(ui = ui, server = server)

