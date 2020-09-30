library(RMySQL)
library(readxl)
library(shiny)

con <- dbConnect(RMySQL::MySQL(), host = "localhost",dbname="houseprice",user = "root", password = "")


# Define UI for application 
yy <- fluidPage(
  titlePanel("Analisis Houseprices"),
  sidebarLayout(
    sidebarPanel(
      selectInput("hprice", "Pilih:", 
                  choices = c("Price", "SqFt",
                              "Bedrooms", "Bathrooms", 
                              "Offers", "Brick", 
                              "Neighborhood"))

      
      
      
    ),
    mainPanel(
      plotOutput('contents')
    )
  )
)

hh <- function(input, output) {
  output$contents <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$hprice
    
    if (is.null(inFile))
      return(NULL)
    
    
    myQuery2 <- "select * from `table 2`"
    dff <- dbGetQuery(con, myQuery2)
    
    select = input$hprice
    
    if(select=="Price"){
      boxplot(dff$Price)
    }else if(select=="SqFt"){
      boxplot(dff$SqFt)
    }
    else if(select=="Bedrooms"){
      boxplot(dff$Price~dff$Bedrooms)
    }
    else if(select=="Bathrooms"){
      boxplot(dff$Price~dff$Bathrooms)
    }
    else if(select=="Offers"){
      boxplot(dff$Price~dff$Offers)
    }
    else if(select=="Brick"){
      boxplot(dff$Price~dff$Brick)
    }
    else if(select=="Neighborhood"){
      boxplot(dff$Price~dff$Neighborhood)
    }
    
  })
}


shinyApp(ui = yy, server=hh)
