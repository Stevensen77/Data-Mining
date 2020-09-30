library(shiny)
library(factoextra)
library(NbClust)


# Define UI for application 
hm <- fluidPage(
  titlePanel("K-Means Cluster"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Pilih File CSV',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      tags$hr() 
    ),
    mainPanel(
      plotOutput('contents')
    )
  )
)
ya <- function(input, output) {
  output$contents <- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    dataku <- read.csv(inFile$datapath)
    kmeanswilda=dataku[,2:6]
    
    nb <- NbClust(kmeanswilda, distance = "euclidean", min.nc =2, max.nc = 10, 
                  method = "complete", index ="all")
    km.res=kmeans(kmeanswilda,3,nstart = 25)
    fviz_cluster(km.res, data = kmeanswilda, geom = "point",stand = FALSE, frame.type = "norm")
    fviz_cluster(km.res, data = kmeanswilda)
  })
}
shinyApp(ui = hm, server=ya)
