#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgeos)
library(rgdal)
library(leaflet)
library(DT)
library(tidyr)
library(dplyr)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  load("data.RData")
  
  output$month <- renderText(input$month)
  
  output$crime <- renderText(input$crime_group)
  
  
  summarydata <- reactive({
    
    if (input$crime_group == "All crime") {
      
      dataSet<-crimeSummary[crimeSummary$Month==input$month,]
      
      # Summarise data by LSOA and Month
      dataSet<- dataSet %>% group_by(LSOA.name, Month) %>% summarise(Total = sum(Total))
      
      # Copy our GIS data
      joinedDataset<-lsoa
      
      # Join the two datasets together
      joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by= c("LSOA11NM" = "LSOA.name")))
  
      # Replace any mising values with 0
      joinedDataset@data$Total[is.na(joinedDataset@data$Total)] <- 0
      
      # Return dataset
      joinedDataset
      
    }
    else {
      
      # Get a subset of the income data which is contingent on the input variables
      dataSet<-crimeSummary[crimeSummary$Month==input$month & crimeSummary$Crime.type==input$crime_group, ]
      
      # Copy our GIS data
      joinedDataset<-lsoa
      
      # Join the two datasets together
      joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, dataSet, by= c("LSOA11NM" = "LSOA.name")))
      
      # Replace any mising values with 0
      joinedDataset@data$Total[is.na(joinedDataset@data$Total)] <- 0
      
      # Return dataset
      joinedDataset
    }
  })
  
  
  output$infoBox1 <- renderInfoBox({
    data <- summarydata()
    
    total <- sum(data$Total, na.rm = TRUE)
    
    infoBox(
      "Monthly Total",
      total,
      icon = icon("equals"),
      color = "navy"
    )
  })
  
  
  output$infoBox2 <- renderInfoBox({ 
    data <- summarydata()
    
    highest <- data@data[which.max(data$Total),]
    
    infoBox(
      "Highest Volume LSOA",
      highest$LSOA11NM,
      icon = icon("arrow-up"),
      color = "navy"
    )
    
  })    
  
  output$infoBox3 <- renderInfoBox({ 
    data <- summarydata()
    
    lowest <- data@data[which.min(data$Total),]
    
    infoBox(
      "Lowest Volume LSOA",
      lowest$LSOA11NM,
      icon = icon("arrow-down"),
      color = "navy"
    )
    
  })  
  
  # Due to use of leafletProxy below, this should only be called once
  output$lsoaMap<-renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      
      # Centre the map in the middle of our co-ordinates
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=10 # set to 10 as 9 is a bit too zoomed out
      )       
    
  })
  
  
  observe({
    
    theData <- summarydata()
    
    if (input$crime_group == "All crime") { 
      
      # colour palette mapped to data
      pal <- colorBin("viridis" , theData$Total, bins = 5, pretty = TRUE) 
      
      # set text for the clickable popup labels
      lsoa_popup <- paste0("<strong>LSOA: </strong>", 
                              theData$LSOA11NM, 
                              "<br><strong>",
                              "Month </strong>",
                              theData$Month,
                              "<br><strong>",
                              "Total: </strong>", 
                              theData$Total
      )
      
      # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
      leafletProxy("lsoaMap", data = theData) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = theData,
                    fillColor = pal(theData$Total), 
                    fillOpacity = 0.7, 
                    color = "#BDBDC3", 
                    weight = 2,
                    popup = lsoa_popup) %>%
        addLegend("bottomright", pal = pal, values = ~Total,
                  title = "Crime Volume",
                  opacity = 1
        )
    }
    else {
      
      # colour palette mapped to data
      pal <- colorBin("viridis", theData$Total, bins = 5, pretty = TRUE) 
      
      # set text for the clickable popup labels
      lsoa_popup <- paste0("<strong>LSOA: </strong>", 
                              theData$LSOA11NM, 
                              "<br><strong>",
                              "Month </strong>",
                              theData$Month,
                              "<br><strong>",
                              "Crime: </strong>",
                              theData$Crime.type,
                              "<br><strong>",
                              "Total: </strong>", 
                              theData$Total
      )
      
      # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
      leafletProxy("lsoaMap", data = theData) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = theData,
                    fillColor = pal(theData$Total), 
                    fillOpacity = 0.7, 
                    color = "#BDBDC3", 
                    weight = 2,
                    popup = lsoa_popup) %>%
        addLegend("bottomright", pal = pal, values = ~Total,
                  title = "Crime Volume",
                  opacity = 1
        )
    }
  })
  
  # table of results, rendered using data table
  output$lsoaTable <- DT::renderDataTable(
    if (input$crime_group == "All crime") {
      
      datatable({
        dataSet<-summarydata()
        dataSet<-dataSet@data[,c(2,4)] # Just get name and value columns
        names(dataSet)<-c("LSOA", "Total" )
        dataSet
      }, 
      options = list(lengthMenu = c(5, 10, 33), pageLength = 5, order = c(2, 'desc'))
      )
    }
    else {
      
      datatable({
        dataSet<-summarydata()
        dataSet<-dataSet@data[,c(2,4,5)] # Just get name and value columns
        names(dataSet)<-c("LSOA", "Crime Type", "Total" )
        dataSet
      }, 
      options = list(lengthMenu = c(5, 10, 33), pageLength = 5, order = c(3, 'desc'))
      )
    }  
  )
  
  output$burglary <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Burglary",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$bicycle <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Bicycle Theft",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$criminal <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Criminal Damage and Arson",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$criminal <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Criminal Damage and Arson",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$drugs <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Drugs",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$other_c <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Other Crime",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$other_t <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Other Theft",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$public <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Public Order",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$weapon <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Possession of Weapons",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$robbery <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Robbery",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$shop <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Shoplifting",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$theft <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Theft from the Person",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$vehicle <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Vehicle Crime",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  output$violence <- renderDataTable(
    datatable({
      dataSet <- police_uk_category_mappings
      dataSet <- dataSet[dataSet$'Police.uk Category' == "Violence and Sexual Offences",]
      dataSet <- dataSet[,c(1,2,3)]
    }, 
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5, dom = 'p')
    )
  )
  
  
     
 
  
})
