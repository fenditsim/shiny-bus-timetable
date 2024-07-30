# Load packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)

# Import relevant R scripts
source("data.R")
source("tabInfo.R")
source("tabLog.R")
#source("https://raw.githubusercontent.com/dreamRs/shinyWidgets/26838f9e9ccdc90a47178b45318d110f5812d6e1/R/useShinydashboard.R")  # In case when useShinyDashboard() is depreciated

# Shiny app
shinyApp(
  ui = tagList(
    useShinyjs(),
    useShinydashboard(),
    navbarPage(
      theme = "mytheme.css",
      title = "UoW Bus TimeTable", 
      collapsible = TRUE,
      tabPanel(title = "Timetable",
              # Inputs for origin and destination
              fluidPage(
                mainPanel(
                  width = 10,
                  fluidRow(
                    virtualSelectInput(inputId = "origin",
                                       label = "From: ",
                                       choices = stops,
                                       selected = "Coventry Pool Meadow",
                                       width = "100%",
                                       search = TRUE
                                       )
                  ),
                  fluidRow(
                    virtualSelectInput(inputId = "dest",
                                       label = "To: ",
                                       choices = stops,
                                       selected = "University of Warwick Bus Int",
                                       width = "100%",
                                       search = TRUE
                    )
                  ),
                  # Latest bus arrival time
                  fluidRow(
                      div(h1("12x"),
                          style="min-width:20px;max-width:50%; float:left"
                          ),
                      div(div(id = "click.12x", shinydashboard::valueBoxOutput(outputId = "latest.12x", width = 12)),
                          style="min-width:280px;max-width:100%; float:right"
                          ),
                  ),
                  fluidRow(
                    div(h1("11"),
                        style="min-width:20px;max-width:50%; float:left"
                    ),
                    div(div(id = "click.11", valueBoxOutput(outputId = "latest.11", width = 12)),
                        style="min-width:280px;max-width:100%; float:right"
                    ),
                  )
                )
              )
      ),
      tabPanel(title = "Info", tabInfo),
      tabPanel(title = "Log", tabLog)
    )
  ),
  server = function(input, output, session){
    
    # update choices for dest
    observe({
      req(input$origin)
      updateVirtualSelect(inputId = "dest", label = "To: ", choices = if(input$origin != "University of Warwick Bus Int") "University of Warwick Bus Interchange" else stops[!stops %in% c("University of Warwick Bus Int", "Quadrant")], 
                          selected = if (input$origin != "University of Warwick Bus Int") "University of Warwick Bus Interchange" else "Coventry Pool Meadow")
    })
    
    # Create reactive objects based on origin and dest
    start <- reactive({ if (input$origin == "University of Warwick Bus Interchange") "University of Warwick Bus Int" else input$origin })
    end <- reactive({ if (input$origin == "University of Warwick Bus Int") { if (input$dest %in% c("Canley Prior Deram Walk", "Kenilworth Clock", "Leamington Spa Upper Parade", "Leamington Spa High Street")) input$dest else "University of Warwick Bus Int" } else "Uni" })
    end.origin <- reactive({ input$dest })
    check <- reactive({ list(start(), end(), end.origin()) })
    
    # Find the latest bus arrival time based on start and end
    observeEvent(check(),{
      # 12x
      output$latest.12x <- renderValueBox({
        invalidateLater(1000, session)  # Ensure the site gets updated after it gets idle for a certain period of time
        
        if (end.origin() %in% c("Earlsdon City Arms", "Coventry Station Interchange", "Leamington Spa High Street", "Leamington Spa Upper Parade", "Kenilworth Clock")){
          valueBox(
            value = "NA",
            subtitle = "", color = "purple"
          )
        }else{
          if (!is.na(timecheck.12x(origin = start(), dest = end()))){
            valueBox(
              value = paste0(timecheck.12x(origin = start(), dest = end(), min = T), " minutes"), 
              subtitle = tags$p(timecheck.12x(origin = start(), dest = end()), style = "font-size: 100%;"), 
              color = if (timecheck.12x(origin = start(), dest = end(), min = T) < 5) "red" else if (timecheck.12x(origin = start(), dest = end(), min = T) >= 5 & timecheck.12x(origin = start(), dest = end(), min = T) < 10) "yellow" else "green"
            )
          }else{
            valueBox(
              value = "NA",
              subtitle = "", color = "purple"
            )
          } 
        }
      })
      
      # 11
      output$latest.11 <- renderValueBox({
        invalidateLater(1000, session)  # Ensure the site gets updated after it gets idle for a certain period of time
        
        if (end.origin() %in% c("Coventry Station Warwick Road", "Canley Prior Deram Walk")){
          valueBox(
            value = "NA",
            subtitle = "", color = "purple"
          )
        }else{
          if (!is.na(timecheck.11(origin = start(), dest = end()))){
            valueBox(
              value = paste0(timecheck.11(origin = start(), dest = end(), min = T), " minutes"), 
              subtitle = tags$p(timecheck.11(origin = start(), dest = end()), style = "font-size: 100%;"), 
              color = if (timecheck.11(origin = start(), dest = end(), min = T) < 5) "red" else if (timecheck.11(origin = start(), dest = end(), min = T) >= 5 & timecheck.11(origin = start(), dest = end(), min = T) < 10) "yellow" else "green"
            )
          }else{
            valueBox(
              value = "NA",
              subtitle = "", color = "purple"
            )
          }
        }
        

      })
    })
    
    observeEvent(check(), {
      # 12x
      onclick('click.12x', showModal(modalDialog(
        title = "Arrival Time (including last bus arrival)",
        DT::renderDT({
          invalidateLater(60000, session)  # Ensure the site gets updated after it gets idle for a certain period of time
          
          if (end.origin() %in% c("Coventry Station Interchange", "Earlsdon City Arms")){
            DT::datatable(timecheck.12x(origin = start(), dest = "dest", DT=T), # set dist=dist to avoid generating incorrect timetable once clicked
                          filter = "none",
                          selection = "none",
                          options = list(searching = FALSE, 
                                         lengthChange = FALSE,
                                         ordering = FALSE
                          ),
                          rownames = FALSE
            )
          }else{
            DT::datatable(timecheck.12x(origin = start(), dest = end(), DT=T),
                          filter = "none",
                          selection = "none",
                          options = list(searching = FALSE, 
                                         lengthChange = FALSE,
                                         ordering = FALSE
                          ),
                          rownames = FALSE
            )
          }
        })
      )))
      
      # 11
      onclick('click.11', showModal(modalDialog(
        title = "Arrival Time (including last bus arrival)",
        DT::renderDT({
          invalidateLater(60000, session)  # Ensure the site gets updated after it gets idle for a certain period of time
          
          if(end.origin() %in% c("Coventry Station Warwick Road")){
            DT::datatable(timecheck.11(origin = start(), dest = "dest", DT=T), # set dist=dist to avoid generating incorrect timetable once clicked
                          filter = "none",
                          selection = "none",
                          options = list(searching = FALSE, 
                                         lengthChange = FALSE,
                                         ordering = FALSE
                          ),
                          rownames = FALSE
            )
          }else{
            DT::datatable(timecheck.11(origin = start(), dest = end(), DT=T),
                          filter = "none",
                          selection = "none",
                          options = list(searching = FALSE, 
                                         lengthChange = FALSE,
                                         ordering = FALSE
                          ),
                          rownames = FALSE
            )
          }
          

        })
      )))
    })
    
    
    
  }
)