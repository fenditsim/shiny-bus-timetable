tabLog <- tabItem(tabName = "log",
                  h1("Version Log"),
                  h2("v0.4 - 2024.08.01"),
                  tags$div(
                    tags$ul(
                      tags$li("Fixed known issue with Bus 14 (from Coventry to University) and 14a (not showing timeslots)"),
                      tags$li("Updated Resources section (in Info tab) for relevant post")
                    )
                  ),
                  h2("v0.3 - 2024.07.31"),
                  tags$div(
                    tags$ul(
                      tags$li("Added welcome popup message and message about known issue once this app is accessed"),
                      tags$li("Updated layout (app.R and tabLog.R)"),
                      tags$li("Updated structure to address issue of timezone in shinyapps.io server (UTC and BST)"),
                      tags$li("Added timetable of 14a and 14"),
                      tags$li("Updated Info tab for four different colors in a valueBox")
                    )
                  ),
                  h2("v0.2 - 2024.07.30"),
                  tags$div(
                    tags$ul(
                      tags$li("Updated layout with narbarPage instead of dashboardPage"),
                      tags$li("Updated inputs with VirtualSelectInput() instead of SelectInput()"),
                      tags$li("Updated Info tab for reporting any issues found")
                    )
                  ),
                  h2("v0.1 - 2024.07.29"),
                  tags$div(
                    tags$ul(
                      tags$li("Initial commit with timetable of 12x and 11"),
                    )
                  )
                  )