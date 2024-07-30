tabLog <- tabItem(tabName = "log",
                  h1("Version Log"),
                  h2("v0.2 - 2024.07.30"),
                  tags$div(
                    tags$ul(
                      tags$li("Updated layout with narbarPage instead of dashboardPage"),
                      tags$li("Updated inputs with VirtualSelectInput() instead of SelectInput()"),
                      tags$li("Updated Info tab for reporting any issues found")
                    )
                  ),
                  h2("v0.1 - 2024.07.29"),
                  p("Initial commit with timetable of 12x and 11.")
                  )