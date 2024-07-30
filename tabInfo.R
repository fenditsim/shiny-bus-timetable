tabInfo <- tabItem(tabName = "info",
                   h1("About"),
                   p("© 2024", a(href = "https://github.com/fenditsim", "Fendi Tsim")," | Built with Shiny"),
                   br(),
                   h2("Remarks"),
                   p("This Shiny app is for illustration purpose only."),
                   p("TimeTable data are extracted from NX Bus West Midlands. It does not produce real-time information."),
                   br(),
                   h2("Features"),
                   p("1. Color of each time box represents how soon the bus arrives:"),
                   tags$ul(
                    tags$li("'red': less than 5 minutes"),
                    tags$li("'yellow': between 5 and 10 minutes"),
                    tags$li("'green': more than 10 minutes")
                    ),
                  p("2. Click the time box show a table of bus arrivals in terms of time and minutes, including the last one (with a minus indicating it's been away)."),
                  p("3. No information available is shown as NA inside a purple box"),
                  br(),
                  h2("Resources"),
                  p("Click ", a(href = "https://github.com/fenditsim/shiny-bus-timetable", "here"), " to the corresponding Github repository."),
                  p("Click ", a(href = "https://github.com/fenditsim/shiny-bus-timetable/issues", "here"), " to report for any issues you find :)"),
                  
                  )