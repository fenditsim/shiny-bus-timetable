tabInfo <- tabItem(tabName = "info",
                   h1("About"),
                   p("© 2024", a(href = "https://github.com/fenditsim", "Fendi Tsim")," | Built with Shiny"),
                   br(),
                   h2("Remarks"),
                   p("This Shiny app is for illustration purpose only."),
                   p("TimeTable data are extracted from ", a(href = "https://nxbus.co.uk/coventry/services-timetables", "NX Bus Coventry"), " website. It does not produce real-time information."),
                   br(),
                   h2("Features"),
                   p("1. Color of each time box represents how soon the bus arrives:"),
                   tags$ul(
                    tags$li(span("red", style="color:red"), ": less than 5 minutes"),
                    tags$li(span("yellow", style="color:yellow"), ": between 5 and 10 minutes"),
                    tags$li(span("green", style="color:green"), ": more than 10 minutes"),
                    tags$li(span("purple", style="color:purple"), ": No information available is shown as NA (Not Applicable).")
                    
                    ),
                  p("2. Click the time box show a table of bus arrivals in terms of time and minutes, including the last one (with a minus indicating it's been away)."),
                  br(),
                  h2("Resources"),
                  p("Click ", a(href = "https://fenditsim.github.io/projects/building-a-bus-timetable-app-with-shiny/", "here"), " to a relevant in my personal website."),
                  p("Click ", a(href = "https://github.com/fenditsim/shiny-bus-timetable", "here"), " to the corresponding Github repository."),
                  p("Click ", a(href = "https://github.com/fenditsim/shiny-bus-timetable/issues", "here"), " to report for any issues you find :)"),
                  )