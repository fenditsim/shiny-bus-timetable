library(pdftools)
library(tidyverse)

## Configurations for shinyapps.io server
# System server (shinyapps.io) uses UTC instead of British Summer Time (BST); the updated frequency does not match
days.range <- seq(as.Date(paste0(format(Sys.Date(),"%Y"), "-03-24")), as.Date(paste0(format(Sys.Date(),"%Y"), "-11-08")), by = "day") # Range of days for BST this year
bst.min <- max(days.range[weekdays(days.range)=="Sunday" & as.numeric(format(days.range, "%m")) == 03 & as.numeric(format(days.range, "%d")) >= 24])  # First day of BST this year
bst.max <- min(days.range[weekdays(days.range)=="Sunday" & as.numeric(format(days.range, "%m")) == 10 & as.numeric(format(days.range, "%d")) >= 24])  # Last day of BST this year
time.switch <- if (Sys.Date() >= bst.min & Sys.Date() <= bst.max) TRUE else FALSE # Check if today is within BST or not
# time.switch <- FALSE # Uncomment this line when debugging

## Download Data
# Download bus timetable (pdf) from official site
for (bus in c('12x', '11', '14', '14a')) assign(paste0('ttb.', bus), 
                                                map(paste0("https://bustimetables-pdf.utrackapps.com/generateDaily.php?origin=Canley,%20Prior%20Deram%20Walk&dest=Coventry,%20Pool%20Meadow&privateCode=CVAO0", bus,"&departDate=", Sys.Date()), pdf_text)
                                                )

# Convert pdf into r dataframe
uni.to.cov <- str_split(ttb.12x[[1]][1], "\n") %>% unlist() # University of Warwick to Coventry
cov.to.uni <- str_split(ttb.12x[[1]][2], "\n") %>% unlist() # Coventry to University of Warwick

lps.to.cov <- str_split(ttb.11[[1]][1], "\n") %>% unlist() # LMS to Coventry
cov.to.lps <- str_split(ttb.11[[1]][2], "\n") %>% unlist() # Coventry to LMS

cov.to.uni.14a <- str_split(ttb.14a[[1]][1], "\n") %>% unlist() # Coventry to University of Warwick

uni.to.cov.14 <- str_split(ttb.14[[1]][1], "\n") %>% unlist() # University of Warwick to Coventry
cov.to.uni.14 <- str_split(ttb.14[[1]][2], "\n") %>% unlist() # Coventry to University of Warwick


# Bus stops
stops.12x <- c("Pool Meadow Bus Station"="Coventry Pool Meadow", 
               #"The Quadrant"="Quadrant",
               "Rail Station Bridge"="Coventry Station Warwick Road", 
               "University of Warwick Bus Interchange"="University of Warwick Bus Int", 
               "Canley Prior Deram Walk"="Canley Prior Deram Walk"
               )

stops.11 <- c("High Street, Leamington Spa"="Leamington Spa High Street",
              "Upper Parade, Leamington Spa"="Leamington Spa Upper Parade",
              "Kenilworth Clock"="Kenilworth Clock",
              "University of Warwick Bus Interchange"="University of Warwick Bus Int",
              "Earlsdon City Arms"="Earlsdon City Arms",
              "Coventry Rail Station"="Coventry Station Interchange",
              "Pool Meadow Bus Station"="Coventry Pool Meadow"
              )

stops.14a <- c("Pool Meadow Bus Station"="Coventry Pool Meadow",
               "Maudslay Road The Maudslay" = "Maudslay Road The Maudslay",
               "Eastern Green Farcroft Avenue" = "Eastern Green Farcroft Avenue",
               "Torrington Ave" = "Torrington Ave",
               "University of Warwick Bus Interchange"="University of Warwick Bus Int"
               )

stops.14 <- c("Pool Meadow Bus Station"="Coventry Pool Meadow",
              "Maudslay Road The Maudslay" = "Maudslay Road The Maudslay",
              "Eastern Green Farcroft Avenue" = "Eastern Green Farcroft Avenue",
              "Torrington Ave" = "Torrington Ave",
              "Tile Hill Rail Station" = "Tile Hill Rail Station",
              "Allesley Old Road / Mount St" = "Allesley Old Road / Mount St",
              "Cannon Park Shops" = "Cannon Park Shops",
              "University of Warwick Bus Interchange"="University of Warwick Bus Int"
              )

stops <- c("Pool Meadow Bus Station"="Coventry Pool Meadow", 
           #"The Quadrant"="Quadrant",
           "Coventry Rail Station"="Coventry Station Interchange",
           "Rail Station Bridge"="Coventry Station Warwick Road",
           "Earlsdon City Arms"="Earlsdon City Arms",
           "University of Warwick Bus Interchange"="University of Warwick Bus Int", 
           "Kenilworth Clock"="Kenilworth Clock", 
           "Upper Parade, Leamington Spa"="Leamington Spa Upper Parade",
           "High Street, Leamington Spa"="Leamington Spa High Street",
           "Canley Prior Deram Walk"="Canley Prior Deram Walk",
           "Maudslay Road The Maudslay" = "Maudslay Road The Maudslay",
           "Eastern Green Farcroft Avenue" = "Eastern Green Farcroft Avenue",
           "Torrington Ave" = "Torrington Ave",
           "Tile Hill Rail Station" = "Tile Hill Rail Station",
           "Allesley Old Road / Mount St" = "Allesley Old Road / Mount St",
           "Cannon Park Shops" = "Cannon Park Shops"
           )

# Convert each page of downloaded pdf
# 12x
uni.to.cov <- uni.to.cov[grep(paste(stops.12x[stops.12x != "Quadrant"], collapse="|"), uni.to.cov)]
cov.to.uni <- cov.to.uni[grep(paste(stops.12x, collapse="|"), cov.to.uni)]

# 11
lps.to.cov <- lps.to.cov[grep(paste(stops.11, collapse="|"), lps.to.cov)]
cov.to.lps <- cov.to.lps[grep(paste(stops.11, collapse="|"), cov.to.lps)]

# 14a
cov.to.uni.14a <- cov.to.uni.14a[grep(paste(stops.14a, collapse="|"), cov.to.uni.14a)]

# 14
uni.to.cov.14 <- uni.to.cov.14[grep(paste(stops.14, collapse="|"), uni.to.cov.14)]
cov.to.uni.14 <- cov.to.uni.14[grep(paste(stops.14, collapse="|"), cov.to.uni.14)]

# Convert the downloaded pdf into a readable dataframe object
transform <- function(data, stop){
  ttb <- data[grep(stop, data)] %>% 
    str_remove(stop) %>% 
    paste(collapse = "") %>% 
    str_split(pattern = " ") %>% unlist() %>%
    str_remove(pattern = "0000")
  return(ttb[which(ttb!="")])
}

timetable <- list() # An empty list that store timeslots for each journey
# 12x
for (stop in stops.12x){
  timetable[[if (stop != "Canley Prior Deram Walk") paste0("University of Warwick Bus Int_", stop, "_12x") else paste0(stop, "_Uni_12x")]] <- transform(data = uni.to.cov, stop=stop)
  timetable[[if (stop != "Canley Prior Deram Walk") paste0(stop, "_Uni_12x") else paste0("University of Warwick Bus Int_Canley Prior Deram Walk_12x")]] <- transform(data = cov.to.uni, stop=stop)
}

# 11
for (stop in stops.11){
  timetable[[if (!stop %in% c("Leamington Spa High Street", "Leamington Spa Upper Parade", "Kenilworth Clock")) paste0("University of Warwick Bus Int_", stop, "_11") else paste0(stop, "_Uni_11")]] <- transform(data = lps.to.cov, stop=stop)
  timetable[[if (!stop %in% c("Leamington Spa High Street", "Leamington Spa Upper Parade", "Kenilworth Clock")) paste0(stop, "_Uni_11") else paste0("University of Warwick Bus Int_", stop, "_11")]] <- transform(data = cov.to.lps, stop=stop)
}

# 14a
for (stop in stops.14a){ timetable[[paste0(stop, "_Uni_14a")]] <- transform(data = cov.to.uni.14a, stop=stop) }

# 14
for (stop in stops.14){
  timetable[[paste0(stop, "_Uni_14")]] <- transform(data = cov.to.uni.14, stop=stop)
  timetable[[paste0("University of Warwick Bus Int_", if (stop == "Eastern Green Farcoft Avenue") "Eastern Green Farcroft Avenue" else stop, "_14")]] <- transform(data = uni.to.cov.14, stop=stop)
}

# Update timeslots of 14 from Coventry to Uni 
# Redo the cleaning for Uni with different filters (pattern of str_split has a 'larger' empty pattern)
uni <- cov.to.uni.14[grep('University of Warwick Bus Int', cov.to.uni.14)] %>% str_remove('University of Warwick Bus Int') %>% paste(collapse = "") %>% str_split(pattern = "      ") %>% unlist()
uni <- uni[!uni==""]  # Remove empty elements
uni <- uni[-grep(pattern = "0000", uni)]  # Remove elements with 0000
uni <- as.vector(rbind(" NA", uni)) # Add ' NA' in between elements
uni <- uni[-1]  # Remove the first element
uni <- uni %>% str_split(pattern = " ") %>% unlist()  # Split uni with " "
uni <- uni[!uni==""]  # Remove empty elements 
uni[uni=="NA"] <- ""  # Replace NA with ""
timetable$`University of Warwick Bus Int_Uni_14` <- uni # Put the update vector within timetable

timetable$`Coventry Pool Meadow_Uni_14` <- timetable$`Coventry Pool Meadow_Uni_14`[1:length(uni)] # Ensure the vector (Cov to Uni) has the same length with that uni_uni vector
timetable$`Coventry Pool Meadow_Uni_14` <- timetable$`Coventry Pool Meadow_Uni_14`[!timetable$`University of Warwick Bus Int_Uni_14`==""] # Remove unnecessary timeslots

timetable$`Maudslay Road The Maudslay_Uni_14` <- timetable$`Maudslay Road The Maudslay_Uni_14`[1:length(uni)] # Ensure the vector (Cov to Uni) has the same length with that uni_uni vector
timetable$`Maudslay Road The Maudslay_Uni_14` <- timetable$`Maudslay Road The Maudslay_Uni_14`[!timetable$`University of Warwick Bus Int_Uni_14`==""] # Remove unnecessary timeslots

timetable$`Eastern Green Farcroft Avenue_Uni_14` <- timetable$`Eastern Green Farcroft Avenue_Uni_14`[1:length(uni)] # Ensure the vector (Cov to Uni) has the same length with that uni_uni vector
timetable$`Eastern Green Farcroft Avenue_Uni_14` <- timetable$`Eastern Green Farcroft Avenue_Uni_14`[!timetable$`University of Warwick Bus Int_Uni_14`==""] # Remove unnecessary timeslots

timetable$`University of Warwick Bus Int_Uni_14` <- timetable$`University of Warwick Bus Int_Uni_14`[!timetable$`University of Warwick Bus Int_Uni_14`==""] # Remove empty timeslots


# Check when the time of latest bus arrival is
timecheck.12x <- function(origin="Coventry Pool Meadow", dest="Uni", bus="12x", min=F, DT = F){
  check <- paste(origin, dest, bus, sep = "_")
  timeslot <- data.frame(format(as.POSIXct(timetable[[check]], format="%H%M"), format="%H:%M"))
  times <- as.POSIXct(timetable[[check]], format="%H%M")
  
  if (origin == "University of Warwick Bus Int" & dest == "Canley Prior Deram Walk"){
    tmp <- length(as.POSIXct(timetable[['University of Warwick Bus Int_Canley Prior Deram Walk_12x']], format="%H%M"))
    timeslot <- tail(data.frame(format(as.POSIXct(timetable[['University of Warwick Bus Int_Uni_12x']], format="%H%M"), format="%H:%M")), tmp)
    
    if (DT){
      interval <- data.frame("Time" = c(tail(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                             "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                           as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                             )
      )
    }else if (min){
      interval <- as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")][1], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
    }else{
      interval <- format(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")][1], format="%H:%M")
    }
  }else{
    if (DT){
      interval <- data.frame("Time" = c(tail(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                             "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                           as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                             )
      )
    }else if (min){
      interval <- as.numeric(round(difftime(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M"), units='mins'),0))
    }else{
      interval <- format(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], format="%H:%M")
    }
  }
  return(interval)
}

# 11
timecheck.11 <- function(origin="Coventry Pool Meadow", dest="Uni", bus="11", min=F, DT = F){
  check <- paste(origin, dest, bus, sep = "_")
  timeslot <- data.frame(format(as.POSIXct(timetable[[check]], format="%H%M"), format="%H:%M"))
  times <- as.POSIXct(timetable[[check]], format="%H%M")
  
  if (origin == "University of Warwick Bus Int" & dest %in% c("Kenilworth Clock", "Leamington Spa Upper Parade", "Leamington Spa High Street")){
    tmp <- length(as.POSIXct(timetable[['University of Warwick Bus Int_Kenilworth Clock_11']], format="%H%M"))
    timeslot <- tail(data.frame(format(as.POSIXct(timetable[['University of Warwick Bus Int_Uni_11']], format="%H%M"), format="%H:%M")), tmp)
    
    if (DT){
      interval <- data.frame("Time" = c(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                             "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                           as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                             )
      )
    }else if (min){
      interval <- as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")][1], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
    }else{
      interval <- format(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")][1], format="%H:%M")
    }
  }else{
    if (DT){
      interval <- data.frame("Time" = c(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                             "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                           as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                             )
      )
    }else if (min){
      interval <- as.numeric(round(difftime(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M"), units='mins'),0))
    }else{
      interval <- format(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], format="%H:%M")
    }
  }
  return(interval)
}

# 14a
timecheck.14a <- function(origin="Coventry Pool Meadow", dest="Uni", bus="14a", min=F, DT = F){
  check <- paste(origin, dest, bus, sep = "_")
  timeslot <- data.frame(format(as.POSIXct(timetable[[check]], format="%H%M"), format="%H:%M"))
  times <- as.POSIXct(timetable[[check]], format="%H%M")
  
  if (DT){
    interval <- data.frame("Time" = c(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                           "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                         as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                                         )
                           )
  }else if (min){
    interval <- as.numeric(round(difftime(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M"), units='mins'),0))
  }else{
    interval <- format(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], format="%H:%M")
  }
  return(interval)
}

# 14
timecheck.14 <- function(origin="Coventry Pool Meadow", dest="Uni", bus="14", min=F, DT = F){
  check <- paste(origin, dest, bus, sep = "_")
  timeslot <- data.frame(format(as.POSIXct(timetable[[check]], format="%H%M"), format="%H:%M"))
  times <- as.POSIXct(timetable[[check]], format="%H%M")
  
  if (origin == "University of Warwick Bus Int"){
    #tmp <- length(as.POSIXct(timetable[['University of Warwick Bus Int_Kenilworth Clock_11']], format="%H%M"))
    #timeslot <- tail(data.frame(format(as.POSIXct(timetable[['University of Warwick Bus Int_Uni_11']], format="%H%M"), format="%H:%M")), tmp)
    
    if (DT){
      interval <- data.frame("Time" = c(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                             "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                           as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                             )
      )
    }else if (min){
      interval <- as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")][1], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
    }else{
      interval <- format(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")][1], format="%H:%M")
    }
  }else{
    if (DT){
      interval <- data.frame("Time" = c(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]),
                             "Minutes" = c(as.numeric(round(difftime(as.POSIXct(tail(sort(timeslot[timeslot < format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")]), 1), format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0)),
                                           as.numeric(round(difftime(as.POSIXct(timeslot[timeslot >= format(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M")], format = "%H:%M"), as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H:%M"), units = "mins"),0))
                             )
      )
    }else if (min){
      interval <- as.numeric(round(difftime(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M"), units='mins'),0))
    }else{
      interval <- format(times[times >= as.POSIXct(Sys.time() + if (time.switch) 3630 else 0, format = "%H%M")][1], format="%H:%M")
    }
  }
  return(interval)
}
