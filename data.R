library(pdftools)
library(tidyverse)

# System server use UTC instead of BST and the updated frequency does not match
time.switch <- TRUE # FALSE for debugging

# Download bus timetable (pdf) from official site
ttb.12x <- map(paste0("https://bustimetables-pdf.utrackapps.com/generateDaily.php?origin=Canley,%20Prior%20Deram%20Walk&dest=Coventry,%20Pool%20Meadow&privateCode=CVAO012X&departDate=", Sys.Date()),
               pdf_text)

ttb.11 <- map(paste0("https://bustimetables-pdf.utrackapps.com/generateDaily.php?origin=Canley,%20Prior%20Deram%20Walk&dest=Coventry,%20Pool%20Meadow&privateCode=CVAO011&departDate=", Sys.Date()),
              pdf_text)

# Convert pdf into r dataframe
uni.to.cov <- str_split(ttb.12x[[1]][1], "\n") %>% unlist() # University of Warwick to Coventry
cov.to.uni <- str_split(ttb.12x[[1]][2], "\n") %>% unlist() # Coventry to University of Warwick

lps.to.cov <- str_split(ttb.11[[1]][1], "\n") %>% unlist() # LMS to Coventry
cov.to.lps <- str_split(ttb.11[[1]][2], "\n") %>% unlist() # Coventry to LMS

# Bus stops
stops.12x <- c("Pool Meadow Bus Station"="Coventry Pool Meadow", 
               #"The Quadrant"="Quadrant",
               "Rail Station Bridge"="Coventry Station Warwick Road", 
               "University of Warwick Bus Interchange"="University of Warwick Bus Int", 
               "Canley Prior Deram Walk"="Canley Prior Deram Walk")

stops.11 <- c("High Street, Leamington Spa"="Leamington Spa High Street",
              "Upper Parade, Leamington Spa"="Leamington Spa Upper Parade",
              "Kenilworth Clock"="Kenilworth Clock",
              "University of Warwick Bus Interchange"="University of Warwick Bus Int",
              "Earlsdon City Arms"="Earlsdon City Arms",
              "Coventry Rail Station"="Coventry Station Interchange",
              "Pool Meadow Bus Station"="Coventry Pool Meadow")

stops <- c("Pool Meadow Bus Station"="Coventry Pool Meadow", 
           #"The Quadrant"="Quadrant",
           "Coventry Rail Station"="Coventry Station Interchange",
           "Rail Station Bridge"="Coventry Station Warwick Road",
           "Earlsdon City Arms"="Earlsdon City Arms",
           "University of Warwick Bus Interchange"="University of Warwick Bus Int", 
           "Kenilworth Clock"="Kenilworth Clock", 
           "Upper Parade, Leamington Spa"="Leamington Spa Upper Parade",
           "High Street, Leamington Spa"="Leamington Spa High Street",
           "Canley Prior Deram Walk"="Canley Prior Deram Walk"
)

# Convert the downloaded pdf into a readable dataframe object
transform <- function(data, stop){
  ttb <- data[grep(stop, data)] %>% 
    str_remove(stop) %>% 
    paste(collapse = "") %>% 
    str_split(pattern = " ") %>% unlist() %>%
    str_remove(pattern = "0000")
  return(ttb[which(ttb!="")])
}

# Convert each page of downloaded pdf
# 12x
uni.to.cov <- uni.to.cov[grep(paste(stops.12x[stops.12x != "Quadrant"], collapse="|"), uni.to.cov)]
cov.to.uni <- cov.to.uni[grep(paste(stops.12x, collapse="|"), cov.to.uni)]

# 11
lps.to.cov <- lps.to.cov[grep(paste(stops.11, collapse="|"), lps.to.cov)]
cov.to.lps <- cov.to.lps[grep(paste(stops.11, collapse="|"), cov.to.lps)]

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
