# shiny-bus-timetable

## Introduction
This Shiny app is for illustration purpose only.

Timetable data are extracted from NX Bus West Midlands. It does not produce real-time information.

## Features
1. Color of each time box represents how soon the bus arrives or not:
-  ![red](https://placehold.co/15x15/f03c15/f03c15.png) : less than 5 minutes
-  ![yellow](https://placehold.co/15x15/ffff00/ffff00.png) : between 5 and 10 minutes
-  ![green](https://placehold.co/15x15/32cd32/32cd32.png) : more than 10 minutes
-  ![purple](https://placehold.co/15x15/663399/663399.png) : No information available is shown as NA (Not Applicable)

2. Click the time box show a table of bus arrivals in terms of time and minutes, including the last one (with a minus indicating it's been away).
