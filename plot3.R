##
## This file contains the functions to plot graph 3 in project 1 for 
## Exploratory Data Analysis.
##

## Default value of number of rows to skip and total number of rows to include 
## only dates 2007-02-01 and 2007-02-02. Function "findSkipAndRows" is included 
## in this file for how to find these parameters.

NumSkip = 66636
NumRows = 2880


## Function to plot graph 3

plot3 <- function(inputFile = "household_power_consumption.txt",
                  nSkip = NumSkip, 
                  nRowsInclude = NumRows) {
    
    ## Input 
    ##      'inputFile' is the input file name
    ##      'nSkip' is the number of rows to skip in reading the file
    ##      'nRowsInclude' is the number of rows to include
    
    # Read data
    dat <- readData(inputFile, nSkip, nRowsInclude)
    
    # Plot and save the graph in PNG format
    png("plot3.png")
    
    # Plot sub metering 1. Add lines for sub metering 2 and 3, and the legend. 
    plot(dat$DateTime,
         dat$Sub_metering_1, 
         type = "l",
         col = "black",
         xlab = "",
         ylab = "Energy sub metering")
    
    lines(dat$DateTime, 
          dat$Sub_metering_2, 
          col = "red")
    
    lines(dat$DateTime, 
          dat$Sub_metering_3, 
          col = "blue")
    
    legend("topright", 
           lty = 1,
           col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.off()
}


## Function read data from the input file

readData <- function(inputFile, nSkip, nRowsToSkip){
    
    ## Input 
    ##      'inputFile' is the input file name
    ##      'nSkip' is the number of rows to skip in reading the file
    ##      'nRowsInclude' is the number of rows to include
    ##
    ## Output
    ##      Returns the data frame with the corresponding rows specified
    
    # Check if input file exists
    if(!file.exists(inputFile)){
        stop("Input file does not exist.")
    }
    
    # Read first row to get the title
    title <- read.table(inputFile, 
                        sep = ";", 
                        nrows = 1, 
                        colClasses = "character")
    
    # Read subset only based on global variable NumSkip and NumRows
    colClass <- c("character", "character", rep("numeric", 7))
    dat <- read.table(inputFile, 
                      sep = ";",
                      na.strings = "?",
                      skip = nSkip,
                      nrows = nRowsToSkip,
                      col.names = title,
                      colClasses = colClass)
    
    # Create a POSIXlt column using the date and time column, as the format of 
    # those column cannot be coerce readily into R time format.
    dat$DateTime <- strptime(paste(dat$Date, dat$Time, " "), "%d/%m/%Y %T")
    
    dat
}


## Function to find the number of rows to skip and the total number of rows 
## to read from the input, given the start date and end date (inclusive).

findSkipAndRows <- function(inputFile = "household_power_consumption.txt",
                            startDate = "1/2/2007",
                            endDate = "2/2/2007"){
    
    ## Input 
    ##      'inputFile' is the input file name
    ##      'startDate' is the start date to include 
    ##      'endDate' is the end date to include
    ##
    ## Output
    ##      Sets the global variables NumSkip as the number of rows to skip
    ##      and NumRows the total number of rows to read. Also returns these 
    ##      as a list.
    
    # Check if input file exists
    if(!file.exists(inputFile)){
        stop("Input file does not exist.")
    }
    
    # Read all data    
    colClass <- c("character", "character", rep("numeric", 7))
    dat <- read.table(inputFile, 
                      header = TRUE, 
                      sep = ";",
                      na.strings = "?",
                      colClasses = colClass)
    
    # startRow is the first row start date is found
    # endRow is the last row end date is found
    startRow <- min(which(dat$Date == startDate))
    endRow <- max(which(dat$Date == endDate))
    
    # First row start date is found needs to be included, i.e. number of rows 
    # to skip is startRow - 1
    NumSkip <<- startRow - 1
    NumRows <<- endRow - startRow + 1
    
    invisible(c(skip = NumSkip, nRows = NumRows))
}
