pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

	# Initialise Data Frame
	wholeSample <- data.frame(Date=c(NA), sulfate=c(NA), nitrate=c(NA), ID=c(NA))

	# Iterate through monitor ID's reading in data
	for (monitorID in id) {
		# Build full file name
		filename <- paste(sprintf("%03d", monitorID), "csv", sep=".")
		filepath <- paste(directory, filename, sep="/")

		# read data
		thisData <- read.csv(filepath)

		# append data to main data set
		tempDF <- rbind(wholeSample, thisData)
		wholeSample <- tempDF
	}

	summary(wholeSample)

	# Now assign relevant column to working variable
	if (pollutant == "nitrate") {
		pollutantSample = wholeSample$nitrate
	} else if (pollutant == "sulfate") {
		pollutantSample = wholeSample$sulfate
	} else {
		stop("Invalid Pollutant Specified")
	}

	# Calculate Mean, expluding NAs
	mean(pollutantSample, na.rm=TRUE)

}

complete <- function(directory, monId = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	# Initialise Data Frame for results
	resultsDF <- data.frame(id = numeric(), nobs = numeric())

	# Iterate through monitor ID's reading in data
	for (monitorID in monId) {
		# Build full file name
		filename <- paste(sprintf("%03d", monitorID), "csv", sep=".")
		filepath <- paste(directory, filename, sep="/")

		# read data
		thisData <- read.csv(filepath)

		completeCases <- sum(as.numeric(complete.cases(thisData)))

		newRow <- data.frame(id = monitorID, nobs = completeCases)

		tempDF <- rbind(resultsDF, newRow)
		resultsDF <- tempDF

	}

	resultsDF

}

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

	# Initialise output
	corVector <- numeric()

	# Get the list of files in the directory
	listFiles <- list.files(directory)

	# Iterate through the files 
	for (monitorFile in listFiles) {
		# Build full file name
		filepath <- paste(directory, monitorFile, sep="/")

		# read data
		thisData <- read.csv(filepath)

		thisCount <- sum(as.numeric(complete.cases(thisData)))

		if (thisCount >= threshold) {
			goodData <- thisData[complete.cases(thisData),]
			thisCor <- cor(goodData$nitrate, goodData$sulfate)
			corVector <- append(corVector, thisCor)
		}

	}

	corVector

}

