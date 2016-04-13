library('stringr')
library('httr')
library('RCurl')
library('XML')
library('RJSONIO')

# API_KEY for timezonedb.com (free).
TZDB_API_KEY <<- "VHAD5XMMMDQK"
# API_KEY for Google Geocoding (free).
#G_API_KEY <<- "AIzaSyD8b0PZzbiYHjXe9f7LyzQLt5VgO-WSBwg"
G_API_KEY <<- "AIzaSyB5tftkQwmZ49JKKnS4zkRmsIFvHEWIZxc"

CACHED_GOOGLE_DATA <<- list()
###############################################################################
# HELPER FUNCTIONS
###############################################################################

# Return proper name for the timezone.
tzconv <- function(x) {
  if (x['TIME_ZONE']=="CDT") {
    x['ZONENAMES'] = "America/Chicago"
  } else if (x['TIME_ZONE']=="CST") {
    x['ZONENAMES'] = "America/Chicago"
  } else if (x['TIME_ZONE']=="EDT") {
    x['ZONENAMES'] = "America/New_York"
  } else if (x['TIME_ZONE']=="EST") {
    x['ZONENAMES'] = "America/New_York"
  } else if (x['TIME_ZONE']=="GMT") {
    x['ZONENAMES'] = "GMT"
  } else if (x['TIME_ZONE']=="HST") {
    x['ZONENAMES'] = "Pacific/Honolulu"
  } else if (x['TIME_ZONE']=="MDT") {
    x['ZONENAMES'] = "America/Denver"
  } else if (x['TIME_ZONE']=="MST") {
    x['ZONENAMES'] = "America/Denver"
  } else if (x['TIME_ZONE']=="PDT") {
    x['ZONENAMES'] = "America/Los_Angeles"
  } else if (x['TIME_ZONE']=="PST") {
    x['ZONENAMES'] = "America/Los_Angeles"
  } else if (x['TIME_ZONE']=="UTC") {
    x['ZONENAMES'] = "UTC"
  } else {
    x['ZONENAMES'] = timezonedb(x)
  }
}

# Non-standard US abbreviation or no timezone provided. Let's figure it out.
timezonedb <- function(x) {
  if (as.numeric(x['LATITUDE']) == 0 && as.numeric(x['LONGITUDE']) == 0) {
    url = sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s", str_trim(x['STATE']), G_API_KEY)
    
    results = CACHED_GOOGLE_DATA[[url]]
    
    if (is.null(results)) {
      print('No LAT/LNG data provided. Using Google to fetch data based on state.')
      response = gethttpresponse(url, 3)
      results = response$results
      results = results[[1]]
      
      x['LATITUDE'] = results$geometry$location['lat']*100
      x['LONGITUDE'] = results$geometry$location['lng']*-100
      
      CACHED_GOOGLE_DATA[url] <<- list(url = c(x['LATITUDE'], x['LONGITUDE']))
      print(sprintf("Received response with lat: %f and lng:%f", results$geometry$location['lat'], results$geometry$location['lng']))
    } else {
      print('No LAT/LNG data provided. Using cached Google data based on state.')
      x['LATITUDE'] = results[['LATITUDE']]
      x['LONGITUDE'] = results[['LONGITUDE']]
    }
  }
  url = sprintf("http://api.timezonedb.com/?format=json&lat=%f&lng=%f&time=%.0f&key=%s", as.numeric(x['LATITUDE'])/100, as.numeric(x['LONGITUDE'])/-100, gettimestamp(x), TZDB_API_KEY)
  response = gethttpresponse(url, 3)

  Sys.sleep(1) # I don't like this.
  
  if(response$status == 'OK') {
    x['ZONENAMES'] = response$zoneName
  } else {
    print(paste('Bad response from timezonedb: ', response))
  }
}

gethttpresponse <- function(url, numretries) {
  print(paste("Getting HTTP response from: ", url))
  tries = 0
  response = NA
  while(tries < numretries && is.na(response)) {
    response <- tryCatch({
      fromJSON(getURLContent(url), asText = TRUE)
    },
    error = function(e) {
      print(paste("Error: \n", e, "\nTrying again..."))
      tries = tries + 1
      if(tries == numretries) {
        stop(e)
      }
      Sys.sleep(1)
      return(NA)
    }, 
    warning = function(e) {
      print(paste("Warning: \n", e, "\nTrying again..."))
      tries = tries + 1
      if(tries == numretries) {
        stop(e)
      }
      Sys.sleep(1)
      return(NA)
    }
    )
  }
  return(response)
}

# Convert date time to UNIX-epoch timestamp.
gettimestamp <- function(x) {
  if (str_trim(x['BGN_DATE']) != '') {
    datetime = paste(str_split(str_trim(x['BGN_DATE']), ' ', 2)[[1]][1], str_trim(x['BGN_TIME']), sep = ' ')
    format = getdatetimeformat(x['BGN_DATE'], x['BGN_TIME'])
    
    # Just getting the UNIX timestamp, timezone doesn't really matter here.
    timestamp_begin = strptime(datetime, format = format, tz="GMT")
    
    if(is.na(as.numeric(timestamp_begin))) {
      stop(x)
    }
    
    return(as.numeric(timestamp_begin))
  }
}

getdatetimeformat <- function(date, time) {
  if (str_trim(date) != '') {
    datetime = str_trim(date)
    if (str_trim(time) != '') {
      datetime = paste(str_split(str_trim(date), ' ', 2)[[1]][1], str_trim(time), sep = ' ')
    }
    
    format = "%m/%d/%Y %H%M%S"
    
    timestamp = strptime(datetime, format = format, tz="GMT")
    
    if(is.na(as.numeric(timestamp))) {
      format = "%m/%d/%Y %H:%M:%S"
      timestamp = strptime(datetime, format = format, tz="GMT")
    }
    
    if(is.na(as.numeric(timestamp))) {
      format = "%m/%d/%Y %H%M"
      timestamp = strptime(datetime, format = format, tz="GMT")
    }
    
    if(is.na(as.numeric(timestamp))) {
      format = "%m/%d/%Y %H:%M"
      timestamp = strptime(datetime, format = format, tz="GMT")
    }
    
    if(is.na(as.numeric(timestamp))) {
      format = "%m/%d/%Y %I:%M:%S %p"
      timestamp = strptime(datetime, format = format, tz="GMT")
    }
    
    if(is.na(as.numeric(timestamp))) {
      format = "%m/%d/%Y %I:%M %p"
      timestamp = strptime(datetime, format = format, tz="GMT")
    }
  } else {
    stop(paste('Date: ', date, ' Time: ', time))
  }
  return(format)
}

# Convert dates and times to POSIXct timestamps
createbegintimestamps <- function(x) {
  if (str_trim(x['BGN_DATE']) != '') {
    datetime = paste(str_split(str_trim(x['BGN_DATE']), ' ', 2)[[1]][1], str_trim(x['BGN_TIME']), sep = ' ')
    format = getdatetimeformat(x['BGN_DATE'], x['BGN_TIME'])
    
    x['BGN_TIMESTAMP'] = as.POSIXct(strptime(datetime, format = format, tz=str_trim(x['ZONENAMES'])))
  } else {
    x['BGN_TIMESTAMP'] = NA
  }
  return(x['BGN_TIMESTAMP'])
}

# Convert dates and times to POSIXct timestamps
createendtimestamps <- function(x) {
  if (str_trim(x['END_DATE']) != '') {
    datetime = paste(str_split(str_trim(x['END_DATE']), ' ', 2)[[1]][1], str_trim(x['END_TIME']), sep = ' ')
    format = getdatetimeformat(x['END_DATE'], x['END_TIME'])
    
    x['END_TIMESTAMP'] = as.POSIXct(strptime(datetime, format = "%m/%d/%Y %H%M", tz=str_trim(x['ZONENAMES'])), origin = '1970-01-01')
  } else {
    x['END_TIMESTAMP'] = NA
  }
  return(x['END_TIMESTAMP'])
}

###############################################################################
# BEGIN PROCESSING
###############################################################################

# Download (if necessary) and load data.
dir.create(file.path('.', 'data'), showWarnings = FALSE)

if(!file.exists('data/repdata-data-StormData.csv.bz2')) {
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', './data/repdata-data-StormData.csv.bz2')
  # TODO: FIX
  unzip('./data/repdata-data-StormData.csv.bz2', exdir = './data/', unzip = 'bzip2')
}

columnnames = c("STATE__", "BGN_DATE", "BGN_TIME", "TIME_ZONE", "COUNTY", "COUNTYNAME", "STATE", "EVTYPE", "BGN_RANGE", "BGN_AZI", "BGN_LOCATI", "END_DATE", "END_TIME", "COUNTY_END", "COUNTYENDN", "END_RANGE", "END_AZI", "END_LOCATI", "LENGTH", "WIDTH", "F", "MAG", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "WFO", "STATEOFFIC", "ZONENAMES", "LATITUDE", "LONGITUDE", "LATITUDE_E", "LONGITUDE_", "REMARKS", "REFNUM")
classes = c("numeric", "character", "character", "character", "numeric", "character", "character", "character", "numeric", "character", "character", "character", "character", "numeric", "logical", "numeric", "character", "character", "numeric", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric")
processeddata = data.frame()

initprocesseddata <- function(x) {
  # Replace known bad values
  x$BGN_TIME <- gsub('O', '0', x$BGN_TIME)
  x$END_TIME <- gsub('O', '0', x$END_TIME)
  
  # Normalize timestamps so data can be compared.
  
  x[c("BGN_TIMESTAMP", "END_TIMESTAMP")] <- NA
  x$BGN_TIMESTAMP <- as.POSIXct(x$BGN_TIMESTAMP)
  x$END_TIMESTAMP <- as.POSIXct(x$END_TIMESTAMP)

  x$ZONENAMES = apply(x, 1, tzconv)
  x$BGN_TIMESTAMP = apply(x, 1, createbegintimestamps)
  x$END_TIMESTAMP = apply(x, 1, createendtimestamps)
  
  # Check BGN_TIMESTAMP < END_TIMESTAMP
  
  return(x)
}

rawdata = read.csv('./data/repdata-data-StormData.csv', colClasses = classes, col.names = columnnames)
numrecords = length(rawdata[,1])
readsizeof = 20000
offset = 0

for (begin in seq(1, numrecords, by=20000)) {
  end = begin + readsizeof - 1
  
  tmpdata = initprocesseddata(rawdata[begin:end, ])
  processeddata = rbind(processeddata, tmpdata)
  remove(tmpdata)
  offset = offset + 20000
}

processeddata$EVTYPE = toupper(processeddata$EVTYPE)
processeddata$EVTYPE = as.factor(processeddata$EVTYPE)

processeddata$PROPDMGEXP = toupper(processeddata$PROPDMGEXP)
processeddata$PROPDMGEXP = as.factor(processeddata$PROPDMGEXP)

processeddata$CROPDMGEXP = toupper(processeddata$CROPDMGEXP)
processeddata$CROPDMGEXP = as.factor(processeddata$CROPDMGEXP)

###############################################################################
# Answer Questions
###############################################################################

# Across the United States, which types of events (as indicated in the 𝙴𝚅𝚃𝚈𝙿𝙴 variable) are most harmful with respect to population health?

tapply(processeddata$FATALITIES, processeddata$EVTYPE, sum)
tapply(processeddata$INJURIES, processeddata$EVTYPE, sum)

tapply(processeddata$FATALITIES, processeddata$EVTYPE, mean)
tapply(processeddata$INJURIES, processeddata$EVTYPE, mean)

# Across the United States, which types of events have the greatest economic consequences?

tapply(processeddata$CROPDMGEXP, processeddata$EVTYPE, sum)
tapply(processeddata$PROPDMGEXP, processeddata$EVTYPE, sum)

tapply(processeddata$CROPDMGEXP, processeddata$EVTYPE, mean)
tapply(processeddata$PROPDMGEXP, processeddata$EVTYPE, mean)