library('stringr')
library('httr')
library('RCurl')
library('XML')
library('RJSONIO')

# API_KEY for timezonedb.com (free).
TZDB_API_KEY="VHAD5XMMMDQK"
# API_KEY for Google Geocoding (free).
G_API_KEY="AIzaSyD8b0PZzbiYHjXe9f7LyzQLt5VgO-WSBwg"

# HELPER FUNCTIONS
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
    x['ZONENAMES'] = "Hawaii-Aleutian Time"
  } else if (x['TIME_ZONE']=="MDT") {
    x['ZONENAMES'] = "America/Denver"
  } else if (x['TIME_ZONE']=="MST") {
    x['ZONENAMES'] = "America/Denver"
  } else if (x['TIME_ZONE']=="PDT") {
    x['ZONENAMES'] = "America/Los_Angeles"
  } else if (x['TIME_ZONE']=="PST") {
    x['ZONENAMES'] = "America/Los_Angeles"
  } else if (x['TIME_ZONE']=="SCT") {
    x['ZONENAMES'] = "Seychelles Time"
  } else if (x['TIME_ZONE']=="SST") {
    x['ZONENAMES'] = "Samoa Standard Time"
  } else if (x['TIME_ZONE']=="UTC") {
    x['ZONENAMES'] = "UTC"
  } else {
    x['ZONENAMES'] = timezonedb(x)
  }
}

timezonedb <- function(x) {
  if (str_trim(x['LATITUDE']) == 0 && str_trim(x['LONGITUDE']) == 0) {
    url = sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s", str_trim(x['STATE']), G_API_KEY)
    print(paste("Getting response from: ", url))
    response <- fromJSON(url)
    results = response$results
    results = results[[1]]
    
    x['LATITUDE'] <- as.numeric(results$geometry$location['lat'])*100
    x['LONGITUDE'] <- as.numeric(results$geometry$location['lng'])*100
    print(sprintf("Received response with lat: %f and lng:%f", x['LATITUDE'], x['LONGITUDE']))
  }
  url = sprintf("http://api.timezonedb.com/?lat=%f&lng=-%f&time=%d&key=%s", as.numeric(x['LATITUDE'])/100, as.numeric(x['LONGITUDE'])/100, gettimestamp(x), TZDB_API_KEY)
  print(paste("Getting response from timezonedb using: ", url))
  response = getURL(url)
  
  if (length(response) > 0 && grep('<status>OK</status>', response[1]) == 1) {
    response = tryCatch(xmlTreeParse(response, useInternalNodes = TRUE), error = function(e) { print(response) })
    
    x['ZONENAMES'] = xmlValue(xmlChildren(xmlChildren(response)$result)$zoneName)
  } else {
    print('Bad response from timezonedb: ' + response)
  }
}

gettimestamp <- function(x) {
  datetime = paste(str_split(str_trim(x['BGN_DATE']), ' ', 2)[[1]][1], str_trim(x['BGN_TIME']), sep = ' ')
  # Just getting the UNIX timestamp, timezone doesn't really matter here.
  timestamp = strptime(datetime, format = "%m/%d/%Y %H%M", tz="GMT")
  as.numeric(timestamp)
}

createbegintimestamps <- function(x) {
  if (str_trim(x['BGN_DATE']) != '') {
    datetime = paste(str_split(str_trim(x['BGN_DATE']), ' ', 2)[[1]][1], str_trim(x['BGN_TIME']), sep = ' ')
    x['BGN_TIMESTAMP'] = strptime(datetime, format = "%m/%d/%Y %H%M", tz=str_trim(x['ZONENAMES']))
  }
}

createendtimestamps <- function(x) {
  if (str_trim(x['END_DATE']) != '') {
    datetime = paste(str_split(str_trim(x['END_DATE']), ' ', 2)[[1]][1], str_trim(x['END_TIME']), sep = ' ')
    x['END_TIMESTAMP'] = strptime(datetime, format = "%m/%d/%Y %H%M", tz=str_trim(x['ZONENAMES']))
  }
}
# BEGIN PROCESSING

# Download (if necessary) and load data.
dir.create(file.path('.', 'data'), showWarnings = FALSE)

if(!file.exists('data/repdata-data-StormData.csv.bz2')) {
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', './data/repdata-data-StormData.csv.bz2')
}

readsizeof = 20000
numrecords = length(read.csv(bzfile('./data/repdata-data-StormData.csv.bz2'))[,1])
columnnames = c("STATE__", "BGN_DATE", "BGN_TIME", "TIME_ZONE", "COUNTY", "COUNTYNAME", "STATE", "EVTYPE", "BGN_RANGE", "BGN_AZI", "BGN_LOCATI", "END_DATE", "END_TIME", "COUNTY_END", "COUNTYENDN", "END_RANGE", "END_AZI", "END_LOCATI", "LENGTH", "WIDTH", "F", "MAG", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "WFO", "STATEOFFIC", "ZONENAMES", "LATITUDE", "LONGITUDE", "LATITUDE_E", "LONGITUDE_", "REMARKS", "REFNUM")
classes = c("numeric", "character", "character", "character", "numeric", "character", "character", "character", "numeric", "character", "character", "character", "character", "numeric", "logical", "numeric", "character", "character", "numeric", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric")
processeddata = data.frame()

for (skip in seq(0,numrecords+readsizeof, by=readsizeof)) {
  tmpdata = read.csv(bzfile('./data/repdata-data-StormData.csv.bz2'), skip = skip, nrows = readsizeof, colClasses = classes, col.names = columnnames)
  zonenames = apply(tmpdata, 1, tzconv)
  tmpdata$ZONENAMES = zonenames
  tmpdata$BGN_TIMESTAMP = apply(tmpdata, 1, createbegintimestamps)
  tmpdata$END_TIMESTAMP = apply(tmpdata, 1, createendtimestamps)
  processeddata = rbind.data.frame(processeddata, tmpdata)
  print("end loop")
}
