##### Environment Setup
## get working directory
getwd()
library(dwcHelpeR)

## read data file
ss = fread("../examples/surveySites.csv")
## make names like make bed
setnames(ss, make.names(colnames(ss)))

#### Discover and map data columns to DwC standards
## hmm.. comfortable~
colnames(ss)

## try auto map
map = colMapping(colnames(ss))
## the result REALLY sucks
map

## Still sucks
similarDwcTerms('SITELONG')$terms
similarDwcTerms('SITECODE', v = 'Event')$terms

## OK result
similarDwcTerms('SITE.CODE')$terms
similarDwcTerms('SITE.CODE', v = 'Event')$terms

similarDwcTerms('coordSys', v = 'Event')$terms
similarDwcTerms('coordSystem', v = 'Event')$terms

## Some Test
similarTest()
## should output T,F,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,F,T,T,T,T,T,T,F,T,F,T,T,T,T,T,T

## make names better, add seperator between words
setnames(ss, c('SITE.CODE',	'SITE',	'SITE.LAT',	'SITE.LONG',	'COUNTRY', 	'ECO.REGION',	'SURVEY.DATE',	'SURVEY.ID',	'DEPTH',	'WATER.TEMPERATURE.F',	'pH',	'SALINITY.parts.per.1000'))
## or this line will do, too
#setnames(ss, c('siteCode',	'site',	'siteLat',	'siteLong',	'country', 	'ecoRegion',	'surveyDate',	'surveyID',	'depth',	'waterTemperatureF',	'pH',	'salinityPartsPer1000'))

## generate mapping file
map_better = betterColMapping(colnames(ss), vocab = 'Event')
write.table(x = map_better, file = "../examples/colMap_candidates.csv", sep="\t", row.names = F, quote = F)

## open mapping file and verify mapping in excel or other editor
# ...
# ...


## show description of a DwC term
## known
describeDwc('locality')
describeDwc('decimalLongitude')
describeDwc('longitude')
## unknow
describeDwc('coordSys')

## narrow down results
describeDwc('ecoRegion')
describeDwc('ecoRegion', v='Event')
describeDwc('ecoRegion', verbatim = F)
describeDwc('ecoRegion', v='Event', verbatim = F)

## read back modified map
map_verified = getMappingFromFile("../examples/colMap_verified.csv", header = F, na.strings = "")
map_verified

## rename columns
ss = renameSubset(ss, map_dt = map_verified)
names(ss)

## create event uuid for real eventID
ss = createUUID(ss, 'event', c('eventID'))

## current eventID should be?
## see related terms
describeDwc('eventID', v = 'Event')
## should use fieldNumber instead?
describeDwc('fieldNumber', v = 'Event')
setnames(ss, 'eventID', 'fieldNumber')

ssEvent = getDwcTable(ss, getDwcTerms('Event'), eventID = 'event_uuid')

##
eventMeasTpl = makeMeasurementTpl(ss)
write.table(file = "../examples/event_measurementOfFact.tpl.csv", x = eventMeasTpl, sep = "\t", quote = F, row.names = F)

## TODO: complete the function, make it right
# getMeasVars(eventMeasTplFilled)
## before that, we have to pick our own measurement variables
measVars = c('WATER.TEMPERATURE.F', 'pH', 'SALINITY.parts.per.1000')

## read measurement-metadata-filled data
eventMeasTplFilled = fread(input = "../examples/event_measurementOfFact.tpl.filled.csv")

## create measurementOrFact table
eventMeas = makeMeasurement(eventMeasTplFilled, eventID = 'event_uuid', measVars = measVars)

## Create measurement ID (TBD how to do it)
eventMeas = createUUID(eventMeas, 'measurement', c('fieldNumber', 'measurementType'))

