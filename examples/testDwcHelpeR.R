## get working directory
getwd()

## read data file
ss = fread("../examples/surveySites.csv")
## make names like make bed
setnames(ss, make.names(colnames(ss)))

## hmm.. comfortable~
colnames(ss)

## try auto map
map = colMapping(colnames(ss))
## the result REALLY sucks
map

describeDwc('saline')
similarDwcTerms('saline')

## Still sucks
similarDwcTerms('SITECODE')$terms
similarDwcTerms('SITECODE', v = 'Event')$terms

## OK result
similarDwcTerms('SITE.CODE')$terms
similarDwcTerms('SITE.CODE', v = 'Event')$terms

## Some Test
similarTest()


## sep field name
setnames(ss, c('SITE.CODE',	'SITE',	'SITE.LAT',	'SITE.LONG',	'COUNTRY', 	'ECO.REGION',	'SURVEY.DATE',	'SURVEY.ID',	'DEPTH',	'WATER.TEMPERATURE.F',	'pH',	'SALINITY.parts.per.1000'))

##
map_better = betterColMapping(colnames(ss), vocab = 'Event')
write.table(x = map_better, file = "../examples/colMap.csv", sep="\t", row.names = F, quote = F)





