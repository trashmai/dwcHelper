require(dplyr)
require(data.table)
require(uuid)
require(digest)
require(httr)
require(xml2)
require(stringdist)


## Load gbif core and ext vocab names and urls
loadDwcExtensionList = function(url="http://tools.gbif.org/dwca-validator/extensions.do"){

  # get core and ext link
  dwcExtListPage = GET(url)
  xpath = '//div[@class="definition"]//div[@class="title"]//div[@class="head"]//a'
  x = xml_find_all(read_html(dwcExtListPage), xpath)
  dwc_attr = xml_attr(x, 'href')

  # get core and ext camelCase name
  extname_xpath = '//div[@class="definition"]//div[@class="body"]//div[@class="details"]//table//tr[2]//td'
  x = xml_find_all(read_html(dwcExtListPage), extname_xpath)
  dwc_text = xml_text(x)

  # key-value pairs (name-url pairs)
  assign('dwcExtensionList', setNames(as.list(dwc_attr), dwc_text), envir=.GlobalEnv)

  # cached
  # save(dwcExtensionList, file='dwcExtensionList.RData')
}

#loadDwcExtensionList()

## idea from https://github.com/nvbn/thefuck
fxckYeh = function (dwc_type) {
  dwcTerms = getDwcTerms(dwc_type)
  print('Fxck yeh!')
  if (is.na(dwcTerms)) {
    fxck()
  }
  else {
    dwcTerms
  }
}


## idea from https://github.com/nvbn/thefuck
fxck = function () {
  load("dwcVocabCand.RData")
  getDwcTerms(v)
}

## Load dwc terms from each vocab page and merge them all
getDwcAllTerms = function () {

  # cExtensionList = loadDwcExtensionList()
  if (!exists('dwcExtensionList')) {
    loadDwcExtensionList()
  }
  dwcVocab = names(dwcExtensionList)

  if (!exists('dwcVocabTerms')) {

    if (!exists('terms_dt_list', envir = .GlobalEnv)) {
      terms_dt_list = lapply(dwcVocab, function(v){
        print (paste0("Loading ", v, " from GBIF..."))
        terms = getDwcTerms(v, desc = T)
        terms[, 'vocab' := v]
      })
    }


    assign('dwcVocabTerms',
           unique(Reduce(rbind, terms_dt_list)),
           envir=.GlobalEnv
    )

    #save("tosolr.json", dwcVocabTerms)

  }

}

termInDwc = function (terms, n = NULL) {

  if (!exists('dwcVocabTerms')) {
    getDwcAllTerms()
  }

  vocabs = sapply(terms, function(t) {
    unique(dwcVocabTerms[term==t, 'vocab', with=F])
  })
  if (is.null(n)) {
    unique(unlist(vocabs))
  } else {
    unique(unlist(vocabs))[n]
  }

}


similarTest = function () {

  c(
    "locationID" %in% similarDwcTerms('SITE.CODE')$terms == T,
    "decimalLongitude" %in% similarDwcTerms('SITELONG')$terms == T,
    "decimalLongitude" %in% similarDwcTerms('SITE.LONG')$terms == T,
    "decimalLongitude" %in% similarDwcTerms('SITE.LONG', v='Event')$terms == T,
    "decimalLatitude" %in% similarDwcTerms('SITE.LAT')$terms == T,
    "decimalLongitude" %in% similarDwcTerms('LONG')$terms == T,
    "decimalLongitude" %in% similarDwcTerms('LNG')$terms == T,
    "decimalLatitude" %in% similarDwcTerms('LAT')$terms == T,
    "decimalLongitude" %in% similarDwcTerms('longitude')$terms == T,
    "vernacularName" %in% similarDwcTerms('vname')$terms == T,
    "vernacularName" %in% similarDwcTerms('commonName')$terms == T,
    "taxonID" %in% similarDwcTerms('txn')$terms == T,
    "scientificName" %in% similarDwcTerms('txnName')$terms == T,
    "occurrenceID" %in% similarDwcTerms('occID')$terms == T,
    "catalogNumber" %in% similarDwcTerms('catID')$terms == T,
    "collectionCode" %in% similarDwcTerms('colID')$terms == T,
    "coordinatePrecision" %in% similarDwcTerms('coordErr')$terms == T,
    "coordinatePrecision" %in% similarDwcTerms('coorPrecise')$terms == T,
    "coordinatePrecision" %in% similarDwcTerms('latlngErr')$terms == T,
    "coordinatePrecision" %in% similarDwcTerms('latlngPrecise')$terms == T,
    "specificEpithet" %in% similarDwcTerms('sp')$terms == T,
    "specificEpithet" %in% similarDwcTerms('species')$terms == T,
    "infraspecificEpithet" %in% similarDwcTerms('var')$terms == T,
    "infraspecificEpithet" %in% similarDwcTerms('subsp')$terms == T,
    "individualCount" %in% similarDwcTerms('count')$terms == T,
    "higherGeography" %in% similarDwcTerms('region')$terms == T,
    "higherGeography" %in% similarDwcTerms('region', v='Event')$terms == T,
    "geodeticDatum" %in% similarDwcTerms('coordSystem')$terms == T,
    "geodeticDatum" %in% similarDwcTerms('coordSystem', v='Occurrence')$terms == T,
    "samplingProtocol" %in% similarDwcTerms('sampleMethod')$terms == T,
    "samplingProtocol" %in% similarDwcTerms('eventProtocol')$terms == T,
    "geodeticDatum" %in% similarDwcTerms('coordSys')$terms == T,
    "latitude" %in% similarDwcTerms('south')$terms == T,
    "latitude" %in% similarDwcTerms('north')$terms == T

  )
}


## List the top N similar terms
similarDwcTerms = function (col, v='*', num=10, verbatim = F) {

  # Using solr index of DwC Terms to implement this function
  urlTpl = "http://twebi.net/solr/dwc/select?q=desc:%s&mlt=true&mlt.fl=desc&mlt.mintf=1&mlt.count=%d&mlt.boost=true&fq=vocab:%s&fl=vocab,term,desc_orig_s&rows=50"
  url = sprintf(urlTpl, col, num, v)
  html = read_html(GET(url))

  term_xpath = '//result[@name="response"]/doc/str[@name="term"]'
  desc_xpath = '//result[@name="response"]/doc/str[@name="desc_orig_s"]'
  vocab_xpath = '//result[@name="response"]/doc/str[@name="vocab"]'

  term = xml_text(xml_find_all(html, term_xpath))
  desc = xml_text(xml_find_all(html, desc_xpath))
  vocab = xml_text(xml_find_all(html, vocab_xpath))

  match = data.table(vocab, term, desc)


  mlt.term_xpath = '//lst[@name="moreLikeThis"]/result/doc/str[@name="term"]'
  mlt.desc_xpath = '//lst[@name="moreLikeThis"]/result/doc/str[@name="desc_orig_s"]'
  mlt.vocab_xpath = '//lst[@name="moreLikeThis"]/result/doc/str[@name="vocab"]'

  mlt.term = xml_text(xml_find_all(html, mlt.term_xpath))
  mlt.desc = xml_text(xml_find_all(html, mlt.desc_xpath))
  mlt.vocab = xml_text(xml_find_all(html, mlt.vocab_xpath))
  suggest = data.table(vocab = mlt.vocab, term = mlt.term, desc = mlt.desc)

  suggest = rbind(match, suggest)

  match[, dist := stringdist(tolower(col), vtolower(term), method = 'dl') / nchar(term)][, vt := paste0(vocab, ':', term)]
  suggest[, dist := stringdist(tolower(col), vtolower(term), method = 'dl') / nchar(term)][, vt := paste0(vocab, ':', term)]

  if (!exists('dwcVocabTerms')) {
    getDwcAllTerms()
  }

  if (v %in% dwcVocabTerms$vocab) {
    match = match[vocab==v]
    suggest = suggest[vocab==v]
  }
  #suggest = suggest[-grep(x = term, pattern = '^verbatim')]
  #suggest = suggest[-grep(x = term, pattern = 'Unstructured$')]

  if (nrow(match) == 0 & nrow(suggest) ==0) {
    # terrible fallback
    if (!exists('dwcVocabTerms')) {
      getDwcAllTerms()
    }
    print("No match, fallback!")

    match = dwcVocabTerms[, c('vocab', 'term', 'desc'), with=F]
    match[, dist:=stringdist(tolower(col), vtolower(dwcVocabTerms$term), method='dl') / nchar(dwcVocabTerms$term)][, vt := paste0(vocab, ':', term)]
    match = unique(match[order(dist)])
    q = match[1, 'term', with=F][[1]]
    print(paste0("Try \"", q, "\"..."))
    sim = similarDwcTerms(as.character(q), v, num)

    if (v %in% dwcVocabTerms$vocab) {
      sim$match = sim$match[vocab==v]
      sim$suggest = sim$suggest[vocab==v]
    }
    #sim$suggest = sim$suggest[-grep(x = term, pattern = '^verbatim')]
    #sim$suggest = sim$suggest[-grep(x = term, pattern = 'Unstructured$')]
    sim.terms = as.vector(unlist(unique(sim$suggest$term)[c(1:num)]))

  } else {

    sim = list(match = unique(match), suggest = unique(suggest))
  }

  if (verbatim != T) {
    verbatim_patterned = grep(x = sim$suggest$term, pattern = '^verbatim')
    if (!identical(verbatim_patterned, integer(0))) {
      sim$suggest = sim$suggest[-verbatim_patterned]
    }
    unstructured_patterned = grep(x = sim$suggest$term, pattern = 'Unstructured$')
    if (!identical(unstructured_patterned, integer(0))) {
      sim$suggest = sim$suggest[-unstructured_patterned]
    }
  }

  unsorted_terms = unique(sim$suggest[, c('dist', 'term'), with=F])
  unsorted_terms[, o:=.I]
  #print(unsorted_terms)

  w = 5
  sorted_terms = unsorted_terms[order(dist), c('o', 'term'), with=F]
  sorted_terms[, w_pos:=(.I + o * w)]

  sim$terms = as.vector(unlist(sorted_terms[order(w_pos), 'term', with=F][c(1:num)]))

  sim

}


vtolower = function(arr) {
  unlist(sapply(arr, tolower))
}


##
colMapping = function (cols, v = NULL) {

  print(cols)

  if (!exists('dwcTerms')) {
    getDwcAllTerms()
  }

  if (!is.null(v)) {
    if (v %in% unique(dwcVocabTerms$vocab)) {
      dwcTerms = dwcVocabTerms[vocab == v, 'term', with=F]
    }
  }
  else {
    dwcTerms = dwcVocabTerms$term
  }

  matched = sapply(cols, function(col) {
    sims = stringsim(tolower(col), vtolower(dwcTerms), method = "dl")
    max_sim = which.max(sims)
    dwcTerms[max_sim]
  })

  # print(head(matched))
  map = data.table(old=names(matched), new=matched)

  vocabs = sapply(map$new, function(t) {
    termInDwc(t, 1)
  })

  map[, vocab := vocabs]

}


betterColMapping = function (cols, vocab='*') {

    x= sapply(cols, function(col) {
      tmp = similarDwcTerms(col, v = vocab)$term
      if (identical(tmp, character(0))) {
        return (NA)
      }
      else {
        as.vector(unlist(tmp))
      }
    })

    x
}

getMappingFromFile = function (filename, ...) {
  map_tmp = fread(input = filename, ...)
  map = as.data.table(t(map_tmp))
  names(map) <- c('old', 'new')
  map[is.na(new), new := old]
  map
}



describeDwc = function (t, v = '*', verbatim = T) {

  use.verbatim = verbatim

  if (!exists('dwcVocabTerms')) {
    getDwcAllTerms()
  }
  sim = similarDwcTerms(t, v, verbatim = use.verbatim)

  vocab = as.vector(unlist(dwcVocabTerms[term==t, 'vocab', with=F]))
  desc = as.vector(unlist(dwcVocabTerms[term==t, 'desc', with=F]))

  if (length(vocab) == 0) {
    t = sim$suggest$term[1]
    print(paste0('Do you mean "', t, '"?'))
    vocab = as.vector(unlist(dwcVocabTerms[term==t, 'vocab', with=F]))
    desc = as.vector(unlist(dwcVocabTerms[term==t, 'desc', with=F]))
  }

  desc = setNames(desc, vocab)
  desc = as.list(desc)
  desc$related = head(sim$terms, 20)
  desc

  #list(
  #  vocab = vocab,
  #  desc = desc,
  #  related = head(unique(sim$suggest$vt), 10)
  #)
}



getDwcTerms = function(dwc_type, desc = F){
  # we will use gbif darwin core archive validator to get darwin
  # core extention terms
  if (!exists('dwcExtensionList')) {
    loadDwcExtensionList()
  }
  dwcExtNames = names(dwcExtensionList)
  sims = stringsim(tolower(dwc_type), vtolower(dwcExtNames), method='dl')
  #print(sims)
  if (dwcExtNames[which.max(sims)] == dwc_type) {

  }
  else {
    v = dwcExtNames[which.max(sims)]
    save(v, file = "dwcVocabCand.RData")
    print (paste0('Do you mean "', dwcExtNames[which.max(sims)], '"?'))
    return (NA)
  }

  if (!exists('dwcVocabTerms')) {
    dwcExtUrl = dwcExtensionList[dwc_type]
    dwcUrlBase = "http://tools.gbif.org/dwca-validator"
    dwcURL = paste(dwcUrlBase, dwcExtUrl, sep='/')
    term_name_xpath = '//div[@class="definition"]//div[@class="title"]//div[@class="head"]'
    # textize the result of DwC results
    # note: we removed break new lines, tab and space
    html = read_html(GET(dwcURL))
    terms = gsub('\n|\t| ', '', xml_text(xml_find_all(html, term_name_xpath)))

    term_desc_xpath = '//div[@class="definition"]//div[@class="body"]/div[1]'
    descs = gsub(' {2,}', ' ', trimws(gsub('\n|\t', '', xml_text(xml_find_all(html, term_desc_xpath)))))

    ret = data.table(term = terms, desc = descs)

  }
  else {
    ret = dwcVocabTerms[vocab==dwc_type, .SD, .SDcols=c('term', 'desc')]
  }

  if (desc == F) {
    ret[, .SD, .SDcols='term']
  }
  else if (desc == T) {
    ret
  }
  else {
    print ("Arg 'desc' only allows boolean value (T or F)")
  }

}


#dwcEventTerms = c(
#  "parentEventID",
#  "eventID",
#  "sampleSizeValue",
#  "sampleSizeUnit",
#  "samplingProtocol",
#  "samplingEffort",
#  "eventDate",
#  "locality",
#  "locationID",
#  "higherGeography",
#  "fieldNumber",
#  "county",
#  "country",
#  "decimalLatitude",
#  "decimalLongitude",
#  "eventRemarks",
#  "year"
#)

#dwcMeasurementOrFactTerms = c(
#  "eventID",
#  "measurementType",
#  "measurementValue",
#  "measurementAccuracy",
#  "measurementUnit",
#  "measurementDeterminedDate",
#  "measurementDeterminedBy",
#  "measurementMethod",
#  "measurementRemarks"
#)

#dwcOccurrenceTerms = c(
#  "eventID",
#  "occurrenceID",
#  "recordNumber",
#  "recordedBy",
#  "catalogNumber",
#  "family",
#  "scientificName",
#  "vernacularFaName",
#  "individualCount",
#  "datasetName",
#  "license",
#  "references",
#  "occurrenceRemarks"
#)


renameSubset = function (dt, wanted, mapped, map_dt = NULL) {

  if (!is.null(map_dt)) {
    if (is.data.table(map_dt)) {
      wanted = map_dt$old
      mapped = map_dt$new
    }
  }

  dt = dt[, .SD, .SDcols = wanted]

  ## map column names to dwc vocab (just do your best)
  setnames(dt,
           old=wanted,
           new=mapped
  )
  dt
}


getDwcTable = function (dt_name, vocab, ext='o', ...) {

  vocab = as.vector(unlist(vocab))
  argg <- c(as.list(environment()), list(...))

  if (!is.character(dt_name)) {
    dt_name = deparse(substitute(dt_name))
  }

  if (exists(dt_name)) {
    dt_orig = get(dt_name)

  }
  else {
    dt_orig = NULL
  }

  if (!is.null(dt_orig) & is.data.table(dt_orig)) {
    dt = copy(get(dt_name))
  }
  else if (!is.null(dt_orig) & is.data.frame(dt_orig)) {
    dt = as.data.table(copy(get(dt_name)))
  }
  else {
    dt = data.table('dummy' = NA_character_)
  }

  dtnames = names(dt)

  if (ext == 'o') {
    sapply(vocab, FUN = function (n) {
      if ((n %in% dtnames) == F) {
        # print(argg[n][[1]])
        if (!is.null(argg[n][[1]])) {
          dt[, eval(n) := get(argg[n][[1]])]
        }
        else {
          dt[, eval(n) := NA_character_]
        }
      }
    })
    #dt = dt[[1]]
  }
  else if (ext == 'i') {
    sapply(vocab, FUN = function (n) {
      if ((n %in% dtnames) == F) {
        # print(argg[n][[1]])
        if (!is.null(argg[n][[1]])) {
          dt[, eval(n) := get(argg[n][[1]])]
        }
      }
    })

    dt = dt[, .SD, .SDcols = intersect(colnames(dt), vocab)]
  }
  else {
    sapply(vocab, FUN = function (n) {
      if ((n %in% dtnames) == F) {
        # print(argg[n][[1]])
        if (!is.null(argg[n][[1]])) {
          dt[, eval(n) := get(argg[n][[1]])]
        }
        else {
          dt[, eval(n) := NA_character_]
        }
      }
    })
    dt = dt[, .SD, .SDcols = intersect(colnames(dt), vocab)]

  }

  if ('dummy' %in% dtnames) {
    dt[, .SD, .SDcols=-c('dummy')]
  }
  else {
    dt
  }

}


getSpecialVarPattern = function () {
  eventSpecialVarPattern = "(MeasUnit|measUnitVar|DetBy|detByVar|_uuid)$"
  eventSpecialVarPattern
}

getIDVars = function (dt) {
  eventSpecialVarPattern = getSpecialVarPattern()
  idVars = append(
    intersect(
      #Reduce(c, list(dwcOccurrenceTerms, dwcMeasurementOrFactTerms, dwcEventTerms)),
      dwcVocabTerms$term,
      colnames(dt)
    ),
    grep(colnames(dt), pattern = eventSpecialVarPattern, value = T)
  )
  idVars
}

getMeasVars = function (dt) {
  measVars = setdiff(colnames(dt), getIDVars(dt))
  measVars
}

createUUID = function (dt_orig, typeLevel, cols) {

  uuid_colname = paste0(typeLevel, '_uuid')

  print(cols)
  dt = copy(dt_orig)
  dt[, 'md5'] = apply(as.matrix(dt[, .SD, .SDcols=cols]), MARGIN = 1, digest, algo='md5')
  print(nrow(dt))

  if (uuid_colname %in% colnames(dt)) {
    dt_idmap = unique(dt[complete.cases(dt[, .SD, .SD=c('md5', uuid_colname)]), .SD, .SD=c('md5', uuid_colname)])
    #should check if one md5 map to multiple uuid
    print(dt_idmap)
    setkey(dt_idmap, 'md5')
  }

  dt_type_lvl = unique(dt[, .SD, .SDcols=c(cols, 'md5')])

  dt_type_lvl[, uuid_colname] = sapply(X = rep(NA, times=nrow(dt_type_lvl)), UUIDgenerate)
  print(nrow(dt_type_lvl))

  setkey(dt, 'md5')
  setkey(dt_type_lvl, 'md5')

  dt_uuided = dt_type_lvl[, .SD, .SDcols=c('md5', uuid_colname)][dt]
  if (uuid_colname %in% colnames(dt)) {
    dt_uuided = dt_uuided[, .SD, .SDcols=-paste0('i.', uuid_colname)]
    dt_uuided = dt_idmap[dt_uuided]
    dt_uuided = dt_uuided[, .SD, .SDcols=-paste0('i.', uuid_colname)]
  }

  dt_uuided[, .SD, .SDcols=-"md5"]

}

#rktw[3, 'fbGroupSetup_uuid'] = NA
#head(rktw)
#createUUID(rktw, 'fbGroupSetup', c('fbGroup'))

makeMeasurementTpl = function (dt_orig, measVars = NA, ...) {
  dt = copy(dt_orig)

  eventSpecialVarPattern = getSpecialVarPattern()

  meas_meta_suffix = c(
    '_detBy',
    '_unit',
    '_accuracy',
    '_detDate',
    '_method',
    '_remarks'
  )

  meas_meta_full = c(
    'measurementDeterminedBy',
    'measurementUnit',
    'measurementAccuracy',
    'measurementDeterminedDate',
    'measurementMethod',
    'measurementRemarks'
  )

  idVars = getIDVars(dt)

  dt_nrow = nrow(dt)

  if (is.na(measVars)) {
    measVars = setdiff(colnames(dt), idVars)
  }

  dt.molten = melt.data.table (
    dt,
    id.vars = idVars,
    measure.vars = measVars,
    variable.name = "measurementType",
    value.name = "measurementValue"
  )

  meas_vars = unique(dt.molten$measurementType)
  tpl = copy(dt)
  meas_meta_var_colname = as.matrix(sapply(meas_vars, function(mv){
    meas_meta_full = paste0(mv, meas_meta_suffix)
    tpl.list = setNames(as.list(rep(NA, times=length(meas_meta_full))), meas_meta_full)
    assign(x = 'tpl', value = data.frame(append(tpl, tpl.list, after=match(mv, names(tpl)))), envir = parent.env(environment()))
  }))

  as.data.table(tpl)

}

makeMeasurement = function (dt_orig, measVars = NA, ...) {

  argg <- c(as.list(environment()), list(...))
  dt = copy(dt_orig)

  eventSpecialVarPattern = getSpecialVarPattern()

  meas_meta_suffix = c(
    '_detBy',
    '_unit',
    '_accuracy',
    '_detDate',
    '_method',
    '_remarks'
  )

  meas_meta_full = c(
    'measurementDeterminedBy',
    'measurementUnit',
    'measurementAccuracy',
    'measurementDeterminedDate',
    'measurementMethod',
    'measurementRemarks'
  )

  idVars = getIDVars(dt)
  if (!is.na(measVars)) {
    #idVars = setdiff(colnames(dt), measVars)
  }
  else {
    measVars = setdiff(colnames(dt), idVars)
  }
  #append(
  #    intersect(
  #        Reduce(c, list(dwcOccurrenceTerms, dwcMeasurementOrFactTerms, dwcEventTerms)),
  #        colnames(dt)
  #    ),
  #    grep(colnames(dt), pattern = eventSpecialVarPattern, value = T)
  #)

  dt_nrow = nrow(dt)


  dt = melt.data.table (
    dt,
    #id.vars = idVars,
    measure.vars = measVars,
    variable.name = "measurementType",
    value.name = "measurementValue"
  )

  View(dt)
  #invisible(dt[, detBy:=paste0(measurementType, "DetBy")][, measUnit:=paste0(measurementType, "MeasUnit")])


  #print(unique(dt$detBy))
  dtnames = colnames(dt)

  meas_vars = unique(dt$measurementType)
  meas_meta_var_colname = as.matrix(sapply(meas_vars, function(mv){
    paste0(mv, meas_meta_suffix)
  }))
  #print(meas_meta_var_colname)


  qq = apply(meas_meta_var_colname, MARGIN = 1, function(tpl) {
    #print(tpl)
    meta = sapply(tpl, function(metaVar) {
      #print(metaVar)
      if (metaVar %in% dtnames) {
        dt[c(1:dt_nrow), get(eval(metaVar))]
      }
      else {
        rep(x=NA_character_, times = dt_nrow)
      }
    })
    as.vector(as.matrix(meta))
  })
  qq = as.data.table(qq)
  colnames(qq) <- meas_meta_full
  dt = cbind(dt, qq)


  dwcMeasurementOrFactTerms = as.vector(unlist(getDwcTerms('MeasurementOrFact')))
  meas = dt[, .SD, .SDcols = intersect(append(idVars, dwcMeasurementOrFactTerms), colnames(dt))]
  print(names(meas))
  meas[, measurementIDMaterial := paste0('meas_',make.names(measurementType))]

  meas_names = colnames(meas)

  sapply(dwcMeasurementOrFactTerms, FUN = function (n) {
    if ((n %in% meas_names) == F) {
      if (!is.null(argg[n][[1]])) {
        meas[, eval(n) := dt[, argg[n][[1]], with=F]]
      }
      else {
        meas[, eval(n) := NA_character_]
      }
    }
    else {
      if (!is.null(argg[n][[1]])) {
        meas[, eval(n) := dt[, argg[n][[1]], with=F]]
      }
    }
  })

  # meas[, 'measurementID'] = sapply(X = rep(NA, times=nrow(meas)), UUIDgenerate)
  meas

}
