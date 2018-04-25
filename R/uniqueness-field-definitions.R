id_fields <- c('id', 'collection', 'provider')
id_types <- paste(rep('c', length(id_fields)), collapse='')

uniqueness_fields <- c(
  'Proxy_dc_title', 'Proxy_dcterms_alternative', 'Proxy_dc_description'
)
uniqueness_labels <- c(
  'Proxy/dc:title', 'Proxy/dcterms:alternative', 'Proxy/dc:description'
)

uniqueness_fields_ext <- c(
  'Proxy_dc_title', 
  'Proxy_dc_title_score', 
  'Proxy_dcterms_alternative',
  'Proxy_dcterms_alternative_score',
  'Proxy_dc_description',
  'Proxy_dc_description_score'
)

uniqueness_types <- paste(rep('n', length(uniqueness_fields_ext)), collapse='')

all_fields <- c(id_fields, uniqueness_fields_ext)
all_types <- paste0(id_types,  uniqueness_types)
