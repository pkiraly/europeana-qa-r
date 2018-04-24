id_fields <- c('id', 'collection', 'provider')
id_types <- paste(rep('c', length(id_fields)), collapse='')

uniqueness_fields <- c(
  'Proxy_dc_title', 'Proxy_dcterms_alternative', 'Proxy_dc_description'
)
uniqueness_labels <- c(
  'Proxy/dc:title', 'Proxy/dcterms:alternative', 'Proxy/dc:description'
)
uniqueness_types <- paste(rep('n', length(uniqueness_fields)), collapse='')

all_fields <- c(id_fields, uniqueness_fields)
all_types <- paste0(id_types,  uniqueness_types)
