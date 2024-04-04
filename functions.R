as.data.frame.px <- function( x, ..., use.codes = FALSE, warnings.as.errors = TRUE, direction = "long", language=FALSE){
  
  dat <- x$DATA$value  # already a data frame
  
  if ((!is.logical(language) && is.character(language))){
    if(language %in% strsplit(x$LANGUAGES$value, "\",\"")[[1]]){
      use.language = paste0('.',language,'.')
      default.language = x$LANGUAGE
      codes.ids = match(names(x$CODES), colnames(dat))
      
      for(code_id in codes.ids) {
        if (is.na(code_id)) {
          next
        }
        from_values = x$VALUES[[code_id]]
        to_values = strsplit(x[[paste0('VALUES', use.language)]][[code_id]], "\",\"|\", \"")[[1]]

        if (length(from_values) != length(to_values)) {
          stop(paste0('The from and to vectors for code_id ', code_id, ' are not the same length.'))
        }

        dat[[codes.ids[code_id]]] <- mapvalues(dat[[codes.ids[code_id]]], 
                                               from = from_values, 
                                               to   = to_values)
      }
      translated_colnames = names(x[[paste0('CODES', use.language)]])[codes.ids]
      translated_colnames = c(translated_colnames, 'value')
      colnames(dat) = translated_colnames
      
    } else {
      stop(paste0('Can\'t find the proposed language. Please choose one of the available languages: ', x$LANGUAGES$value))
    }
  } else {
    language = ''
  }
  
  ## maybe we need to change values to codes
  if (is.logical(use.codes) && use.codes)
    use.codes <- names(x$CODES)
  
  if (! is.logical(use.codes))
    for( var.name in intersect( use.codes, intersect(colnames(dat), names(x$CODES) ) ) )
      dat[[var.name]] <- mapvalues(dat[[var.name]], 
                                   from = x$VALUES[[var.name]], 
                                   to   = x$CODES[[var.name]])
  
  ## do we need to reshape?
  if (direction == "wide")
    dcast(dat, list(x$STUB$value,x$HEADING$value))
  else
    dat
}