#' Select levels by variables.
#' @import magrittr
#' @import tidyr
#' 
#' @parameter dat A data.frame-class object.
#' @parameter definition A data.frame-class object.
#' @parameter pattern A char vector with length of 2.
#' 
#' @export
#' 

mf.var.selectLevels <- 
  function(
    dat, definition_in=NULL, definition_out=NULL, 
    col_name="col_name", var.strata="strata_in", pattern=c("{","}"),
    will.droplevels=TRUE
    ){
    string.pattern    <- sprintf("^\\%s(.+)\\%s$", pattern[1], pattern[2])
    separator.pattern <- sprintf("\\%s\\%s", pattern[2], pattern[1])
      
    if(is.null(definition_out)){
      for(i in 1:nrow(definition_in)){
        dat <-
          dat[
            as.character(
              dat[,definition_in[i,col_name]]
              ) %in% 
              unlist(
                strsplit(
                  gsub(string.pattern,"\\1",definition_in[i,var.strata]),
                  split = separator.pattern
                  )
                )
            ,]
        }
      }
        
    if(is.null(definition_in)){
      for(i in 1:nrow(definition_out)){
        dat <-
          dat[
            !(
              as.character(dat[
                ,
                definition_in[i,col_name]
                ]
                ) %in% 
                unlist(
                  strsplit(
                    gsub(string.pattern,"\\1",definition_in[i,var.strata]),
                    split = separator.pattern
                    )
                  )
              )
            ,]
        }
      }
    if(will.droplevels) dat <- droplevels(dat)
    return(dat)
    }

