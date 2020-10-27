#' Define levels on a variable.
#' @import magrittr
#' @import tidyr
#' 
#' @parameter dat A data.frame-class object.
#' @parameter definition A data.frame-class object.
#' @parameter pattern A char vector with length of 2.
#' 
#' @export
#' 

mf.var.defineLevels <- 
  function(
    dat, definition, col_name="col_name", var.lavel="var.lavel", var.level="var.level", pattern=c("{","}")
    ){
  
  string.pattern    <- sprintf("^\\%s(.+)\\%s$", pattern[1], pattern[2])
  separator.pattern <- sprintf("\\%s\\%s", pattern[2], pattern[1])
  
  for(i in 1:nrow(definition)){
  dat[,definition[i,col_name]] <-
  factor(
    dat[,definition[i,col_name]],
    unique(dat[,definition[i,col_name]])[
      order(
        unique(dat[,definition[i,col_name]])
        )
      ],
    unlist(
      strsplit(
        gsub(
          string.pattern, "\\1", definition[i,var.level]
          ),
        split = separator.pattern
        )
      )
    )
  }
  return(dat)
  }
