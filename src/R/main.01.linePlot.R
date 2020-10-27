#' Create line plot.
#' 2020/10/26

Bibtex <- TRUE

dir.sub <- "./src/R/sub"
fn.sub <- list.files(dir.sub)

for(fn.sub.i in fn.sub) source(sprintf("%s/%s",dir.sub,fn.sub.i))

##```
  
load(
  file = sprintf("%s/%s", dir.RData, fn.imported_data)
  )

df.working_data <-
  df.imported_data.completed


# Propensity score model ----------

fml.ps_model <-
  sprintf(
    'as.factor(%s) ~ %s',
    var.exposure,
    paste(
      var.Psmodel,
      collapse = "+"
      )
    )

# Finishing coreating analysis data ---------------------------------------

df.working_data_completed <-
  df.working_data[
    !is.na(df.working_data[,var.exposure]),
    ]

df.imported_data.completed_leveled <-
  mf.var.defineLevels(
    dat = df.imported_data.completed,
    definition = var.label
    )

df.imported_data.completed_leveled_selected <-
  mf.var.selectLevels(
    dat = df.imported_data.completed_leveled,
    definition_in = var.strata_in
    )

## Construct a table
tabUnmatched <-
  CreateTableOne(
    vars = var.smd, 
    strata = var.exposure,
    data = df.imported_data.completed_leveled_selected, 
    test = FALSE
    )

## Simple plot

fml.facetting <- sprintf("%s~.","lung_onset_type")

df.tidy.imported_data.completed_leveled_selected <-
  df.imported_data.completed_leveled_selected %>%
    tidyr::pivot_longer(
      cols = var.outcome$col_name,
      names_to = "name.pivot_longer"
      ) %>%
    left_join(
      var.outcome,
      by = c("name.pivot_longer"="col_name")
      )


list.ggdata.simple_plot <-
  plyr::dlply(
    df.tidy.imported_data.completed_leveled_selected,
    plyr::.(outcome),
    function(D){
      measuring_results <- unique(D$name.pivot_longer)
      var.aes <- var.aes[var.aes$col_name %in% measuring_results,]
      D <-
        D %>%
        dplyr::left_join(
          df.imported_data.completed_leveled_selected[
            ,
            c(var.ID,var.aes$col_name)
            ],
          by=var.ID
        )
      print(var.aes[var.aes$aes=="color","col_name"])
      D.ggdata <- ggplot(
        data = D[!is.na(D$value),],
        aes(
          x=timepoint, 
          y=value, 
          group=get(var.ID), 
          color=get(
            var.aes[var.aes$aes=="color","col_name"]
            )
          )
        )
      return(
        D.ggdata +
          labs(
            color =
              var.col_label[
                var.col_label$col_name==var.aes[var.aes$aes=="color","col_name"],
                "col_label"
                ]
            )
        )
      }
    )

quartz(file = "output/test.review.pdf",type = "pdf")
lapply(
  list.ggdata.simple_plot,
  function(L){
    plot(
      L + geom_line() + 
        facet_grid(fml.facetting) +
        scale_color_continuous(type="viridis") +
        theme_bw()
      )
    }
  )
lapply(
  list.ggdata.simple_plot,
  function(L){
    plot(
      L + geom_line() + 
        geom_text_repel(aes(label=ID),size=1) +
        facet_grid(fml.facetting) +
        scale_color_continuous(type="viridis") +
        theme_bw()
      )
    }
  )
dev.off()

# End
