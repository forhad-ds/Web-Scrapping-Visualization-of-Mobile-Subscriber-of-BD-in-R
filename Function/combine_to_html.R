# function to incorporate player name + team
#combine_to_html <- function(kpi_name){
#   
#   splitKpi <- 
#     str_split(
#       kpi_name,
#       pattern = "\\(",
#       simplify = TRUE) %>% 
#     trimws() %>% 
#     as.vector()
#   
#   if(length(splitKpi) > 1){
#     glue::glue(
#       "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:12px'>{splitKpi[1]}</div>
#         <div style='line-height:12px'><span style ='font-weight:bold;color:grey;font-size:10px'>({splitKpi[2]}</span></div>"
#     )
#   } else{
#     glue::glue(
#       "<div style='line-height:10px'><span style='font-weight:bold;font-variant:small-caps;font-size:12px'>{splitKpi[1]}</div>
#       "
#     )
#   }
#   
# }




# function to incorporate player name + team
combine_to_html <- function(kpi_name, col = 'grey') {
  splitKpi <-
    str_split(kpi_name,
              pattern = "\\(",
              simplify = TRUE) %>%
    trimws() %>%
    as.vector()
  
  if (length(splitKpi) > 1) {
    glue::glue(
      "<div><span>{splitKpi[1]}</span>
 <span style ='color:{col};font-size:9px'>({splitKpi[2]}</span></div>"
    )
  } else{
    glue::glue(
      "<div>{splitKpi[1]}</div>
      "
    )
  }
  
}

