SpecialBorders <-
  function(wb_,
           sheet_,
           rows_,
           cols_,
           out_border_col = "#808080",
           out_border_thickness = "thin",
           in_border = 'TopBottomLeftRight',
           in_borderColour = '#BFBFBF',
           in_borderStyle = 'hair') {
    if (!is.list(rows_))
      print('Please provide a 2D list')
    
    row_list <-
      data.frame(start = rows_[[1]], end = rows_[[2]]) %>%
      mutate(ROW_CELL = paste(start, end, sep = ':')) %>%
      pull(ROW_CELL) %>%
      toString() %>%
      paste('c(', ., ')') %>%
      parse(text = .) %>%
      eval()
    
    
    col_list <-
      data.frame(start = cols_[[1]], end = cols_[[2]]) %>%
      mutate(ROW_CELL = paste(start, end, sep = ':')) %>%
      pull(ROW_CELL) %>%
      toString() %>%
      paste('c(', ., ')') %>%
      parse(text = .) %>%
      eval()
    
    sub_rows <- list(rep(list(row_list), 2),
                     rows_) %>%
      unlist(recursive = FALSE)
    
    sub_cols <- list(cols_,
                     rep(list(col_list), 2)) %>%
      unlist(recursive = FALSE)
    
    directions <- c("Left",
                    "Right",
                    "Top",
                    "Bottom") %>%
      as.list()
    
    ##########################  Inside Borders #############################
    
    inStyle <- createStyle(border = in_border,
                           borderColour = in_borderColour,
                           borderStyle = in_borderStyle)
    addStyle(
      wb = wb_,
      sheet = sheet_,
      rows = row_list,
      cols = col_list,
      style = inStyle,
      gridExpand = TRUE,
      stack = TRUE
    )
    
    ##########################  OutSide Borders #############################
    
    mapply(function(r_, c_, d) {
      out_style <- createStyle(border = d,
                               borderColour = out_border_col,
                               borderStyle = out_border_thickness)
      addStyle(
        wb_,
        sheet_,
        style = out_style,
        rows = r_,
        cols = c_,
        gridExpand = TRUE,
        stack = TRUE
      )
    }, sub_rows, sub_cols, directions)
    
  }

cellCols <- function(start_end) {
  if (is.list(start_end)) {
    if (length(start_end) == 2L) {
      col_list <-
        data.frame(start = start_end[[1]], end = start_end[[2]]) %>%
        mutate(ROW_CELL = paste(start, end, sep = ':')) %>%
        pull(ROW_CELL) %>%
        toString() %>%
        paste('c(', ., ')') %>%
        parse(text = .) %>%
        eval()
      return(col_list)
    } else
      print('Provide a list with length 2!')
  } else
    print("Provide a list!")
}


cellrows <- function(start_end) {
  if (is.list(start_end)) {
    if (length(start_end) == 2L) {
      row_list <-
        data.frame(start = start_end[[1]], end = start_end[[2]]) %>%
        mutate(ROW_CELL = paste(start, end, sep = ':')) %>%
        pull(ROW_CELL) %>%
        toString() %>%
        paste('c(', ., ')') %>%
        parse(text = .) %>%
        eval()
      return(row_list)
    } else
      print('Provide a list with length 2!')
  } else
    print("Provide a list!")
}
