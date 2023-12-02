############################## Out Side Borders #################################
#' Applies custom border to excel sheet
#'
#' @param wb_ A workbook
#' @param sheet_ sheet name to apply border
#' @param rows_ row numbers to apply border
#' @param cols_ col number to apply border
#' @param border_col color to apply in border
#' @param border_thickness weight of the border line
#'
#' @return  applies border color & style
#' @export
#'
#' @examples
#'   function(wb_,
#'           sheet_,
#'           rows_,
#'           cols_,
#'           border_col = "#808080",
#'           border_thickness = "thin")
#'           

OutsideBorders <-
  function(wb_,
           sheet_,
           rows_,
           cols_,
           border_col = "#808080",
           border_thickness = "thin") {
    
    left_col = min(cols_)
    right_col = max(cols_)
    top_row = min(rows_)
    bottom_row = max(rows_)
    
    sub_rows <- list(c(bottom_row:top_row),
                     c(bottom_row:top_row),
                     top_row,
                     bottom_row)
    
    sub_cols <- list(left_col,
                     right_col,
                     c(left_col:right_col),
                     c(left_col:right_col))
    
    directions <- list("Left", "Right", "Top", "Bottom")
    
    mapply(function(r_, c_, d) {
      out_style <- createStyle(border = d,
                                borderColour = border_col,
                                borderStyle = border_thickness)
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
############################## inSide Borders #################################

inSideBorders <- 
  function(wb_,
           sheet_,
           rows_,
           cols_,
           border = 'TopBottomLeftRight', 
           borderColour = '#BFBFBF', 
           borderStyle = 'hair'){
    
    
    inStyle <- createStyle(border = border, 
                             borderColour = borderColour, 
                             borderStyle = borderStyle)
    addStyle(wb = wb_,
             sheet = sheet_, 
             rows = rows_, 
             cols = cols_, 
             style = inStyle, 
             gridExpand = TRUE,
             stack = TRUE)
  }


############################## Special Borders #################################
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
################################  Cells ####################################
cells <- function(start_end) {
  if (is.list(start_end)) {
    if (length(start_end) == 2L) {
      cell_list <-
        data.frame(start = start_end[[1]], end = start_end[[2]]) %>%
        mutate(ROW_CELL = paste(start, end, sep = ':')) %>%
        pull(ROW_CELL) %>%
        toString() %>%
        paste('c(', ., ')') %>%
        parse(text = .) %>%
        eval()
      return(cell_list)
    } else
      print('Provide a list with length 2!')
  } else
    print("Provide a list!")
}

