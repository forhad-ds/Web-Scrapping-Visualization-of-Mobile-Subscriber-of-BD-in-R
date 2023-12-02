data_to_gt_extend <- function(df, labels_collapse = NULL) {
  
  region_name <- unique(df$REGION)
  
  if(region_name == 'National'){
    df <- df %>% 
      select(-c(llr, lnamer))
  } else if (str_detect(toupper(region_name), 'CIRCLE')){
    df <- df %>% 
      select(-c(ll, lname)) %>% 
      rename(ll = llr,
             lname = lnamer)
  } else {
    df <- df %>% 
      select(-c(ll:lnamer))
  }
  
  if(region_name == 'National' |
     str_detect(toupper(region_name), 'CIRCLE')){
   
     labels <- df %>% 
      filter(!is.na(`Mtd Trgt`)) %>% 
      pull(lname) %>% 
      unlist() %>% 
      unique()
    
    # labels_collapse <- '  /  '
    
    if(is.null(labels_collapse)){
      labels_collapse <- '||'
    }
    
    palette <- scales::hue_pal()(length(labels))
    
    label_built <- mapply(function(pal, lab) {
      glue::glue("<span style='color:{pal}'><b>{lab}</b></span>")
    }, as.list(palette), as.list(labels), SIMPLIFY = TRUE) %>% 
      paste(collapse = labels_collapse) %>% 
      gt::html()
  }
  
  df <- df %>%
    mutate(
      Type = factor(Type, levels = unique(image_summary_mapping$Type)),
      KPI = factor(KPI, levels = unique(image_summary_mapping$KPI_TO_BE)),
      Unit = if_else(Unit %in% c('000', 'Count'), 'Cnt', Unit)
    ) %>%
    rename(
       Mtd = MTD ,
      `Mtd Acv%` = `MTD ACH %`,
      `Mtd vs Lm` = `MTD vs LM`) %>% 
    arrange(Type, KPI)
  
  row_spanner_row <- df %>%
    mutate(col = (lag(Type, default = first(Type)) != Type) * row_number() - 1,
           .after = Type) %>%
    filter(col > 0) %>%
    pull(col) %>%
    unique()
  
  
  
  df %>%
    relocate(`Mtd Date`, .after = everything()) %>% 
    select(- c(IDENTIFIER:REGION), - contains('lname')) %>%
    group_by(Type) %>%
    #mutate(KPI = map(KPI, combine_to_html) %>% map(gt::html)) %>%
    gt() %>%
    gt_theme_excel_customized(color = 'white') %>%
    cols_align(align = 'left', columns = KPI) %>%
    cols_align(align = 'center',
               columns = c(Unit, `Mtd Acv%`, `Mtd vs Lm`, `Mtd Date`)) %>%
    fmt_number(
      columns = 4:7,
      rows = !str_detect(Unit, '%'),
      decimals = 1,
      sep_mark = ','
    ) %>%
    fmt_number(
      columns = 4:7,
      rows = Unit == 'Cnt',
      decimals = 0,
      accounting = TRUE,
      sep_mark = ','
    ) %>%
    fmt_number(
      columns = 4:7,
      rows = Type == 'Price',
      decimals = 3,
      sep_mark = ','
    ) %>%
    fmt_percent(
      columns = 4:7,
      rows = Unit == '%',
      decimals = 1,
      sep_mark = ','
    ) %>%
    fmt_percent(
      columns = c(`Mtd Acv%`, `Mtd vs Lm`),
      #rows = Unit == '%',
      decimals = 1,
      accounting = TRUE,
      sep_mark = ','
    ) %>%
    data_color(
      columns = `Mtd Acv%`,
      # rows = `Mtd Ach %` < 0.97,
      palette = '#C00000',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    data_color(
      columns = `Mtd Acv%`,
      rows = `Mtd Acv%` > 0.97,
      palette = '#FFC000',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    data_color(
      columns = `Mtd Acv%`,
      rows = `Mtd Acv%` > 0.99,
      palette = '#00B050',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    data_color(
      columns = `Mtd vs Lm`,
      palette = '#C00000',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    data_color(
      columns = `Mtd vs Lm`,
      rows = `Mtd vs Lm` >= 0,
      palette = '#00B050',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    data_color(
      columns = `Mtd vs Lm`,
      rows = KPI %in% c(
        'Gross Churn',
        'Net Churn',
        'DH with STR < STS',
        'Replenishment % (Rolling 35D)',
        'POS % with <1 day SSC'
      ),
      palette = '#00B050',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    data_color(
      columns = `Mtd vs Lm`,
      rows = 
        KPI %in% c(
          'Gross Churn',
          'Net Churn',
          'DH with STR < STS',
          'Replenishment % (Rolling 35D)',
          'POS % with <1 day SSC'
        ) & `Mtd vs Lm` > 0,
      palette = '#C00000',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    text_transform(
      locations = cells_body(columns = KPI), 
      fn = function(x) {map(x, combine_to_html) %>% map(gt::html)}
    ) %>% 
    {if(region_name == 'National' |
        str_detect(toupper(region_name), 'CIRCLE')){
      text_transform(
        data = .,
        locations = cells_body(columns = ll,
                               rows = !is.na(`Mtd Trgt`)),
        fn = function(x) {
          bar_fx <- function(x_val,
                             position = "stack", 
                             width = 70, 
                             total_rng = length(labels), 
                             col_pal = palette) {
            if (x_val %in% c("NA", "NULL")) {
              return("<div></div>")
            }
            
            vals <- strsplit(x_val, split = ", ") %>%
              unlist() %>%
              as.double()
            
            n_val <- length(vals)
            
            total_rng <- max(total_rng, sum(vals))
            
            if(is.null(col_pal)){
              col_pal <- scales::hue_pal()(n_val)
            }
            
            
            col_fill <- c(1:n_val)
            
            df_in <- dplyr::tibble(x = vals,
                                   y = rep(1, n_val),
                                   fill = col_pal[col_fill])
            
            plot_out <- df_in %>%
              ggplot(aes(
                x = .data$x,
                y = factor(.data$y),
                fill = I(.data$fill),
                group = .data$y
              )) +
              geom_col(position = position,
                       color = "white",
                       width = 1) +
              geom_text(
                aes(label = scales::label_percent(accuracy = 1)(x)),
                hjust = 0.4,
                size = 3,
                family = "mono",
                position = if (position == "fill") {
                  position_fill(vjust = .5)
                } else if (position == "stack") {
                  position_stack(vjust = .5)
                },
                color = "white"
              ) +
              scale_x_continuous(expand = if (position == "stack") {
                expansion(mult = c(0, 0.1))
              } else {
                c(0, 0)
              },
              limits = if (position == "stack") {
                c(0, total_rng)
              } else {
                NULL
              }) +
              scale_y_discrete(expand = c(0, 0)) +
              coord_cartesian(clip = "off") +
              theme_void() +
              theme(legend.position = "none",
                    plot.margin = margin(0, 0, 0, 0, "pt"))
            
            out_name <- file.path(tempfile(
              pattern = "file",
              tmpdir = tempdir(),
              fileext = ".svg"
            ))
            
            ggsave(
              out_name,
              plot = plot_out,
              dpi = 25.4,
              height = 4,
              width = width,
              units = "mm",
              device = "svg"
            )
            
            img_plot <- readLines(out_name) %>%
              paste0(collapse = "") %>%
              gt::html()
            
            on.exit(file.remove(out_name), add = TRUE)
            
            img_plot
          }
          
          tab_built <- lapply(X = x, FUN = bar_fx)
        }
        
      )
    } else .
      } %>% 
    {if(region_name == 'National' |
        str_detect(toupper(region_name), 'CIRCLE')){
    cols_label(.data = ., 
      ll = label_built
    )} else .
      } %>% 
    tab_style(style = list(cell_text(align = 'center', v_align = 'middle')),
              locations = list(cells_row_groups())) %>%
    tab_style(style = cell_borders(sides = c('left', 'right'),
                                   color = "#808080"),
              locations = cells_body(columns = c(KPI, Unit))) %>%
    tab_style(style = cell_borders(sides = 'bottom',
                                   color = "#808080"),
              locations = cells_body(rows = row_spanner_row)) %>%
    tab_stubhead(label = 'Type') %>%
    tab_footnote(
      footnote = 
        glue::glue(
          "<div style='color:red'><span>&#42;{region_name}</span></div>"
        ) %>% gt::html()
    ) %>% 
    sub_missing(missing_text = '') %>% 
    tab_options(data_row.padding = px(3)) -> gt_table
  ##############################################################################
  
  ##############################################################################
  ############# Save Summary as Image PNG
  
  gtsave(gt_table,
         glue::glue('Image Extend/ext_{region_name}_{Sys.Date()}.png'),
         vwidth = 1400,
         vheight = 1000,
         zoom = 1.3, 
         expand = 2)
  
  ##############################################################################
  
  return(gt_table)
}
