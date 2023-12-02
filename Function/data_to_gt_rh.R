data_to_gt_rh <- function(df) {
  
  df <- df %>% 
    rename(Mtd = MTD ,
           `Mtd Acv%` = `MTD ACH %`,
           `Mtd vs Lm` = `MTD vs LM`) %>%
    mutate(Type = factor(Type, levels = c('Rev', 'Usage', 'STS', 'S&D'))) %>% 
    arrange(Type, KPI)
  
  row_spanner_row <- df %>%
    mutate(col = (lag(Type, default = first(Type)) != Type) * row_number() - 1,
           .after = Type) %>%
    filter(col > 0) %>%
    pull(col) %>%
    unique()
  
  region_name <- unique(df$REGION)
  
  df %>%
    select(-c(IDENTIFIER:REGION, `Mtd Date`)) %>% 
    group_by(Type) %>%
    arrange(Type, KPI)%>% 
    gt(groupname_col = 'Type') %>%
    gt_theme_excel_customized(color = 'white') %>%
    cols_align(align = 'left', columns = KPI) %>% 
    cols_align(align = 'center',
               columns = c(Unit, `Mtd Acv%`, `Mtd vs Lm`)) %>%
    fmt_number(
      columns = 4:8,
      #rows = !str_detect(Unit, '%'),
      accounting = TRUE,
      decimals = 1,
      sep_mark = ','
    ) %>%
    fmt_number(
      columns = 4:8,
      rows = Unit == 'Cnt',
      decimals = 0,
      accounting = TRUE,
      sep_mark = ','
    ) %>%
    fmt_percent(
      columns = c(`Mtd Acv%`, `Mtd vs Lm`),
      decimals = 1,
      accounting = TRUE,
      sep_mark = ','
    ) %>% 
    data_color(
      columns = `Mtd Acv%`,
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
      columns = c(MTDD, `Mtd vs Lm`),
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
      columns = MTDD,
      rows = MTDD >= 0 ,
      palette = '#00B050',
      apply_to = 'text',
      method = 'numeric'
    ) %>%
    tab_style(style = list(cell_text(align = 'center', v_align = 'middle')),
              locations = list(cells_row_groups())) %>%
    tab_style(style = cell_borders(sides = c('left', 'right'),
                                   color = "#808080"),
              locations = cells_body(columns = c(KPI, Unit))) %>%
    tab_style(
      style = cell_borders(sides = 'bottom',
                           color = "#808080"),
      locations = cells_body(rows = row_spanner_row)
    ) %>%
    tab_stubhead(label = 'Type') %>% 
    text_transform(
      locations = cells_body(columns = KPI), 
      fn = function(x) {map(x, combine_to_html) %>% map(gt::html)}
    ) %>% 
    cols_label(
      MTDD = html("Mtd &Delta;")
    ) %>%
    tab_footnote(
      footnote =
        glue::glue(
          "<div style='color:red'><span>&#42;{region_name}</span></div>"
        ) %>% gt::html()
    ) %>%
    sub_missing(missing_text = '') -> gt_table
  ##############################################################################
  
  ##############################################################################
  ############# Save Summary as Image PNG
  
  gtsave(
    gt_table,
    glue::glue('Image RH/{region_name}_{Sys.Date()}.png'),
    vwidth = 1400,
    vheight = 1000,
    zoom = 1.3,
    expand = 2
  )
  
  ##############################################################################
  
  return(gt_table)
}
