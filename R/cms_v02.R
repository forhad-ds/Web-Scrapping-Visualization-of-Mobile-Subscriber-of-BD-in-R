# CMS Raw Data ------------------------------------------------------------

cmsRaw <- rawSub %>%
  mutate(
    month_key = as.Date(paste0(month, year, '-01'), format = '%B %Y -%d'),
    year = year(month_key),
    month = month(month_key),
    rn = dense_rank(desc(month_key)),
    qrt = paste0(year, quarter(month_key)) %>%
      as.numeric()
  ) %>% 
  clean_names() %>% 
  mutate(across(gp:industry, as.numeric))

updated <- max(cmsRaw$month_key)
updatedFrom <- min(cmsRaw$month_key)

trend <- cmsRaw %>%
  select(month_key, gp:industry) %>%
  arrange(month_key) %>%
  summarise(across(gp:industry, list)) %>%
  gather(key = 'Operator', value = 'Trend')

# Process -----------------------------------------------------------------

op <- c('GP', 'RB', 'BL', 'TT', 'industry')

latest <- cmsRaw %>%
  filter(month_key == updated) %>%
  select(gp:industry) %>%
  mutate(across(gp:tt, ~ .x / industry, .names = '{.col}_share'))

latestSub <- latest %>%
  select(gp:industry) %>%
  gather(key = 'Operator', value = 'Sub', everything())

latestShare <- latest %>%
  select(contains('share')) %>%
  gather(key = 'Operator', value = 'Share', everything()) %>%
  mutate(Operator = gsub('_share', '', Operator))

momShare <- cmsRaw %>%
  filter(rn <= 2) %>%
  mutate(across(gp:tt, ~ .x / industry, .names = '{.col}_share')) %>%
  select(rn, contains('share')) %>%
  gather(key = 'Operator', value = 'Share',  contains('share')) %>%
  mutate(rn = paste0('M', rn)) %>%
  spread(rn, Share) %>%
  mutate(Operator = gsub('_share', '', Operator),
         MoMShare = M1 - M2) %>%
  select(Operator, MoMShare)


YoYShare <- cmsRaw %>%
  filter(month == month(updated)) %>%
  mutate(rn = dense_rank(desc(month_key))) %>%
  filter(rn <= 2) %>%
  mutate(across(gp:tt, ~ .x / industry, .names = '{.col}_share')) %>%
  select(rn, contains('share')) %>%
  gather(key = 'Operator', value = 'Share',  contains('share')) %>%
  mutate(rn = paste0('M', rn)) %>%
  spread(rn, Share) %>%
  mutate(Operator = gsub('_share', '', Operator),
         YoY = M1 - M2) %>%
  select(Operator, YoY)


momSub <- cmsRaw %>%
  filter(rn <= 2) %>%
  select(rn, gp:industry) %>%
  gather(key = 'Operator', value = 'Sub', -rn) %>%
  mutate(rn = paste0('M', rn)) %>%
  spread(rn, Sub) %>%
  mutate(MoMSub = M1 - M2) %>%
  select(Operator, MoMSub)

YoYSub <- cmsRaw %>%
  filter(month == month(updated)) %>%
  mutate(rn = dense_rank(desc(month_key))) %>%
  filter(rn <= 2) %>%
  select(rn, gp:industry) %>%
  gather(key = 'Operator', value = 'Sub', -rn) %>%
  mutate(rn = paste0('M', rn)) %>%
  spread(rn, Sub) %>%
  mutate(YoYSub = M1 - M2) %>%
  select(Operator, YoYSub)


QoQSub <- cmsRaw %>%
  mutate(QRNK = dense_rank(desc(qrt))) %>%
  group_by(QRNK) %>%
  mutate(MRNK = dense_rank(desc(month_key))) %>%
  filter(QRNK <= 2, MRNK == 1) %>%
  select(QRNK, gp:industry) %>%
  gather(key = 'Operator', value = 'Sub',-QRNK) %>%
  mutate(QRNK = paste0('M', QRNK)) %>%
  spread(QRNK, Sub) %>%
  mutate(QoQSub = M1 - M2) %>%
  select(Operator, QoQSub)


slist <-
  list(latestSub, latestShare, momShare, momSub, YoYSub, QoQSub, trend)

comb <- Reduce(function(x, y)
  left_join(x, y), slist) %>%
  mutate(
    Sub = case_when(
      MoMSub > 0 ~ glue::glue(
        '<div><span style="color:green">&#9650;</span> <span> {Sub}</span></div>'
      ),
      MoMSub < 0 ~ glue::glue(
        '<div><span style="color:red">&#9660;</span><span> {Sub}</span></div>'
      )
    ),
    Share = case_when(
      MoMShare  > 0 ~ glue::glue(
        '<div><span style="color:green">&#9650;</span> <span> {scales::label_percent(accuracy = 0.01)(Share)}</span></div>'
      ),
      MoMShare  < 0 ~ glue::glue(
        '<div><span style="color:red">&#9660;</span><span> {scales::label_percent(accuracy = 0.01)(Share)}</span></div>'
      )
    ),
    MoMShare = case_when(
      MoMShare  > 0 ~ glue::glue(
        '<div><span style="color:green">&#9650;</span> <span> {scales::label_percent(accuracy = 0.01)(MoMShare)}</span></div>'
      ),
      MoMShare  < 0 ~ glue::glue(
        '<div><span style="color:red">&#9660;</span><span> {scales::label_percent(accuracy = 0.01)(MoMShare)}</span></div>'
      )
    ),
    MoMSub = case_when(
      MoMSub  > 0 ~ glue::glue(
        '<div><span style="color:green">&#9650;</span> <span> {scales::label_comma(accuracy = 0.01)(MoMSub)}</span></div>'
      ),
      MoMSub  < 0 ~ glue::glue(
        '<div><span style="color:red">&#9660;</span><span> {scales::label_comma(accuracy = 0.01)(MoMSub)}</span></div>'
      )
    ),
    YoYSub = case_when(
      YoYSub  > 0 ~ glue::glue(
        '<div><span style="color:green">&#9650;</span> <span> {scales::label_comma(accuracy = 0.01)(YoYSub)}</span></div>'
      ),
      YoYSub  < 0 ~ glue::glue(
        '<div><span style="color:red">&#9660;</span><span> {scales::label_comma(accuracy = 0.01)(YoYSub)}</span></div>'
      )
    ),
    QoQSub = case_when(
      QoQSub  > 0 ~ glue::glue(
        '<div><span style="color:green">&#9650;</span> <span> {scales::label_comma(accuracy = 0.01)(QoQSub)}</span></div>'
      ),
      QoQSub  < 0 ~ glue::glue(
        '<div><span style="color:red">&#9660;</span><span> {scales::label_comma(accuracy = 0.01)(QoQSub)}</span></div>'
      )
    )
  )

# GT Object ---------------------------------------------------------------

upmonthF <- updated %>%
  format("%b'%y")

upFromF <- updatedFrom %>%
  format("%b'%y")

comb %>%
  gt() %>%
  cols_align(align = 'center') %>%
  fmt_percent(
    columns = contains('Share'),
    decimals = 2,
    accounting = TRUE
  ) %>%
  fmt_number(
    columns = !contains('Share'),
    decimals = 2,
    accounting = TRUE
  ) %>%
  fmt_markdown(columns = c(Sub, Share, MoMShare, MoMSub:QoQSub)) %>%
  # data_color(
  #   columns = MoMShare,
  #   rows =   MoMShare < 0,
  #   pallete = 'red'
  # ) %>%
  text_transform(
    locations = cells_body(columns = Operator),
    #, rows = Operator != 'industry'),
    fn = function(x) {
      local_image(filename = glue::glue('Operator Logo/{tolower(x)}.png'),
                  height = 50)
    }
  ) %>%
  tab_header(title = glue::glue('Mobile Phone Subscribers Status till {upmonthF}')) %>%
  cols_nanoplot(
    columns = Trend,
    plot_type = 'line',
    new_col_name = 'TT',
    reference_line = 'q3',
    new_col_label = html(glue::glue('Trend <br>({upFromF} ~ {upmonthF})')),
    options = nanoplot_options(show_data_area = TRUE)
  ) %>%
  cols_label(
    starts_with('SUB') &
      !ends_with('Icon') ~ html('Total User <br> (Mn)'),
    starts_with('Share') ~ html('Market <br> Share'),
    contains('MoMShare') ~ html('MoM <br> MS'),
    contains('MoMSub') ~ html('MoM <br> (Mn)'),
    contains('YoYSub') ~ html('YoY <br> (Mn)'),
    contains('QoQSub') ~ html('QoQ <br> (Mn)')
  ) %>%
  tab_style(
    style = cell_text(
      size = 'large',
      align = 'center',
      v_align = 'middle',
      weight = 'bold'
    ),
    locations = cells_title()
  ) %>%
  tab_style(
    style = list(
      cell_text(
        #size = 'large',
        align = 'center',
        v_align = 'middle',
        weight = 'bold',
      ),
      cell_text(
        size = 'large',
        align = 'center',
        v_align = 'middle',
        weight = 'bold',
      )
    ),
    locations = list(cells_body(columns = -Operator), cells_column_labels())
  ) %>%
  cols_hide(columns = c(Trend)) %>%
  tab_footnote(
    footnote = glue::glue("Data is last updated {updated_date}")
  ) %>% 
  sub_missing(missing_text = '') %>%
  gt_theme_excel_customized(color = 'white') %>%
  cols_align(columns = Operator, align = 'center') -> gt_cms

gt_cms



# Save the GT Object ------------------------------------------------------
    
gtsave(
  gt_cms,
  glue::glue('Image/CMS.png'),
  vwidth = 1400,
  vheight = 1000,
  zoom = 1.3,
  expand = 2
)

# Save GT object as html --------------------------------------------------

gtsave(
  gt_cms,
  glue::glue('html/CMS.html')
)
    
    
# The End -----------------------------------------------------------------
    