currentMonth <- month(Sys.Date())

lastYear <- Sys.Date() %m-% 
  months(11+currentMonth) %>% 
  floor_date(unit = 'month') %>% 
  format('%d-%b-%y') %>% 
  toupper()

ytd <- dbGetQuery(
  con_edw,
 glue::glue("select A.*, Y2023/Y2022 - 1 YTD 
from(
select 
CATEGORY, KPI_NAME, HRCY_LVL, HIARARCHY, YN, avg(KPI_value) KPI_value

from(
SELECT
    a.kpi_name,
    b.hrcy_lvl,
    b.hiararchy,
    b.month_key,
    to_char(b.month_key, 'MM') MN, 
    to_char(b.month_key, 'yyYY') yn, 
    b.kpi_value
FROM
    (
        SELECT
            *
        FROM
            gpbi_marketing.gpbi_cbr_kpi_dimension
        WHERE
            kpi_key IN (
                1101, 1102, 1103, 1202, 1205, 1808, 1905, 1402, 1401, 1801, 1701, 1703, 1705
            )
    ) a,
    (
        SELECT
            *
        FROM
            gpbi_marketing.gpbi_cbr_kpis_nsh
        WHERE
            month_key >= '{lastYear}'
          AND
            hrcy_lvl in ('NATIONAL', 'REGION')
    ) b
WHERE
    a.kpi_key = b.kpi_key (+))
    where MN <= 7
    and HIARARCHY <> 'NA' 
    group by CATEGORY, KPI_NAME, HRCY_LVL, HIARARCHY, YN)
    pivot(
    sum(kpi_value) for yn in ('2022' Y2022, '2023' Y2023)) A
    where Y2023 is not null
    order by 1, 2, 3
  ")
)