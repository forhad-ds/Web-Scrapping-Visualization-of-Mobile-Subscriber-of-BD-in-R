
strLtSts <- function(currentDate){
  
  currentDateF <- currentDate %>% 
    format('%Y%m%d')
  
  currentDateStart <- currentDate %m-%
    days(34) %>% 
    format('%Y%m%d')
  
  currentMonthFirstDate <- currentDate %>% 
    rollback(roll_to_first = TRUE) %>% 
    format('%d-%b-%y') %>% 
    toupper()
  
  currentDay <- day(currentDate)
  
  
  lastMonthlastDate <- currentDate %m-%
    months(1) %>% 
    format('%Y%m%d')
  
  lastMonthStartDate <- currentDate %m-%
    months(1) %m-%
    days(34) %>%  
    format('%Y%m%d')
  
  lastMonthFirstDate <- currentDate %m-%
    months(1) %>% 
    rollback(roll_to_first = TRUE) %>% 
    format('%d-%b-%y') %>% 
    toupper()
  
  lastMonthDay <- currentDate %m-%
    months(1) %>% 
    day()
  
  df <- dbGetQuery(
    con_edw,
    glue::glue(
      "
      SELECT /*+PARALLEL(A,8) PARALLEL(B,8)*/
            circle_name circle,
            region_name region,
            area_name area,
            territory_name territory,
            distributor_cd,
            pos_cd,
            SUM(ers_str_amt) AS ers_str_amt,
            SUM(ers_sts_amt) AS ers_sts_amt,
            sum(case when day_key >= '{currentMonthFirstDate}' then ssc_1 else 0 end) / {currentDay} ssc_l1,
          
            'M1' month_key
        FROM
            (
                SELECT
                    
                    TO_DATE(day_key, 'RRRRMMDD') day_key,
                    sales_geo_key,
                    ( reload_str_amt + str_comm ) AS ers_str_amt,
                    reload_sts_amt AS ers_sts_amt,
                    ssc_1
                FROM
                    ocdm_sys.dwd_al_ret_all_distribution
                WHERE
                    day_key BETWEEN {currentDateStart} AND {currentDateF}
                    AND ( reload_str_amt + str_comm + reload_sts_amt ) > 0
            ) a
            LEFT JOIN
(
                SELECT
                    a.*,
                    1 AS valid_pos
                FROM
                    ocdm_sys.dwr_al_sales_geo_dim a
                WHERE
                    pos_stat = 'Active'
                and pos_cntc_num is not null
                    AND pos_chnl_cd IN ('Distributor',
                        'GPC Franchise'
                    )
                        AND territory_name NOT IN (
                        'PREMIG V1 TERRITORY',
                        'PREMIG V2 TERRITORY',
                        'MFS V1 TEST TERRITORY',
                        'ASIATIC TERRITORY'
                    ) 
            ) b ON a.sales_geo_key = b.sales_geo_key
        WHERE
            nvl(valid_pos, 0) = 1
        GROUP BY
            circle_name,
            region_name,
            area_name,
            territory_name,
            distributor_cd,
            pos_cd
        
      UNION ALL 
      
      SELECT /*+PARALLEL(A,8) PARALLEL(B,8)*/
            circle_name circle,
            region_name region,
            area_name area,
            territory_name territory,
            distributor_cd,
            pos_cd,
            SUM(ers_str_amt) AS ers_str_amt,
            SUM(ers_sts_amt) AS ers_sts_amt,
      sum(case when day_key >= '{lastMonthFirstDate}' then ssc_1 else 0 end) / {lastMonthDay} ssc_l1,
            'M2' month_key
        FROM
            (
                SELECT
                    
                    TO_DATE(day_key, 'RRRRMMDD') day_key,
                    sales_geo_key,
                    ( reload_str_amt + str_comm ) AS ers_str_amt,
                    reload_sts_amt AS ers_sts_amt,
                    ssc_1
                FROM
                    ocdm_sys.dwd_al_ret_all_distribution
                WHERE
                    day_key BETWEEN {lastMonthStartDate} AND {lastMonthlastDate}
                    AND ( reload_str_amt + str_comm + reload_sts_amt ) > 0
            ) a
            LEFT JOIN
(
                SELECT
                    a.*,
                    1 AS valid_pos
                FROM
                    ocdm_sys.dwr_al_sales_geo_dim a
                WHERE
                    pos_stat = 'Active'
                and pos_cntc_num is not null
                    AND pos_chnl_cd IN ('Distributor',
                        'GPC Franchise'
                    )
                        AND territory_name NOT IN (
                        'PREMIG V1 TERRITORY',
                        'PREMIG V2 TERRITORY',
                        'MFS V1 TEST TERRITORY',
                        'ASIATIC TERRITORY'
                    ) 
            ) b ON a.sales_geo_key = b.sales_geo_key
        WHERE
            nvl(valid_pos, 0) = 1
        GROUP BY
            circle_name,
            region_name,
            area_name,
            territory_name,
            distributor_cd,
            pos_cd
      "
    )
  )

return(df)
}
