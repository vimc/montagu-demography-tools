# Miscellaneous simple graphs...

small_country_pop <- function(con) {
  tot_pop_id <- DBI::dbGetQuery(con, "SELECT id FROM demographic_statistic_type WHERE code='tot_pop'")
  med_variant <- DBI::dbGetQuery(con, "SELECT id FROM demographic_variant WHERE code='unwpp_medium_variant'")  
  estimates <- DBI::dbGetQuery(con, "SELECT id FROM demographic_variant WHERE code='unwpp_estimates'")  
  both_gender <- DBI::dbGetQuery(con, "SELECT id FROM gender WHERE code='both'")  
  wpp2017     <- DBI::dbGetQuery(con, "SELECT id FROM demographic_source WHERE code='unwpp_2017'")
  
  all_tot_pop <- DBI::dbGetQuery(con, paste("SELECT country, year, value FROM demographic_statistic WHERE ",
                                            "demographic_statistic_type=", tot_pop_id, " AND ",
                                            "(demographic_variant=", med_variant, " OR ",
                                            "demographic_variant=", estimates, ") AND ",
                                            "gender=", both_gender, " AND ",
                                            "demographic_source=", wpp2017, sep="")
  )
  
  all_countries <- sort(unique(all_tot_pop$country))
  small_countries <- c("MHL","TUV")
  
  # Want to make a graph with x-axis 1950..2100
  # y-axis is percentage of small_countries / total.
  
  percent <- rep(0,151)
  for (i in 1950:2100) {
    this_year <- all_tot_pop[all_tot_pop$year==i,]
    total_pop <- sum(this_year$value)
    small_countries <- sum(this_year$value[this_year$country %in% c("TUV","MHL")])
    percent[i-1949] <- 100 * (small_countries / total_pop)
  }
  
  df <- data.frame(years = 1950:2100, percent = percent)
  plot(df, type="l")  
  dev.off()
  
  
   
  
}