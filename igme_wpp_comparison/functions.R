# Create a bunch of PNG files that compare
# Child Mortality data to UNWPP data for U5MR and IMR.


add_expand <- function(data, country) {
  # Insert into 1950:2100, with NA for missing
  
  country_data <- data[data$country == country,]
  country_data$value[match(1950:2100, country_data$year)]
}

plot_u5mr_imr <- function(filename, max_y, df, toptitle, ylab, year1=1950, year2=2100, est) {
  png(file = filename, width=800, height=600)
  
  plot(x = year1:year2, y = df$UNWPP, type="l", col="black", ylim=c(0,max_y), xaxt="n",
       xlab = "Year", ylab = ylab, main=toptitle)
  
  axis(side = 1,  at = seq(year1, year2, by=10))
  lines(x = year1:year2, y = df$"IGME lower", type = "l", col = "blue")
  lines(x = year1:year2, y = df$"IGME median", type = "l", col = "green")
  lines(x = year1:year2, y = df$"IGME upper", type = "l", col = "red")
  grid(nx = NULL,ny = NULL, 5, lwd = 1, col="grey")
  points(x = est$date, y=est$estimates, col="purple")
  
  legend('topright',legend = c("UNWPP","IGME lower","IGME median","IGME upper"), 
         col=c('black','blue','green','red'),
         lty=c(1,1,1))
  dev.off()
}

plot_nmr <- function(filename, max_y, df, toptitle, ylab, year1=1950, year2=2020, est) {
  png(file = filename, width=800, height=600)
  
  plot(x = year1:year2, y = df$"IGME lower"[1:71], type="l", col="blue", ylim=c(0,max_y), xaxt="n",
       xlab = "Year", ylab = ylab, main=toptitle)
  
  axis(side = 1,  at = seq(year1, year2, by=10))
  lines(x = year1:year2, y = df$"IGME median"[1:71], type = "l", col = "green")
  lines(x = year1:year2, y = df$"IGME upper"[1:71], type = "l", col = "red")
  grid(nx = NULL,ny = NULL, 5, lwd = 1, col="grey") 
  points(x = est$date, y=est$estimates, col="purple")
  
  legend('topright',legend = c("IGME NMR lower","IGME NMR median", "IGME NMR upper"),
         col=c('black','blue','green','red'),
         lty=c(1,1,1))
  dev.off()
}

plot_nmr_frac <- function(filename, max_y, df, toptitle, ylab, year1=1950, year2=2020) {
  png(file = filename, width=800, height=600)
  
  plot(x = year1:year2, y = df$UNWPP, type="l", col="black", ylim=c(0,max_y), xaxt="n",
       xlab = "Year", ylab = ylab, main=toptitle)
  
  axis(side = 1,  at = seq(year1, year2, by=10))
  lines(x = year1:year2, y = df$"IGME lower", type = "l", col = "blue")
  lines(x = year1:year2, y = df$"IGME median", type = "l", col = "green")
  lines(x = year1:year2, y = df$"IGME upper", type = "l", col = "red")
  grid(nx = NULL,ny = NULL, 5, lwd = 1, col="grey") 
  
  legend('bottomright',legend = c("IGME NMR median / UNWPP IMR","IGME NMR lower / IGME IMR lower","IGME NMR median / IGME IMR median", "IGME NMR upper / IGME IMR upper"), 
         col=c('black','blue','green','red'),
         lty=c(1,1,1))
  dev.off()
}

get_unwpp_data <- function(con, type) {
  DBI::dbGetQuery(con, paste("SELECT country, year, value FROM demographic_statistic",
                             "  WHERE demographic_statistic_type = ",type))
}

get_cm_data <- function(con, type, variant) {
  DBI::dbGetQuery(con, paste("SELECT country, year, value FROM demographic_statistic",
                             "  WHERE demographic_statistic_type = ",type,
                             "    AND demographic_variant = ",variant))
}


cm_comparison_graphs <- function(con, igme) {
  
  # Get IGME estimates
  
  est_nmr <- read_sheet(igme, sheet="Total NMR, Log.Ratio.NMR.U5MR", est_col="NMR")
  est_imr <- read_sheet(igme, sheet="Total IMR")
  est_u5mr <- read_sheet(igme, sheet="Total U5MR")
  
  # Get ids for the demographic_statics_types
  
  types <- DBI::dbGetQuery(con, "SELECT id, code FROM demographic_statistic_type")
  unwpp_u5mr_id <- types$id[types$code == 'unwpp_u5mr']
  unwpp_imr_id  <- types$id[types$code == 'unwpp_imr']
  cm_u5mr_id    <- types$id[types$code == 'cm_u5mr']
  cm_imr_id     <- types$id[types$code == 'cm_imr']
  cm_nmr_id     <- types$id[types$code == 'cm_nmr']
  
  # Get ids for the variants
  
  variants <- DBI::dbGetQuery(con, "SELECT id, code FROM demographic_variant")
  
  unwpp_estimates <- variants$id[variants$code == 'unwpp_estimates']
  unwpp_medium    <- variants$id[variants$code == 'unwpp_medium_variant']
  cm_median       <- variants$id[variants$code == 'cm_median']
  cm_lower        <- variants$id[variants$code == 'cm_lower']
  cm_upper        <- variants$id[variants$code == 'cm_upper']
  
  # Get data
  
  unwpp_u5mr_data  <- get_unwpp_data(con, unwpp_u5mr_id)
  unwpp_imr_data   <- get_unwpp_data(con, unwpp_imr_id)
  cm_u5mr_med_data   <- get_cm_data(con, cm_u5mr_id, cm_median)
  cm_u5mr_upper_data <- get_cm_data(con, cm_u5mr_id, cm_upper)
  cm_u5mr_lower_data <- get_cm_data(con, cm_u5mr_id, cm_lower)
  cm_imr_med_data    <- get_cm_data(con, cm_imr_id, cm_median)
  cm_imr_upper_data  <- get_cm_data(con, cm_imr_id, cm_upper)
  cm_imr_lower_data  <- get_cm_data(con, cm_imr_id, cm_lower)
  cm_nmr_med_data    <- get_cm_data(con, cm_nmr_id, cm_median)
  cm_nmr_upper_data  <- get_cm_data(con, cm_nmr_id, cm_upper)
  cm_nmr_lower_data  <- get_cm_data(con, cm_nmr_id, cm_lower)
  
  # Get country superset
  
  country_codes <- sort(unique(c(cm_u5mr_med_data$country,unwpp_u5mr_data$country)))
  country_db <- DBI::dbGetQuery(con, "SELECT * FROM country")
  country_names <- country_db$name[match(country_codes, country_db$id)]
  
  # Make graphs
  
  years <- 1950:2100
  
  for (i in 1:length(country_codes)) {
    est_u5mr_data_i      <- est_u5mr[est_u5mr$country == country_codes[i], ]
    est_imr_data_i       <- est_imr[est_imr$country == country_codes[i], ]
    est_nmr_data_i       <- est_nmr[est_nmr$country == country_codes[i], ]    
      
    unwpp_u5mr_data_i    <- add_expand(unwpp_u5mr_data, country_codes[i])
    cm_u5mr_lower_data_i <- add_expand(cm_u5mr_lower_data, country_codes[i])
    cm_u5mr_med_data_i   <- add_expand(cm_u5mr_med_data, country_codes[i])
    cm_u5mr_upper_data_i <- add_expand(cm_u5mr_upper_data, country_codes[i])
    unwpp_imr_data_i     <- add_expand(unwpp_imr_data, country_codes[i])
    cm_imr_lower_data_i  <- add_expand(cm_imr_lower_data, country_codes[i])
    cm_imr_med_data_i    <- add_expand(cm_imr_med_data, country_codes[i])
    cm_imr_upper_data_i  <- add_expand(cm_imr_upper_data, country_codes[i])
    cm_nmr_lower_data_i  <- add_expand(cm_nmr_lower_data, country_codes[i])
    cm_nmr_med_data_i    <- add_expand(cm_nmr_med_data, country_codes[i])
    cm_nmr_upper_data_i  <- add_expand(cm_nmr_upper_data, country_codes[i])
    
    df_u5mr <- data.frame(years, unwpp_u5mr_data_i, cm_u5mr_lower_data_i, cm_u5mr_med_data_i, cm_u5mr_upper_data_i)
    df_imr  <- data.frame(years, unwpp_imr_data_i,  cm_imr_lower_data_i,  cm_imr_med_data_i,  cm_imr_upper_data_i)    
    df_nmr  <- data.frame(years, cm_nmr_lower_data_i,  cm_nmr_med_data_i,  cm_nmr_upper_data_i)    
    colnames(df_u5mr) <- c("Year", "UNWPP","IGME lower","IGME median","IGME upper")
    colnames(df_imr) <- colnames(df_u5mr)
    colnames(df_nmr) <- c("Year","IGME lower","IGME median","IGME upper")
    
    max_y <- max(c(df_u5mr$UNWPP,df_u5mr$"IGME lower",df_u5mr$"IGME median", df_u5mr$"IGME upper",
                   df_imr$UNWPP,df_imr$"IGME lower",df_imr$"IGME median", df_imr$"IGME upper",
                   df_imr$UNWPP,df_nmr$"IGME lower",df_nmr$"IGME median", df_nmr$"IGME upper"
                   ), na.rm=TRUE)

    plot_u5mr_imr(filename=paste("graphs/",country_codes[i],"_u5mr.png", sep = ""), 
                max_y = max_y,
                df = df_u5mr, 
                toptitle = paste("U5MR for",country_names[i]),
                ylab = "Under 5 mortality rate per live birth",
                est = est_u5mr_data_i)
    
    plot_u5mr_imr(filename=paste("graphs/",country_codes[i],"_imr.png", sep = ""), 
                max_y = max_y,
                df = df_imr, 
                toptitle = paste("IMR for",country_names[i]),
                ylab = "Under 1 mortality rate per live birth",
                est = est_imr_data_i)
    
    plot_nmr(filename=paste("graphs/",country_codes[i],"_nmr.png", sep = ""), 
                  max_y = max_y,
                  df = df_nmr, 
                  toptitle = paste("NMR for",country_names[i]),
                  ylab = "Neonatal (28-day) mortality rate per live birth",
                  est = est_nmr_data_i)
    
    # Neonatal as a proportion of IMR.
    
    igme_igme_med <- cm_nmr_med_data_i / cm_imr_med_data_i
    igme_igme_lower <- cm_nmr_lower_data_i / cm_imr_lower_data_i
    igme_igme_upper <- cm_nmr_upper_data_i / cm_imr_upper_data_i
    igme_unwpp <- cm_nmr_med_data_i / unwpp_imr_data_i
    
    igme_igme_med <- igme_igme_med[1:71]
    igme_igme_lower <- igme_igme_lower[1:71]
    igme_igme_upper <- igme_igme_upper[1:71]
    igme_unwpp <- igme_unwpp[1:71]
    
    df_nmr <- data.frame(1950:2020, igme_unwpp, igme_igme_lower, igme_igme_med, igme_igme_upper)
    max_y <- max(c(igme_unwpp,igme_igme_lower,igme_igme_med,igme_igme_upper), na.rm=TRUE)
    colnames(df_nmr) <- c("Year", "UNWPP","IGME lower","IGME median","IGME upper")
    
    plot_nmr_frac(filename = paste("graphs/",country_codes[i],"_nmrfrac.png",sep=""),
                max_y = max_y,
                df = df_nmr, 
                toptitle = paste("NMR/IMR for",country_names[i]),
                ylab = "28 day mortality as fraction of 1 year mortality")
    
    # Neonatal as a proportion of IMR - plotted against IMR
    
    short_cmimr_med   <- cm_imr_med_data_i[1:71]
    short_cmimr_lower <- cm_imr_lower_data_i[1:71]
    short_cmimr_upper <- cm_imr_upper_data_i[1:71]
    short_unwpp_imr   <- unwpp_imr_data_i[1:71]
    
    png(file= paste("graphs/",country_codes[i],"_nmr_vs_imr.png", sep = ""), width=800, height=600)
    
    plot(x = short_cmimr_upper, y = df_nmr$"IGME upper", col="red", ylim=c(0,max_y),
         xlab = "IMR", ylab = "NMR/IMR", main=paste("NMR/IMR against IMR for",country_names[i]))
    
    points(x = short_cmimr_lower, y = df_nmr$"IGME lower", col = "blue")
    points(x = short_cmimr_med, y = df_nmr$"IGME median", col = "green")
    points(x = short_unwpp_imr, y = df_nmr$"UNWPP", col = "black")
    grid(nx = NULL,ny = NULL, 5, lwd = 1, col="grey") 
    
    legend('bottomright',legend = c("IGME NMR median / UNWPP IMR","IGME NMR lower / IGME IMR lower","IGME NMR median / IGME IMR median", "IGME NMR upper / IGME IMR upper"), 
           col=c('black','blue','green','red'),
           lty=c(1,1,1))
    dev.off()
  }      
  
}

read_cm_datapoints <- function(file, sheet) {
  xl <- read_excel(file, sheet = sheet, skip = )
  
}

read_sheet <- function(xlfile, sheet, skip, est_col="Estimates") {
    message(sprintf("Reading %s:%s", xlfile, sheet))
    xl<-read_excel(xlfile, sheet = sheet, skip = 2, col_names = TRUE, na = c("", "?"))
    df = data.frame(country = xl$"Country.Code",
                    estimates = xl[[est_col]]/1000,
                    date = xl$Reference.Date,
                    series = xl$Series.Name,
                    stringsAsFactors = FALSE)
    df
}

read_excel <- function(...) {
  oo <- options(warnPartialMatchArgs = FALSE)
  if (!is.null(oo$warnPartialMatchArgs)) {
    on.exit(options(oo))
  }
  readxl::read_excel(...)
}
