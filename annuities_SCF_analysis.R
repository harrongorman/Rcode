
### load packages ###

library(haven)
library(stringr)
library(survey)
library(mitools)
library(convey)
library(hexbin)

###### USING CODE FROM ######
#### https://guilhermejacob.github.io/context/1.6-survey-of-consumer-finances-scf.html#survey-of-consumer-finances-scf
#############################


# scf_MIcombine function --------------------------------------------------

scf_MIcombine <-
  function (results,
            variances,
            call = sys.call(),
            df.complete = Inf,
            ...) {
    m <- length(results)
    oldcall <- attr(results, "call")
    if (missing(variances)) {
      variances <- suppressWarnings(lapply(results, vcov))
      results <- lapply(results, coef)
    }
    vbar <- variances[[1]]
    cbar <- results[[1]]
    for (i in 2:m) {
      cbar <- cbar + results[[i]]
      # MODIFICATION:
      # vbar <- vbar + variances[[i]]
    }
    cbar <- cbar / m
    # MODIFICATION:
    # vbar <- vbar/m
    evar <- var(do.call("rbind", results))
    r <- (1 + 1 / m) * evar / vbar
    df <- (m - 1) * (1 + 1 / r) ^ 2
    if (is.matrix(df))
      df <- diag(df)
    if (is.finite(df.complete)) {
      dfobs <- ((df.complete + 1) / (df.complete + 3)) * df.complete *
        vbar / (vbar + evar)
      if (is.matrix(dfobs))
        dfobs <- diag(dfobs)
      df <- 1 / (1 / dfobs + 1 / df)
    }
    if (is.matrix(r))
      r <- diag(r)
    rval <- list(
      coefficients = cbar,
      variance = vbar + evar *
        (m + 1) / m,
      call = c(oldcall, call),
      nimp = m,
      df = df,
      missinfo = (r + 2 / (df + 3)) / (r + 1)
    )
    class(rval) <- "MIresult"
    rval
  }

# Download SAS from Website Function --------------------------------------


scf_dta_import <-
  function(this_url) {
    this_tf <- tempfile()
    
    download.file(this_url , this_tf , mode = 'wb')
    
    this_tbl <- read_dta(this_tf)
    
    this_df <- data.frame(this_tbl)
    
    file.remove(this_tf)
    
    names(this_df) <- tolower(names(this_df))
    
    this_df
  }


# Download of Replicate Weights Table -------------------------------------

scf_df <-
  scf_dta_import("https://www.federalreserve.gov/econres/files/scf2022s.zip")

ext_df <-
  scf_dta_import("https://www.federalreserve.gov/econres/files/scfp2022s.zip")

scf_rw_df <-
  scf_dta_import("https://www.federalreserve.gov/econres/files/scf2022rw1s.zip")

stopifnot(nrow(scf_df) == nrow(scf_rw_df) * 5)
stopifnot(nrow(scf_df) == nrow(ext_df))


stopifnot(all(sort(intersect(
  names(scf_df) , names(ext_df)
)) == c('y1' , 'yy1')))
stopifnot(all(sort(intersect(
  names(scf_df) , names(scf_rw_df)
)) == c('y1' , 'yy1')))
stopifnot(all(sort(intersect(
  names(ext_df) , names(scf_rw_df)
)) == c('y1' , 'yy1')))


scf_rw_df[, 'y1'] <- NULL

scf_df[, 'five'] <- 5

s1_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 1 ,]
s2_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 2 ,]
s3_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 3 ,]
s4_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 4 ,]
s5_df <- scf_df[str_sub(scf_df[, 'y1'] ,-1 ,-1) == 5 ,]

scf_imp <- list(s1_df , s2_df , s3_df , s4_df , s5_df)

scf_list <- lapply(scf_imp , merge , ext_df)

scf_rw_df[is.na(scf_rw_df)] <- 0

scf_rw_df[, paste0('wgt' , 1:999)] <-
  scf_rw_df[, paste0('wt1b' , 1:999)] * scf_rw_df[, paste0('mm' , 1:999)]

scf_rw_df <- scf_rw_df[, c('yy1' , paste0('wgt' , 1:999))]

scf_list <-
  lapply(scf_list , function(w)
    w[order(w[, 'yy1']) ,])

scf_rw_df <- scf_rw_df[order(scf_rw_df[, 'yy1']) ,]

scf_design <-
  svrepdesign(
    weights = ~ wgt ,
    repweights = scf_rw_df[,-1] ,
    data = imputationList(scf_list) ,
    scale = 1 ,
    rscales = rep(1 / 998 , 999) ,
    mse = FALSE ,
    type = "other" ,
    combined.weights = TRUE
  )

scf_design$designs <- lapply(scf_design$designs , convey_prep)

mean_net_worth <-
  scf_MIcombine(with(scf_design , svymean(~ networth)))


### used the model to calculate mean net worth
mean_annuity_income <-
  scf_MIcombine(with(scf_design , svymean(~ j8480)))

###############################################################
##### create subset of all of those with annuities income #####
###############################################################

### find variable names @ https://sda.berkeley.edu/sdaweb/analysis/?dataset=scfcomb2022

sub_scf_design_annuitiesincome <- subset(scf_design , annuit > 0, all = TRUE)

sub_scf_design_annuitiesincome <- subset(scf_design , age > 64, all = TRUE)


mean_annuities_income =
  scf_MIcombine(with(sub_scf_design_annuitiesincome , svymean(~ annuit)))

### svy hist

hist_annuities_income =
  scf_MIcombine(with(sub_scf_design, svyhist(currpen ~ annuit, 
                                                             #design = sub_scf_design_annuitiesincome,
                                                             bins = 10,
                                                             xlim = c(0, 100000),
                                                             xlab = "Annuity Income")))


png("S:/DSP_DOCUMENTS/RAs/GORMAN/Topoleski, John/Annuities_SCF/retirementincome_v_pension_over64.png", width = 6, height = 4, res = 300, units = "in", type = 'cairo-png')

  scf_MIcombine(with(sub_scf_design_annuitiesincome, svyplot(ssretinc ~ annuit,
                                                             style = "bubble",
                                                             xlim = c(1, 2000000),
                                                             ylim = c(1, 500000),
                                                             legend = 0,
                                                             xlab = "Current Value Annuities",
                                                             ylab = "SS + Pension Income"
                                                             )))

dev.off()



         
         
         
         
         