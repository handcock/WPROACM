
#library(WPROACM)

# version of textInput with more size options.
# specify class = 'input-small' or class='input-mini' in
# addition to other textInput args
customTextInput <- function(inputId, label, value = "",
                            labelstyle = "dispay:inline;", ...) {
  tagList(tags$label(label, `for` = inputId, style = labelstyle),
          tags$input(id = inputId, type = "text", value = value,
                     ...))
}

customNumericInput <- function(inputId, label, value = 0,
                               labelstyle = "display:inline;", ...) {
  tagList(tags$label(label, `for` = inputId, style = labelstyle),
          tags$input(id = inputId, type = "number", value = value,
                     ...))
}

# version of selectInput...shorter box and label
# inline lapply allows us to add each element of
# choices as an option in the select menu
inlineSelectInput <- function(inputId, label, choices, ...) {
  if(is.null(label)){
    labeldisp <- "display: none;"
  } else {
    labeldisp <- "display: inline;"
  }

  tagList(tags$label(label, `for` = inputId, style = labeldisp),
          tags$select(id = inputId, choices = choices, ...,
                      class = "shiny-bound-input inlineselect",
                      lapply(choices, tags$option)))
}

# disable widgets when they should not be usable
disableWidget <- function(id, session, disabled = TRUE) {
  if (disabled) {
    session$sendCustomMessage(type = "jsCode",
                              list(code = paste("$('#", id, "').prop('disabled',true)",
                                                sep = "")))
  } else {
    session$sendCustomMessage(type = "jsCode",
                              list(code = paste("$('#", id, "').prop('disabled',false)",
                                                sep = "")))
  }
}

attr.info <- function(df, colname, numattrs, breaks) {
  lvls <- length(unique(df[[colname]]))
  if(colname %in% numattrs & lvls > 9){
    tab <- hist(df[[colname]], breaks = breaks, plot = FALSE)
    barname <- paste(tab$breaks[1:2], collapse = "-")
    for(i in seq(length(tab$breaks) - 2)){
      barname <- append(barname, paste(tab$breaks[i+1]+1,
                                       tab$breaks[i+2], sep = "-"))
    }
    tab <- tab$counts
    names(tab) <- barname
  } else {
    tab <- table(df[[colname]])
  }
  return(tab)
}

calculate_spline <- function(src){

src <- src[src$AREA == "Total" & src$CAUSE == "Total",]
src <- src[order(src$SEX, src$AGE_GROUP, src$YEAR, src$PERIOD),]
src$NO_DEATHS <- as.numeric(src$NO_DEATHS)

dom <- c(31,28,31,30,31,30,31,31,30,31,30,31)
moy <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
if(max(src$PERIOD, na.rm =TRUE) == 12){
  day <- cumsum(c(0,dom))[src$PERIOD]+15
  src_PERIOD <- src$PERIOD
  DATE <- cumsum(c(0,365,366,365,365,365))[src$YEAR-2014] + day
}else{
  day <- as.numeric(substr(src$DATE_TO_SPECIFY_WEEK,5,6))
  Date <- match(substr(src$DATE_TO_SPECIFY_WEEK,1,3),moy)
  day <- cumsum(c(0,dom))[Date]+day-3.5
  src_PERIOD <- floor(day / 7) + 1
  src_PERIOD[day < 0 & !is.na(day)] <- 53
  aaa <- src$YEAR
  bbb <- (day - 0) < 0 & !is.na(day)
  aaa[bbb] <- aaa[bbb] + 1
  DATE <- cumsum(c(0,365,366,365,365,365))[aaa-2014] + day
}   

out <- src %>% dplyr::filter(YEAR == "2020")
wm_ident <- ifelse(max(src$PERIOD, na.rm =TRUE) == 12, "Month", "Week")
l_period <- ifelse(max(src$PERIOD, na.rm =TRUE) == 12, 12, 53)
out$ESTIMATE <- out$NO_DEATHS
out$LOWER_LIMIT <- out$NO_DEATHS
out$UPPER_LIMIT <- out$NO_DEATHS
out$EXCESS_DEATHS <- out$NO_DEATHS

pattern <- unique(paste(out$SEX, out$AGE_GROUP))
n_pat <- length(pattern)

for (j in 1:n_pat){

        if(l_period > 51){ 
	  temp_src <- src[paste(src$SEX, src$AGE_GROUP) == pattern[j],] %>% dplyr::filter(YEAR != "2020" & DATE_TO_SPECIFY_WEEK != "")
	  if(sum(temp_src$NO_DEATHS, na.rm=TRUE) == 0) next
	  src_2020 <- src[paste(src$SEX, src$AGE_GROUP) == pattern[j],] %>% dplyr::filter(DATE_TO_SPECIFY_WEEK != "")
          aDATE <-   DATE[paste(src$SEX, src$AGE_GROUP) == pattern[j] & src$DATE_TO_SPECIFY_WEEK != ""]
          num.cycle <- 53
          len.cycle <- 7
          loc_DATE <- DATE[paste(src$SEX, src$AGE_GROUP) == pattern[j] & src$YEAR != "2020" & src$DATE_TO_SPECIFY_WEEK != ""]
          loc_PERIOD <- src_PERIOD[paste(src$SEX, src$AGE_GROUP) == pattern[j] & src$YEAR != "2020" & src$DATE_TO_SPECIFY_WEEK != ""]
          days <- diff(c(0,loc_DATE))
          days[1] <- 7
          temp_src$logdays <- log(days)
          fit <- gam(NO_DEATHS ~ offset(logdays) + YEAR + s(PERIOD,bs="cc",fx=TRUE,k=9),knots=list(PERIOD=c(0,num.cycle)),method="REML", 
                   family=nb(), data=temp_src)
        }else{
	  temp_src <- src[paste(src$SEX, src$AGE_GROUP) == pattern[j],] %>% dplyr::filter(YEAR != "2020")
	  if(sum(temp_src$NO_DEATHS, na.rm=TRUE) == 0) next
	  src_2020 <- src[paste(src$SEX, src$AGE_GROUP) == pattern[j],]
          aDATE <-   DATE[paste(src$SEX, src$AGE_GROUP) == pattern[j]]
          num.cycle <- 12
          len.cycle <- 30
          loc_DATE <- DATE[paste(src$SEX, src$AGE_GROUP) == pattern[j] & src$YEAR != "2020"]
          loc_PERIOD <- temp_src$PERIOD
          days <- rep(dom,5)
          days[14] <- 29
          temp_src$logdays <- log(days)
          fit <- gam(NO_DEATHS ~ offset(logdays) + YEAR + s(PERIOD,bs="cc",fx=TRUE,k=5),knots=list(PERIOD=c(0,num.cycle)),method="REML", 
                   family=nb(), data=temp_src)
        }
	t.start <- Sys.time()
        
        src_2020$loc_DATE <- aDATE
        days <- diff(c(0,aDATE))
        days[1] <- len.cycle
        if(l_period > 51){ 
# Adjust for variable number of days in a leap year
#         days <- rep(len.cycle,nrow(src_2020))
          days[days > 3] <- len.cycle
        }
        src_2020$logdays <- log(days)
        estim <- predict(fit,newdata=src_2020,se.fit=TRUE)
        estim.median <- estim$fit
        estim.lower <- estim$fit # [5*12-1+(1:12)]
        estim.upper <- estim$fit
        theta <- fit$family$getTheta(TRUE) 
        set.seed(1)
        for(i in 1:length(estim.median)){
          a <- rnorm(n=10000, mean=estim$fit[i], sd=estim$se.fit[i])
          estim.median[i] <- mean(qnbinom(mu=exp(a), size=theta, p=0.5))
          estim.lower[i] <- mean(qnbinom(mu=exp(a), size=theta, p=0.025))
          estim.upper[i] <- mean(qnbinom(mu=exp(a), size=theta, p=0.975))
        }
        estim.median.std <- len.cycle*estim.median / exp(src_2020$logdays)
        estim.lower.std <- len.cycle*estim.lower / exp(src_2020$logdays)
        estim.upper.std <- len.cycle*estim.upper / exp(src_2020$logdays)

	for (k in 0:(l_period-1)) {

                y <- 2020
                a <- src_2020$YEAR==y & src_2020$PERIOD==(k+1)
                while(!any(a) & y >= 2017){
                  y <- y - 1
                  a <- src_2020$YEAR==y & src_2020$PERIOD==(k+1)
                }
                if(!any(a)){ a <- src_2020$YEAR==2020 & src_2020$PERIOD==k }
		out[l_period * (j -1) + k +1, "ESTIMATE"]   <- estim.median[a]
		out[l_period * (j -1) + k +1, "LOWER_LIMIT"] <- estim.lower[a]
		out[l_period * (j -1) + k +1, "UPPER_LIMIT"] <- estim.upper[a]

	}

message(paste0(out[l_period * (j -1) + k + 1, "SEX"], " ", out[l_period * (j -1) + k + 1, "AGE_GROUP"], " finished (", round(difftime(Sys.time(),
t.start), 1),"sec)"))

}

out[,3] <- rep(wm_ident)

names(out)[c(3:4)] <- c("WM_IDENTIFIER","PERIOD")

out[,"EXCESS_DEATHS"] <- out$NO_DEATHS - out$ESTIMATE

out <- melt(out, id.vars = c("COUNTRY", "ISO3", "WM_IDENTIFIER", "PERIOD", "SEX", "AGE_GROUP", "AREA", "CAUSE", "DATE_TO_SPECIFY_WEEK", "SE_IDENTIFIER", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS"))

names(out)[c(14:15)] = c("SERIES", "NO_DEATHS")

out$SERIES <- ifelse(out$SERIES == "NO_DEATHS", "Current deaths",
				ifelse(out$SERIES == "ESTIMATE", "Cyclical spline",
					"Unknown series, plz check"))
if(any(out$SERIES == "Unknown series, plz check")) message("Unknown series, plz check")


out <- out[,c("COUNTRY", "ISO3", "WM_IDENTIFIER", "PERIOD", "SEX", "AGE_GROUP", "AREA", "CAUSE", "SERIES", "NO_DEATHS", "DATE_TO_SPECIFY_WEEK", "SE_IDENTIFIER", "LOWER_LIMIT", "UPPER_LIMIT", "EXCESS_DEATHS")]

out <- out[out$SERIES != "Current deaths",]

return(out)
}

