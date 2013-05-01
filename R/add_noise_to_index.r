#' Create a jittered version of a survey index based on biomass data
#'
#' This function is used to create an index of abundance sampled from the
#' expected available biomass for each fleet: survey 1 and survey 2 
#' (which mimics the fishery) + add some lognormal errors around it
#'
#' @param dat_file_in Name of the data file to read in 
#' @param dat_file_out Name of the data file to write to
#' @param start_surv Starting year survey index
#' @param end_surv Ending year survey index
#' @param start_fish Starting year fishery index
#' @param end_fish Ending year fishery index
#' @param freq_surv Frequency to return index values survey index
#' @param sd_obs_surv Standard deviation of the observation error survey index
#' @param freq_fish Frequency to return index values fishery index
#' @param sd_obs_fish Standard deviation of the observation error fishery index
#' @param make_plot Logical - make a plot of the biomass and index values?
#' @export
#' @author Cole Monnahan, Kotaro Ono, Sean Anderson
#' @examples \notrun{
#' Location of the R package code:
#' (I cannnot get system.file() to bring over the package files with the
#' correct permissions for SS_output() to work. Resorting to this in
#' the meantime. Download the package from Github and locate the
#' Simple example folder.)
#' f_in <- "~/Documents/github/ss3sim/inst/extdata/Simple/simple.dat"
#' # change as necessary
#' add_noise_to_index(f_in, "output_test.dat", start_surv = 1980,
#' end_surv = 2001, start_fish = 1980, end_fish = 2001, 
#' make_plot = TRUE)
#' }

#f_as <- system.file("extdata", "flatfish-assessment", package="ss3sim")
#f_op <- system.file("extdata", "flatfish-operating", package="ss3sim")

add_noise_to_index <- function(dat_file_in, dat_file_out, start_surv,
  end_surv, start_fish, end_fish, freq_surv=2, sd_obs_surv = 0.2,
  freq_fish=1, sd_obs_fish = 0.4, make_plot = FALSE){

  ## Calculate which years to have the index values
  years.surveyindex <- seq(from=start_surv, to=end_surv, by=freq_surv)
  years.fisheryindex <- seq(from=start_fish, to=end_fish, by=freq_fish)

  ## Grab the biomass from the report file generated by the operating model; drop
  ## the first two rows since they aren't what we want
  dat.current <- readLines(dat_file_in)
  CPUE_data_start <- grep("_year seas index obs err", dat.current,
    fixed=TRUE)[1]
  CPUE_data_end <- grep("_N_fleets_with_discard", dat.current, fixed=TRUE)[1]
  length_CPUE_dat <- CPUE_data_end - CPUE_data_start - 2
  CPUE_data <- read.table(file=dat_file_in, skip=(CPUE_data_start),
    nrows=length_CPUE_dat) 
	colnames(CPUE_data) <- c("Yr", "Seas", "Fleet", "Mean", "SD")
  yr <- CPUE_data$Yr

  if(sum(!years.surveyindex %in% CPUE_data$Yr) > 0) {
    warning("Not all years you requested for an index value are
      available in the data file. Dropping years that don't match.")
  }
  years.surveyindex <- yr[yr %in% years.surveyindex & CPUE_data$Fleet == 2]
  years.fisheryindex <- yr[yr %in% years.fisheryindex & CPUE_data$Fleet == 3]

  bio.survey <- CPUE_data[which(CPUE_data$Fleet==2 & CPUE_data$Yr %in%
    years.surveyindex),] # biomass for years w/ survey
  bio.fishery <- CPUE_data[which(CPUE_data$Fleet==3 & CPUE_data$Yr
    %in% years.fisheryindex),] # biomass for years w/ survey

  if(length(bio.survey)==0 | length(bio.fishery)==0)
    stop("Error: no matching years in either survey of fishery, index has length 0")

## Add noise:
  index.survey <- bio.survey$Mean*exp(rnorm(n=length(bio.survey$Mean),
      mean=0, sd=sd_obs_surv)-sd_obs_surv^2/2)

  ifelse(length(bio.survey)!=0, index.survey.text <-
    paste(years.surveyindex, 1, 2, index.survey, sd_obs_surv),
    index.survey.text <- NULL)

  index.fishery <-
    bio.fishery$Mean*exp(rnorm(n=length(bio.fishery$Mean), mean=0,
        sd=sd_obs_fish)-sd_obs_fish^2/2)

  ifelse(length(bio.fishery)!=0, index.fishery.text <-
    paste(years.fisheryindex, 1, 3, index.fishery, sd_obs_fish),
    index.fishery.text <- NULL)
## Done adding noise.

  ## Just for testing purposes. Create crude plots.
  if(make_plot){
    plot(bio.survey$Yr, bio.survey$Mean,  ylim=c(0, max(index.survey,
          index.fishery)*1.1), type="l", col = "red", xlab = "Years",
          ylab = "Index")
    lines(bio.fishery$Yr, bio.fishery$Mean, col = "blue")
    points(years.surveyindex, index.survey, pch=16, col="red")
    points(years.fisheryindex, index.fishery, pch=16, col="blue")
    legend("topright", legend = c("survey", "index"), fill = c("red",
        "blue"), bty = "n")
  }

  ## Open the .dat file for the assessment model and find the right lines to
  ## overwrite

  ## Write to file how many lines to read
  ind.N <- grep("#_N_cpue_and_surveyabundance_observations", x=dat.current)
  dat.current[ind.N] <- paste((length(years.surveyindex)+length(years.fisheryindex)),
    " #_N_cpue_and_surveyabundance_observations -- created by add_noise_to_index R function")
  ind.start <- grep("#_year seas index obs", x=dat.current)[1]
  ind.end <- grep("#_N_fleets_with_discard", x=dat.current)
  if(length(ind.end)==0) # different version:
    ind.end <- grep("#_N_discard_fleets", x=dat.current)
  if(length(ind.end)==0)
    stop("Couldn't locate where to print data in the .dat file.")
  dat.new <- c(dat.current[1:ind.start], index.survey.text,
    index.fishery.text, "", dat.current[ind.end:length(dat.current)])

  ## Write it back to file, possibly overwriting the original
  writeLines(text=dat.new, con=dat_file_out)
 }

