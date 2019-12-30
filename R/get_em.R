#' Get an EM from an OM
#'
#' Get EM files from an OM folder. By making small changes to the OM
#' rather than having two sets of files, less files need to be maintained.
#' Differences between the OM and EM are mainly related the OM takes input
#' F values rather than catches.
#' 
#' @param dir_in A file path to a directory that contains the following files:
#' \code{forecast.ss}, \code{starter.ss}, and a control file 
#' (e.g., \code{xxxOM.ctl}). The default is to get the saved codOM from the
#' \code{ss3sim} package.
#' @param dir_out A file path to a directory where the new files will be saved.
#' The default is to save the files in your current working directory in a
#' folder called \code{new-em}.
#'
#' @import r4ss
#' @author Kelli Faye Johnson
#' @return Nothing is returned, but files are saved to the disk.
#'
get_em <- function(
  dir_in = system.file("extdata", "models", "cod-om", package = "ss3sim"),
  dir_out = file.path(getwd(), "new-em")) {

  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

  # Control file
  ctl_name <- dir(dir_in, "ctl")
  ctl_name_out <- gsub("OM", "EM", ctl_name, ignore.case = TRUE)
  dat_list <- r4ss::SS_readdat(dir(dir_in, pattern = "dat$", full.names = TRUE),
    verbose = FALSE)
  ctl_list <- r4ss::SS_readctl(file.path(dir_in, ctl_name), verbose = FALSE,
    use_datlist = TRUE, datlist = dat_list)

which(!unlist(lapply(1:83, function(ii) all(ctl_list[[ii]] == test[[ii]]))))
i <- c(75)
ctl_list[i[1]]
test[i[1]]
ctl_list[[i[1]]][, c("LO", "INIT", "HI")] ==
test[[i[1]]][, c("LO", "INIT", "HI")]

ctl_list$MainRdevYrFirst <- dat_list$Nages
ctl_list$recdev_early_start <- 1
ctl_list$recdev_early_phase <- abs(ctl_list$recdev_early_phase)
ctl_list$first_yr_fullbias_adj <- dat_list$Nages + 1
ctl_list$last_yr_fullbias_adj <- dat_list$endyr - 1
ctl_list$max_bias_adj <- 0.9
ctl_list$F_Method <- 3
names(ctl_list)[which(names(ctl_list) == "F_setup")] <- "F_iter"
ctl_list$F_iter <- 4
ctl_list <- ctl_list[-which(names(ctl_list) == "F_setup2")]

test[[40]]$PRIOR <- 0
test[[40]]$SD <- 0
test[[40]]$PR_type <- 0


  # todo: manipulate the control file
  r4ss::SS_writectl(ctllist = ctl_list, 
    outfile = file.path(dir_out, ctl_name_out),
    verbose = FALSE, overwrite = TRUE)

  # Forecast file
  forecast_list <- r4ss::SS_readforecast(
    file = dir(dir_in, pattern = "forecast", full.names = TRUE),
    Nfleets = dat_list$Nfleets, Nareas = dat_list$Nareas, 
    nseas = dat_list$nseas,
    readAll = TRUE, verbose = FALSE)
  r4ss::SS_writeforecast(forecast_list, dir = dir_out,
    writeAll = TRUE, overwrite = TRUE, verbose = FALSE)

  # Starter file
  starter_list <- r4ss::SS_readstarter(
    file = dir(dir_in, pattern = "starter", full.names = TRUE),
    verbose = FALSE)
  starter_list$datfile <- "ss3.dat"
  starter_list$ctlfile <- ctl_name_out
  starter_list$cumreport <- 1
  starter_list$last_estimation_phase <- 100
  starter_list$maxyr_sdreport <- -2
  r4ss::SS_writestarter(starter_list, dir = dir_out,
    overwrite = TRUE, verbose = FALSE, warn = FALSE)

}
