#' Calculate Weights for Composition Data
#' 
#' Calculate scalar values to weight the input sample sizes for composition data
#' within Stock Synthesis.
#' 
#' @param dirbase A path to the Stock Synthesis files.
#' @param run A logical value specifying if the model should be run. If \code{FALSE},
#' Stock Synthesis will not be run, just the parameters will be calculated.
#' @param niters The number of times you want to tune the model.
#' @param version The version of Stock Synthesis that you are using.
#' @param fleets A vector of numeric values specifying which fleets you want to tune.
#' @param methods The method you want to use to tune the composition data.
#' 
tune_comp <- function(dirbase, run = FALSE, niters = 5, version = "3.3", 
  fleets = c(1:3, 5:8),
  methods = c("Harmonic", "Francis")) {
  start <- tune_start(dirbase)
  methods <- match.arg(methods, 
    choices = c("Harmonic", "Francis"), several.ok = TRUE)  
  
  aa <- changedir(dirbase, x = 1, xx = "0", n = "tune", nn = "Harmonic", nnn = "1")
  bb <- changedir(dirbase, x = 1, xx = "0", n = "tune", nn = "Francis", nnn = "1")
  folders <- NULL
  if ("Harmonic" %in% methods) {
    dir.create(aa, showWarnings = FALSE)
    folders <- c(folders, aa)
  }
  if ("Francis" %in% methods) {
    dir.create(bb, showWarnings = FALSE)
    folders <- c(folders, bb)
  }
  
  filesget <- dir(dirbase, pattern = "forecast.ss\\b|.exe|starter.ss\\b", 
    full.names = TRUE)
  ignore <- mapply(file.copy, to = folders, MoreArgs = list(from = filesget))
  
  dat <- r4ss::SS_readdat(file.path(dirbase, start$file_dat), 
    version = version, verbose = FALSE)
  dat$len_info[, c("CompError", "ParmSelect")] <- 0
  dat$age_info[, c("CompError", "ParmSelect")] <- 0
  ignore <- mapply(r4ss::SS_writedat, 
    outfile = file.path(folders, start$file_dat),
    MoreArgs = list(
      datlist = dat, version = version, overwrite = TRUE, verbose = FALSE)
    )
  SS_changepars(dirbase, ctlfile = "control.ss_new",
    newctlfile = "parsfortuning.txt",
    strings = "Age_DblN_descend_se_NWSLP(15)",
    newhis = 50, estimate = TRUE, verbose = FALSE)
  ctl <- readLines(file.path(dirbase, "parsfortuning.txt"))
  vars <- getvars(ctl.in = ctl, sectionstring = "mult_by_generalized_sizecomp")
  ctl <- removevarpars(ctl.in = ctl, 
    removefactor = 1:9, sectionstring = "mult_by_generalized_sizecomp")
  line <- grep("mult_by_generalized_sizecomp", ctl)
  while(substring(trimws(ctl[line]), 1, 5) != -9999) line <- line + 1
  vars <- vars[
    (!vars[, "type"] %in% 4:5) |
    (vars[, "type"] == 4:5 & !vars[, "fleet"] %in% fleets), ]
  ctl <- append(ctl, 
    values = c(apply(vars, 1, paste, collapse = " "),
    sprintf("4 %d 1 #lcompWeightMultiplier", fleets),
    sprintf("5 %d 1 #acompWeightMultiplier", fleets)),
    after = line - 1)
  ctl <- ctl[-grep("EffN_mult", ctl)]
  ignore <- mapply(writeLines, con = file.path(folders, start$file_ctl),
    MoreArgs = list(text = ctl))

  if (run) {
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    dirs <- dir(start$main, include.dirs = TRUE, pattern = "Harmonic|Francis", 
      full.names = TRUE)
    exename <- dir(dirbase, pattern = "\\.exe")
    for (ii in dirs) {
      if (run) run_ss(dir = ii, hess = FALSE)
      ii_prev <- ii
      weights <- matrix(nrow = length(fleets) * 2, ncol = niters)
      weights[, 1] <- 1
      row.names(weights) <- c(mapply(paste0, c("L", "A"), 
        MoreArgs = list("..." = fleets)))
      for (iii in 2:niters) {
        ii_name <- changedir(basename(ii), xx = iii, nnn = iii)
        copyinput(ii_prev, ii_name, file.control = paste0(start$file_ctl, "$"),
          file.data = paste0(start$file_dat, "$"))
        ctl <- readLines(file.path(ii_name, start$file_ctl))
        out <- r4ss::SS_output(ii_prev, verbose = FALSE, printstats = FALSE,
          covar = FALSE)
        len <- out$Length_comp_Eff_N_tuning_check
        age <- out$Age_comp_Eff_N_tuning_check
        temp <- r4ss::SS_tune_comps(out, write = FALSE)
        if (grepl("Francis", ii)) {
          len$Recommend_var_adj <- (temp$Old_Var_adj * temp$Francis_mult)[
            fleets & temp[, grep("Factor", colnames(temp))] == 4]
          age$Recommend_var_adj <- (temp$Old_Var_adj * temp$Francis_mult)[
            fleets & temp[, grep("Factor", colnames(temp))] == 5]
        }
        len$Recommend_var_adj <- ifelse(len$Recommend_var_adj > 1, 1, len$Recommend_var_adj)
        age$Recommend_var_adj <- ifelse(age$Recommend_var_adj > 1, 1, age$Recommend_var_adj)
        len <- len[len$Fleet %in% fleets, ]
        age <- age[age$Fleet %in% fleets, ]
        ctl[grep("lcompWeightMultiplier", ctl)] <- 
        sprintf("4 %d %.5f #lcompWeightMultiplier", 
          len$Fleet, 
          ifelse(len$Recommend_var_adj > 1, 1, len$Recommend_var_adj))
        ctl[grep("acompWeightMultiplier", ctl)] <- 
        sprintf("5 %d %.5f #acompWeightMultiplier", 
          age$Fleet, 
          ifelse(age$Recommend_var_adj > 1, 1, age$Recommend_var_adj))
        weights[, iii] <- c(len$Recommend_var_adj, age$Recommend_var_adj)
        writeLines(ctl, file.path(ii_name, start$file_ctl))
        if (run) run_ss(dir = ii_name, hess = FALSE)
        ii_prev <- ii_name
    }
  }
  }
}
