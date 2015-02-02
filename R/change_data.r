#' Change the data that is available as output from an SS operating model.
#'
#' \code{change_data} alters the data structure for a data list as read in by
#' \code{\link[r4ss]{SS_readdat}}, for use in preparing the data file for an SS
#' operating model. Original data is removed and dummy data is added, as
#' specified, to the SS \code{.dat} file. This causes SS to produce expected
#' values (OM "truth") when the operating model is run, from which data can be
#' sampled.  For each data type altered, \code{change_data} will add data for
#' the fleets and years given; potentially adding many rows of redundant data.
#' Currently, \code{.dat} files with multiple genders cannot be manipulated with
#' \code{change_data}. \code{\link{calculate_data_units}} is used internally in
#' \code{\link{ss3sim_base}} to create a superset of fleets and years from
#' sample arguments, and \code{\link{clean_data}} to strip out unused data after
#' \code{change_data} is called (see examples below). \code{change_data} is
#' called internally automatically, but can also be used by an \pkg{ss3sim} user
#' to manipulate data as a case, or to prepare a new OM for use in a simulation.
#' See the vignette for more details.
#'
#' @param datfile A list as read in by \code{\link[r4ss]{SS_readdat}}.
#' @param outfile A character value giving the location of an SS \code{.dat}
#'   file to output, if \code{write_file=TRUE}.
#' @param fleets A numeric vector of fleets
#' @param years A numeric vector of years
#' @param types A vector that can take combinations of the following entries:
#'   \code{"index"}, \code{"len"}, \code{"age"}, \code{"cal"}, \code{"mla"}.
#'   \code{types} controls what data structures the function acts on, with
#'   \code{"index"} changing indices/CPUE, \code{"len"} augmenting the length
#'   composition data, \code{"age"} augmenting the age composition, \code{"cal"}
#'   augmenting the conditional age at length, and \code{"mla"} augmenting the
#'   mean length at age data.
#' @param age_bins *A numeric vector of age bins to use. If left as \code{NULL}
#'   then the age bin structure will be taken from the OM.
#' @param len_bins *A numeric vector of length bins to use. If left as
#'   \code{NULL} then the length bin structure will be taken from the OM.
#' @param pop_binwidth *Population length bin width. Note that this value must
#'   be smaller than the bin width specified in length composition data
#'   \code{len_bins} or SS3 will fail (see notes in the SS3 manual).
#' @param pop_minimum_size *Population minimum length bin value.
#' @param pop_maximum_size *Population maximum length bin value.
#' @param lcomp_constant *A new robustification constant for length composition
#'   data to be used. Must be a numeric value, as a proportion. For example 0.1
#'   means 10 percent. See the SS3 manual for further information. A \code{NULL}
#'   value indicates no action resulting in using the current value, and a value
#'   of 0 will throw an error since that leads to an error when zeroes exist in
#'   the data. Instead use a very small value like \code{1e-07}.
#' @param tail_compression *A new tail compression value to be used in SS3. Must
#'   be a numeric value, as a proportion. For example 0.1 means 10 percent. See
#'   the SS3 manual for further information. A \code{NULL} value indicates no
#'   action, a negative value indicates to SS3 to ignore it (not use that
#'   feature).
#' @param write_file Should the \code{.dat} file be written? The new \code{.dat}
#'   file will always be returned invisibly by the function. Setting
#'   \code{write_file = FALSE} can be useful for testing. Note that you must
#'   supply a value to the argument \code{outfile}, but this argument can be set
#'   to any arbitrary value (such as \code{NULL}) if \code{write_file = FALSE}.
#'
#' @details The robustification constant is added to both the observed and
#'   expected proportions of length composition data, before being normalized
#'   internally. It is designed to help stabilize the model, but is unclear how
#'   and when to use it for optimal effect. The same value is used for all
#'   length data.
#'
#' @return An invisible data list, and a file is written if \code{write_file =
#'   TRUE}.
#'
#' @template casefile-footnote
#'
#' @importFrom r4ss SS_readdat SS_writedat
#' @export
#' @seealso \code{\link{sample_lcomp}}, \code{\link{sample_agecomp}}
#' @author Cole Monnahan, Ian Taylor, Sean Anderson, Kelli Johnson
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' fleets <- 1:2
#' years <- c(5, 10, 15)
#' types <- c("len", "age")
#' file_in <- r4ss::SS_readdat(paste0(d, "/models/cod-om/codOM.dat"))
#'
#' # Basic test with just length data, default bins:
#' out <- change_data(file_in, outfile = "ignore.dat", types = "len",
#'   years = years, fleets = fleets, write_file = FALSE)
#' print(out$lbin_vector)
#' print(out$lencomp)
#'
#' # Change the length bins:
#' out <- change_data(file_in, "ignore.dat", types = "len",
#'   years = years, fleets = fleets, len_bins = 3:6, write_file = FALSE)
#' out$lbin_vector
#' out$lencomp
#'
#' # Change the population length bins:
#' out <- change_data(file_in, "ignore.dat", types = "len",
#'   years = years, fleets = fleets, pop_binwidth = 1, pop_minimum_size = 5,
#'   pop_maximum_size = 210, write_file = FALSE)
#' out$binwidth
#' out$maximum_size
#' out$minimum_size
#'
#' # Sample from index, length composition, age composition, catch at length,
#' # mean length at age data: (normally this is all done from within run_ss3sim).
#'
#' index_params = list(fleets = c(1, 2), years = list(c(1, 2),
#'   c(10, 11)), sds_obs = c(0.1, 0.2))
#' lcomp_params = list(Nsamp = list(12345), fleets = 1, years = list(c(1, 5)))
#' agecomp_params = list(Nsamp = list(12345), fleets = c(1, 2),
#'   years = list(2, c(15, 16)))
#' calcomp_params = list(Nsamp = list(1), fleets = c(1), years = 98)
#' mlacomp_params = list(fleets = c(2), Nsamp = 54, years = list(c(1, 15)))
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' datfile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#' data_units <- calculate_data_units(index_params = index_params,
#'   lcomp_params = lcomp_params, agecomp_params = agecomp_params,
#'   calcomp_params = calcomp_params, mlacomp_params = mlacomp_params)
#' data_units
#' dat2 <- with(data_units, change_data(datfile = datfile, fleets = fleets,
#'   years = years, types = types, write_file = FALSE))
#' dat3 <- clean_data(datfile = dat2, lcomp_params = lcomp_params,
#'   index_params = index_params, agecomp_params = agecomp_params,
#'   calcomp_params = calcomp_params, mlacomp_params = mlacomp_params,
#'   verbose = TRUE)

change_data <- function(datfile, outfile, fleets, years, types,
                        age_bins = NULL, len_bins = NULL, pop_binwidth = NULL,
                        pop_minimum_size = NULL, pop_maximum_size = NULL,
                        lcomp_constant = NULL, tail_compression = NULL,
                        write_file = TRUE) {

  # TODO: pop length bins must not be wider than the length data bins, but the
  # boundaries of the bins do not need to align (from SS3 manual)
  # this is also checked within SS3 and will create a fatal error

    is_ssdat_file(datfile)

    ## Input checks:
    types <- match.arg(types,
                       choices = c("index","len", "age", "cal", "mla", "mwa"),
                       several.ok = TRUE)

    ## Test for compatibility with ss3sim
    if (datfile$Ngenders > 1) {
        stop(paste("_Ngenders is greater than 1 in the operating model.",
                   "change_data only works with single-gender models."))
    }

    ## TODO: Need to do things like change age matrices?
    ## TODO: Change the data vectors if specified?

    # population bins:
    # change_pop_bin() deals with NULLs internally by not changing values:
    datfile <- change_pop_bin(datfile,
                              binwidth = pop_binwidth,
                              minimum_size = pop_minimum_size,
                              maximum_size = pop_maximum_size)

    if(is.null(len_bins)) len_bins <- datfile$lbin_vector
    if(is.null(age_bins)) age_bins <- datfile$agebin_vector

    ## Now modify each data type in turn
    if ("index" %in% types) {
        datfile$CPUE <- make_dummy_dat_index(fleets=fleets, years=years)
        datfile$N_cpue <- nrow(datfile$CPUE)
    }
    if ("len" %in% types) {
        datfile$lencomp <-
            make_dummy_dat_lencomp(fleets=fleets, years=years,
                                   len_bins=len_bins)
        datfile$lbin_vector <- len_bins
        datfile$N_lencomp <- nrow(datfile$lencomp)
    }
    ## Need to split calcomp and agecomp data as separate cases
    if ("age" %in% types) {
        conditional_data <- datfile$agecomp[datfile$agecomp$Lbin_lo >= 0, ]
        new.agecomp <-
            make_dummy_dat_agecomp(fleets=fleets, years=years,
                                   age_bins=age_bins)
        datfile$agecomp <- rbind(new.agecomp, conditional_data)
        datfile$agebin_vector <- age_bins
        datfile$N_agecomp <- nrow(datfile$agecomp)
    }
    ## If we don't use this structure to get expected value we don't need
    ## to print them, so don't need it. TODO check this is right. It can
    ## seriously slow down the OM to write uncessary calcomp data.
    if ("cal" %in% types) {
        agecomp <- datfile$agecomp[datfile$agecomp$Lbin_lo < 0, ]
        new.calcomp <-
            make_dummy_dat_calcomp(fleets=fleets, years=years,
                               age_bins=age_bins, len_bins=len_bins)
        datfile$agecomp <- rbind(agecomp, new.calcomp)
        datfile$agebin_vector <- age_bins
        datfile$N_agecomp <- nrow(datfile$agecomp)
    }

    if ("mla" %in% types) {
        datfile$MeanSize_at_Age_obs <-
            make_dummy_dat_mlacomp(fleets = fleets, years = years, age_bins = age_bins)
        datfile$N_MeanSize_at_Age_obs <- nrow(datfile$MeanSize_at_Age_obs)
    }

    if(!is.null(lcomp_constant)) {
      datfile <- change_lcomp_constant(lcomp_constant = lcomp_constant,
        datfile = datfile, file_out = "ignore.dat", write_file = FALSE)
    }
    if(!is.null(tail_compression)) {
      datfile <- change_tail_compression(tail_compression = tail_compression,
        datfile = datfile, file_out = "ignore.dat", write_file = FALSE)
    }

    if (write_file) {
        SS_writedat(datlist = datfile, outfile = outfile, overwrite = TRUE,
                    verbose = FALSE)
    }
    invisible(datfile)
}

#' Given sampling arguments, calculate super set of fleets, years, and data
#' types.
#'
#' @author Cole Monnahan
#' @param index_params, lcomp_params, agecomp_params, calcomp_params,
#' mlacomp_params Named lists containing the arguments for the different
#' sampling functions.
#' @seealso clean_data, change_data
#' @note A superset by nature is larger than the individual sets used to
#' create it (unless all sampling arguments are identical), so that the
#' returned list will created some unnecessary combinations. This was done
#' intentionally for simplicity but may be changed later. See the vignette
#' for further information.
#' @note See further examples in \code{\link{change_data}}.
#' @return An invisible list of fleets, years, and types.
#' @examples
#' ## Should throw error since nothing passed
#' # calculate_data_units()
#' ## Only one fleet
#' calculate_data_units(lcomp_params=list(fleets=1, years=c(3,4,6)))
#' ## Add new fleet
#' calculate_data_units(lcomp_params=list(fleets=1, years=c(3,4,6)),
#'                      agecomp_params=list(fleets=2, years=5))
#' ## If CAL data called, need other types even if not specified
#' calculate_data_units(calcomp_params=list(fleets=1, years=c(3,4,6)))
#' @export
calculate_data_units <- function(index_params=NULL, lcomp_params=NULL,
                                 agecomp_params=NULL, calcomp_params=NULL,
                                 mlacomp_params=NULL, wtatage_params=NULL){
    sample_args <- list("index"=index_params, "len"=lcomp_params,
                        "age"=agecomp_params, "cal"=calcomp_params,
                        "mla"=mlacomp_params, "wtatage"=wtatage_params)
    sample_args_null <- vapply(sample_args, is.null, logical(1L))
    ## Exit if nothing specified to prevent error.
    if(!any(!sample_args_null)) stop("No data passed: all arguments NULL")
    ## Get the superset of fleets
    fleets <-
        as.vector(unlist(lapply(sample_args, function(x) x$fleets)))
    ## Get the superset of years
    years <-
        as.vector(unlist(lapply(sample_args, function(x) x$years)))
    ## Sort the unique values
    fleets <- sort(unique(fleets))
    years <- sort(unique(years))
    ## Now figure out which data types need to be in the OM for sampling (but
    ## not necessarily the EM). For now these are special cases but could be
    ## different based on different algorithms.


    #To-Do
    ##Put line for wtatage
    #if wtatage is in types what do I need to make sure is there
    types <- names(sample_args)[!sample_args_null]
    if("cal" %in% types) types <- c(types, "len", "age")
    if("wtatage" %in% types) types <- c(types, "age", "mla")
    if("mla" %in% types) types <- c(types, "age")
    ## Need this line to remove duplicates
    types <- unique(types)
    return(list(fleets=fleets, years=years, types=types))
}

change_pop_bin <- function(datfile, binwidth = NULL, minimum_size = NULL,
  maximum_size = NULL){

  if (!is.null(binwidth)) datfile$binwidth <- binwidth[1]
  if (!is.null(minimum_size)) datfile$minimum_size <- minimum_size[1]
  if (!is.null(maximum_size)) datfile$maximum_size <- maximum_size[1]

  ## FIXME: Cole left this note:
  ## The ageing error matrices must also
  ## be changed because they have one column per population length bin
  invisible(datfile)
}

# quick checks that datfile looks correct:
is_ssdat_file <- function(x) {
  if(!is.list(x))
    stop("datfile isn't a list. dat should be output from r4ss::SS_readdat()")
  if(!"type" %in% names(x))
    stop(paste("the column *type* wasn't found in datfile.",
      "datfile should be output from r4ss::SS_readdat()"))
}
