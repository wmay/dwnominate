# an interface to Keith Poole's DW-NOMINATE FORTRAN77 program.

# limitations:
# 660 legislators per session
# 2901 votes per session

#' @importFrom Rdpack reprompt

# get legislator names from rollcall objects
get_leg_names = function(x) row.names(x$legis.data)

fix_string = function(string) gsub("[^A-Za-z ()'-]", '', string)

zero_if_missing = function(a, n) {
  if (is.null(a)) {
    rep(0, n)
  } else {
    ifelse(is.na(a), 0, a)
  }
}

format_column = function(df, name, format, alternative=NULL) {
  if (name %in% names(df)) {
    if (inherits(df[, name], 'character')) {
      sprintf(format, fix_string(df[, name]))
    } else {
      sprintf(format, df[, name])
    }
  } else {
    sprintf(format, alternative)
  }
}

write_rc_data_file = function(rc_list, lid, leg_dict) {
  session_rows = sapply(rc_list, function(x) nrow(x$votes))
  session_cols = sapply(rc_list, function(x) ncol(x$votes))
  total_rcs = sum(session_rows)
  max_rcs = max(session_cols)
  if (max_rcs > 2901) {
    stop('Cannot include more than 2901 roll call votes per session')
  }
  if (total_rcs > 54001) {
    stop('Cannot include more than 54001 total legislator/session combinations')
  }
  m1 = matrix(FALSE, nrow=total_rcs, ncol=max_rcs)
  m9 = matrix(FALSE, nrow=total_rcs, ncol=max_rcs)
  
  all_leg_ids = vector()
  all_state_num = vector()
  all_district = vector()
  all_parties = vector()
  start_row = 1
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    rcl = rc$legis.data
    n_rcl = nrow(rcl)
    votes = rc$votes
    codes = rc$codes
    all_leg_ids = c(all_leg_ids, leg_dict[as.character(rcl[, lid])])
    all_state_num = c(all_state_num, zero_if_missing(rcl$icpsrState, n_rcl))
    all_district = c(all_district, zero_if_missing(rcl$cd, n_rcl))
    all_parties = c(all_parties, zero_if_missing(rcl$party, n_rcl))
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9

    end_row = start_row + session_rows[session] - 1
    end_col = session_cols[session]
    m1[start_row:end_row, 1:end_col] = votes == 1
    m9[start_row:end_row, 1:end_col] = votes == 9

    start_row = end_row + 1
  }
  list(jjjj=NA,
       jd1=all_leg_ids,
       jstate=as.integer(all_state_num),
       jdist=as.integer(all_district),
       jparty=as.integer(all_parties),
       rcvote1=m1, rcvote9=m9)
}

write_transposed_rc_data_file = function(rc_list) {
  session_rows = sapply(rc_list, function(x) ncol(x$votes))
  session_cols = sapply(rc_list, function(x) nrow(x$votes))
  total_rcs = sum(session_rows)
  max_rcs = max(session_cols)
  if (max_rcs > 660) {
    stop('Cannot include more than 660 legislators per session')
  }
  if (total_rcs > 99001) {
    stop('Cannot include more than 99001 total roll call votes')
  }
  m1 = matrix(FALSE, nrow=total_rcs, ncol=max_rcs)
  m9 = matrix(FALSE, nrow=total_rcs, ncol=max_rcs)
  start_row = 1
  for (session in 1:length(rc_list)) {
    votes = t(rc_list[[session]]$votes)
    codes = rc_list[[session]]$codes
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9

    end_row = start_row + session_rows[session] - 1
    end_col = session_cols[session]
    m1[start_row:end_row, 1:end_col] = votes == 1
    m9[start_row:end_row, 1:end_col] = votes == 9

    start_row = end_row + 1
  }
  list(rcvotet1=m1, rcvotet9=m9)
}

write_leg_file = function(rc_list, start, dims, lid, leg_dict) {
  ksta = vector()
  lnames = vector()
  all_sessions = vector()
  all_leg_ids = vector()
  all_state_nums = vector()
  all_districts = vector()
  all_parties = vector()
  all_coords = matrix(nrow=0, ncol=dims)
  coordcols = paste0('coord', 1:dims, 'D')
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    # can't use tibbles, only regular data frames
    rcl = as.data.frame(rc$legis.data)
    n_rcl = nrow(rcl)
    all_sessions = c(all_sessions, rep(session, n_rcl))
    all_leg_ids = c(all_leg_ids, leg_dict[as.character(rcl[, lid])])
    all_state_nums = c(all_state_nums, zero_if_missing(rcl$icpsrState, n_rcl))
    all_districts = c(all_districts, zero_if_missing(rcl$cd, n_rcl))
    ksta = c(ksta, as.character(zero_if_missing(rcl$state, n_rcl)))
    all_parties = c(all_parties, zero_if_missing(rcl$party, n_rcl))
    lnames = c(lnames, rownames(rc$votes))
    if (inherits(start, 'common space')) {
      matches = match(rcl[, lid], row.names(start$legislators))
      coords = as.matrix(start$legislators[matches, coordcols])
    } else if (inherits(start, 'dwnominate')) {
      session_starts = start$legislators[start$legislators$session == session, ]
      matches = match(rcl[, lid], session_starts[, lid])
      coords = as.matrix(session_starts[matches, coordcols])
    } else {
      matches = match(rcl[, lid], start$legislators[, lid])
      coords = as.matrix(start$legislators[matches, coordcols])
    }
    # DW-NOMINATE hates NA's
    coords[is.na(coords)] = 0
    all_coords = rbind(all_coords, coords)
  }
  list(ksta=ksta, lname=lnames,
       ncong=as.integer(all_sessions),
       id1=all_leg_ids,
       istate=as.integer(all_state_nums),
       idist=as.integer(all_districts),
       iparty=as.integer(all_parties),
       xdata=all_coords)
}

write_bill_file = function(rc_list, start, dims) {
  ## for now it looks like all bill info is being initialized to zero
  all_sessions = vector()
  all_bill_ids = vector()
  all_dyns = matrix(nrow=0, ncol=dims)
  all_mids = matrix(nrow=0, ncol=dims)
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    nbills = ncol(rc$votes)
    sessions = rep(session, nbills)
    all_sessions = c(all_sessions, sessions)
    bill_ids = 1:nbills
    all_bill_ids = c(all_bill_ids, bill_ids)
    dyns = matrix(0, nrow=nbills, ncol=dims)
    all_dyns = rbind(all_dyns, dyns)
    mids = matrix(0, nrow=nbills, ncol=dims)
    all_mids = rbind(all_mids, mids)
  }
  list(icong=all_sessions, inum=all_bill_ids,
       dyn=all_dyns, zmid=all_mids)
}

write_session_file = function(rc_list) {
  mcong = matrix(nrow=0, ncol=3)
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    mcong = rbind(mcong, c(session, rc$m, rc$n))
  }
  list(mcong=mcong)
}

write_start_file = function(rc_list, sessions, dims, model,
                            iters, beta, w) {
  ## Variables that would have been written to the DW-NOMSTART.DAT
  ## file if I was still creating it
  params1 = as.integer(c(dims, model, sessions, iters))
  if (dims == 1) {
    # no second dimension weight in this case
    weights = c(1, beta)
  } else {
    weights = c(1, w, beta)
  }

  list(nomstart_in=params1, weights=weights)
}

write_input_files = function(rc_list, start, sessions, dims,
                             model, niter, beta, w, lid, leg_dict) {
  # write the data files needed to run DW-NOMINATE
  ## message('Writing DW-NOMINATE input files...\n')
  params5 = write_rc_data_file(rc_list, lid, leg_dict)
  params3 = write_transposed_rc_data_file(rc_list)
  params4 = write_leg_file(rc_list, start, dims, lid, leg_dict)
  params1 = write_bill_file(rc_list, start, dims)
  params2 = write_session_file(rc_list)
  params = write_start_file(rc_list, sessions, dims, model,
                            niter, beta, w)
  c(params, params1, params2, params3, params4, params5)
}

make_leg_df = function(res, params, party_dict, leg_dict) {
  ## organize legislator data
  ndims = res[[1]][1]
  coords = paste0('coord', 1:ndims, 'D')
  if (ndims == 1) {
    leg_data = c(res[13:14], params[c('iparty', 'lname')], res[20], res[25:28])
    legnames = c('session', 'ID', 'party', 'name', coords, 'loglikelihood',
                 'loglikelihood_check', 'numVotes', 'numVotes_check',
                 'numErrors', 'numErrors_check', 'GMP', 'GMP_check')
  } else {
    leg_data = c(res[13:14], params[c('iparty', 'lname')], res[20:28])
    ses = paste0('se', 1:ndims, 'D')
    vars = paste0('var', 1:ndims, 'D')
    legnames = c('session', 'ID', 'party', 'name', coords, ses, vars,
                 'loglikelihood', 'loglikelihood_check', 'numVotes',
                 'numVotes_check', 'numErrors', 'numErrors_check', 'GMP',
                 'GMP_check')
  }
  df = do.call(cbind.data.frame, leg_data)
  names(df) = legnames
  df$party = names(party_dict)[df$party]
  df$ID = names(leg_dict)[df$ID]
  df = df[, -grep('_check$', names(df))]
  df
}

make_rc_df = function(res, params) {
  ## organize rollcall data
  df = cbind.data.frame(res[[4]], params$inum, res[[30]], res[[29]])
  ## fix df names
  ndims = res[[1]][1]
  midcols = paste0('midpoint', 1:ndims, 'D')
  spreadcols = paste0('spread', 1:ndims, 'D')
  est_cols = c(midcols, spreadcols)
  names(df) = c('session', 'ID', est_cols)
  ## replace zeros with NA's
  nas = which(apply(df[, est_cols], 1, function(x) all(x == 0)))
  nacols = 1:(ndims * 2) + 2
  df[nas, nacols] = NA
  df
}

## a convenient wrapper around wnominate that automatically finds
## valid polarity arguments
#' @importFrom utils capture.output
auto_wnominate = function(rc, ...) {
  wnom_args = list(...)
  ## get the minimum number of votes
  if ('minvotes' %in% names(wnom_args)) {
    wnom_minvotes = wnom_args$minvotes
  } else {
    wnom_minvotes = 20
  }
  if ('dims' %in% names(wnom_args)) {
    wnom_dims = wnom_args$dims
  } else {
    wnom_dims = 2
  }
  ## get the first legislator with at least the minimum number of
  ## votes
  vote_codes = c(rc$codes$yea, rc$codes$nay)
  vmat = rc$votes
  vmat[!vmat %in% vote_codes] = NA
  leg_has_min = rowSums(!is.na(vmat)) >= wnom_minvotes
  if (length(which(leg_has_min)) == 0) {
    stop('No legislators meet minimum vote requirements.')
  }
  leg_n = which(leg_has_min)[1]
  capture.output({
    res = wnominate::wnominate(rc, polarity = rep(leg_n, wnom_dims), ...)
  })
  res
}

#' Run DW-NOMINATE
#'
#' Estimate legislator ideal points using the DW-NOMINATE roll call scaling
#' procedure featured in \insertCite{poole_ideology_2011;textual}{dwnominate}.
#'
#' DW-NOMINATE was introduced in
#' \insertCite{mccarty_income_1997;textual}{dwnominate}. It's an extension of
#' the older D-NOMINATE program \insertCite{poole_patterns_1991}{dwnominate}
#' that uses the "weighted" distance model first used by W-NOMINATE.
#'
#' The model assumes that legislators have Guassian utility functions centered
#' around their ideal points, along with an additional roll call-specific random
#' utility that makes the voting decisions probabilistic rather than
#' deterministic.
#'
#' The returned values are a maximum likelihood estimate of the model parameters
#' (with minor exceptions around the boundary of the space). DW-NOMINATE uses a
#' three-step estimation procedure similar to expectation maximization to find
#' the maximum likelihood. The estimation procedure is a local optimization
#' algorithm, but the likelihood function is not globally convex, so the results
#' can be sensitive to starting estimates. Users must provide reasonable
#' starting values to get correct results. See
#' \code{vignette("starting-estimates", package = "dwnominate")} for more
#' discussion of the starting estimates.
#'
#' A fairly complete technical description of the procedure is available in
#' \insertCite{poole_spatial_2005;textual}{dwnominate}.
#'
#' @useDynLib dwnominate, .registration = TRUE
#' @param rc_list A list of \code{rollcall} objects from the
#'   \code{pscl} package, in chronological order.
#' @param id Column name in the rollcall objects' \code{legis.data}
#'   data frames providing a unique legislator ID. If not specified
#'   legislator names will be used.
#' @param start A roll call scaling result of class \code{common space},
#'   \code{wnominate}, \code{oc}, or \code{dwnominate} providing starting
#'   estimates of legislator ideologies. If not provided, dwnominate will
#'   calculate common space scores to get starting values. See the "details"
#'   section for more information about providing starting values.
#' @param sessions A vector of length 2 providing the first and last
#'   sessions to include. Defaults to \code{c(1, length(rc_list))}.
#' @param dims The number of dimensions to estimate. Can be either 1
#'   or 2.
#' @param model The degree of the polynomial representing changes in legislator
#'   ideology over time. \code{0} is constant, \code{1} is linear, \code{2} is
#'   quadratic and \code{3} is cubic. If \code{model} > 0, a minimum of
#'   \code{model} + 4 sessions is required.
#' @param niter Number of iterations. 4 iterations are typically
#'   enough for the results to converge.
#' @param beta Starting estimate of the parameter representing the
#'   spatial error in legislator choices.
#' @param w Starting estimate for the weight of the second
#'   dimension. The first dimension has a weight of 1, so w should be
#'   <= 1.
#' @param polarity A vector of length 1 or \code{dims} specifying, for
#'   each dimension, a legislator who should have a positive
#'   coordinate value. Legislators can be specified either by name or
#'   ID. If unspecified the first legislator in the data is used.
#' @param ... Arguments passed to \code{wnominate} if starting
#'   estimates are calculated.
#' @return A list of class \code{dwnominate} containing: \itemize{
#'   \item{legislators} {A data frame of legislator information}
#'   \item{rollcalls} {A data frame of rollcall information}
#'   \item{start} {The \code{wnominate} or \code{oc} results used as
#'   starting points for DW-NOMINATE} }
#' @references \insertAllCited{}
#' @seealso \url{https://legacy.voteview.com/dw-nominate.htm} for the original
#'   fortran code and instructions. \code{\link{common_space}} to help generate
#'   starting estimates.
#' @examples
#' \donttest{
#' data(nhsenate)
#' results <- dwnominate(nhsenate)
#' plot(results)
#' }
#' @importFrom utils head tail
#' @export
dwnominate = function(rc_list, id=NULL, start=NULL, sessions=NULL,
                      dims=2, model=1, niter=4, beta=5.9539,
                      w=0.3463, polarity=NULL, ...) {
  if (!is.null(id)) {
    for (rc in rc_list) {
      if (!id %in% colnames(rc$legis.data)) {
        stop('specified id column must exist in all rollcall objects')
      }
    }
  }
  # make integer ID's to make DW-NOMINATE happy
  if (is.null(id)) {
    id = 'dwnominate_tmp_ID'
    for (n in 1:length(rc_list)) {
      rc_list[[n]]$legis.data[, id] = get_leg_names(rc_list[[n]])
    }
    get_names_fun = get_leg_names
  } else {
    get_names_fun = function(x) x$legis.data[, id]
  }
  leg_names = unique(unlist(lapply(rc_list, get_names_fun)))
  leg_dict = setNames(1:length(leg_names), as.character(leg_names))
  if (!dims %in% 1:2) stop('dims must be either 1 or 2')
  if (!model %in% 0:3) stop('model must be between 0 and 3')
  nrc = length(rc_list)
  if (nrc < 2)
    stop('rc_list must contain at least 2 rollcall objects')
  if (is.null(sessions))
    sessions = c(1, nrc)
  if (any(sessions < 1) || any(sessions > nrc))
    stop('sessions must be between 1 and length(rc_list)')
  # Iteration 1 is an OC analysis for the roll call parameters according to
  # https://legacy.voteview.com/dw-nominate.htm. Since the code currently
  # doesn't pass roll call parameter starting estimates to fortran, this makes
  # sense to run. In the future when I pass the roll call inputs to fortran I
  # should change this to start at 2.
  iters = c(1, niter + 1)
  # should check that membership overlaps
  
  parties = unique(unlist(lapply(rc_list,
      function(x) x$legis.data$party)))
  party_dict = setNames(1:length(parties), parties)
  
  for (n in 1:length(rc_list)) {
    rc_list[[n]]$legis.data$party =
      party_dict[as.character(rc_list[[n]]$legis.data$party)]
  }
  get_start = is.null(start)
  if (!get_start) {
    if (inherits(start, 'OCobject') && start$dimensions == 1) {
      stop("Can't use optimal classification results if estimated with only one dimension.")
    }
    if (start$dimensions < dims) {
      stop("Dimensions in starting estimates cannot be less than 'dims'.")
    }
    if (is.null(id) && inherits(start, 'dwnominate')) {
      stop("An id argument must be provided with dwnominate starting estimates.")
    }
  }
  if (model > 0 && nrc < model + 4) {
    model_names = c("constant", "linear", "quadratic", "cubic")
    orig_model = model_names[model + 1]
    model = max(0, nrc - 4)
    new_model = model_names[model + 1]
    warning("Too few sessions for ", orig_model, " model, setting model to ",
            new_model)
  }
  if (get_start) {
    if (nrc < 5)
      stop("At least 5 sessions are required to automatically create starting values")
    message(paste('Calculating W-NOMINATE scores for each session...'))
    nom_list = lapply(rc_list, auto_wnominate, ...)
    message(paste('Extracting common space scores...'))
    start = common_space(nom_list, id, dims, polarity = polarity)
  }
  
  params = write_input_files(rc_list, start, sessions, dims,
                             model, iters, beta, w, id, leg_dict)
  
  # run DW-NOMINATE
  message('Running DW-NOMINATE...')
  # change line 40 of DW-NOMINATE.FOR !!
  nomstart = file.path(getwd(), 'DW-NOMSTART.DAT')
  start_time = Sys.time()
  nbills = length(params$icong)
  nlegs = length(params$ncong)
  xdata = matrix(0.0, nrow=nlegs, ncol=dims)
  sdx1 = rep(0.0, nlegs)
  sdx2 = rep(0.0, nlegs)
  varx1 = rep(0.0, nlegs)
  varx2 = rep(0.0, nlegs)
  xbiglog = matrix(0.0, nrow=nlegs, ncol=2)
  kbiglog = matrix(0L, nrow=nlegs, ncol=4)
  gmpa = rep(0.0, nlegs)
  gmpb = rep(0.0, nlegs)
  dyn = matrix(0.0, nrow=nbills, ncol=dims)
  zmid = matrix(0.0, nrow=nbills, ncol=dims)
  weights = vector(mode = 'numeric', length = length(params$weights))
  res = .Fortran('dwnom',
           ## control file (DW-NOMSTART.DAT) params:
           params$nomstart_in, params$weights,
           ## bill file (rollcall_input.dat) params:
           nbills, params$icong, params$dyn, params$zmid,
           ## session file (session_info.num) params:
           params$mcong,
           ## transposed rollcall file
           nrow(params$rcvotet1), ncol(params$rcvotet1),
           params$rcvotet1, params$rcvotet9,
           ## legislator input file
           nlegs, params$ncong, params$id1,
           params$xdata,
           ## rollcall file
           nrow(params$rcvote1), ncol(params$rcvote1),
           params$rcvote1, params$rcvote9,
           ## legislator output objects
           xdata, sdx1, sdx2, varx1, varx2,
           xbiglog, kbiglog, gmpa, gmpb,
           ## rollcall output objects
           dyn, zmid, weights)
  
  runtime = Sys.time() - start_time
  units(runtime) = 'mins'
  message(paste('DW-NOMINATE took', round(runtime, 1),
                'minutes.\n'))
  
  # organize the results
  results = list(legislators=make_leg_df(res, params, party_dict, leg_dict),
                 rollcalls=make_rc_df(res, params),
                 dimensions=dims,
                 beta = tail(res[[31]], 1),
                 weights = head(res[[31]], -1),
                 start=start)
  class(results) = 'dwnominate'
  results
}

#' Plot party means over time
#'
#' @param x An object returned by \code{dwnominate()}.
#' @param ... Arguments passed to \code{plot.default()}
#' @examples
#' \donttest{
#' data(nhsenate)
#' results <- dwnominate(nhsenate)
#' plot(results)
#' }
#' @importFrom graphics grid legend plot points
#' @importFrom grDevices palette.colors
#' @importFrom stats aggregate setNames
#' @export
plot.dwnominate = function(x, ...) {
  pch = 18
  legs = x$legislators
  uniq_parties = unique(legs$party)
  col_dict = setNames(palette.colors(length(uniq_parties), recycle = TRUE),
                      uniq_parties)
  ts1 = aggregate(legs$coord1D,
      by=list(legs$session, legs$party), FUN=mean, na.rm=T)
  plot(ts1$Group.1, ts1$x, ylim=c(-1, 1), type='n',
       main='DW-NOMINATE 1st Dimension',
       ylab='Party Means', xlab='Session', ...)
  grid(NA, NULL, col='#666666')
  for (party in unique(ts1$Group.2)) {
    ts2 = ts1[ts1$Group.2 == party, ]
    points(ts2$Group.1, ts2$x, col=col_dict[party],
           type='o', pch=pch)
  }
  legend('topleft', names(col_dict), fill=col_dict,
         bg='white', horiz=T)
}

#' Merge rollcall objects
#'
#' Merge a list of \code{rollcall} objects.
#' 
#' @param rollcalls A list of \code{rollcall} objects.
#' @param by The column in legis.data to use as an ID for matching
#'   legislators. If not provided legislators are matched based on
#'   name.
#' @examples
#' data(nhsenate)
#' combined_rcs <- merge_rollcalls(nhsenate, by='name')
#' print(combined_rcs)
#' @export
merge_rollcalls = function(rollcalls, by=NULL) {
  rc1 = rollcalls[[1]]
  # check that the codes are the same

  # merge them all!
  if (is.null(by)) {
    # use legislator names as ID
    leg_ids = lapply(rollcalls, get_leg_names)
  } else {
    leg_ids = lapply(rollcalls, function(x) x$legis.data[, by])
  }
  rows = unique(unlist(leg_ids))
  
  legs0 = do.call(rbind, lapply(rollcalls, function(x) x$legis.data))
  cols = unlist(lapply(1:length(rollcalls), function(x)
    paste0('RC', x, ' ', colnames(rollcalls[[x]]$votes))))
  # default vote values are notInLegis
  votes = matrix(rc1$codes$notInLegis[1],
                 nrow=length(rows), ncol=length(cols),
                 dimnames=list(rows, cols))
  rc_bounds = cumsum(unlist(lapply(rollcalls, function(x) ncol(x$votes))))
  rc_bounds = c(0, rc_bounds)
  for (n in 1:length(rollcalls)) {
    if (is.null(by)) {
      rowns = match(leg_ids[[n]], rows)
    } else {
      rowns = match(rollcalls[[n]]$legis.data[, by], rows)
    }
    colns = (rc_bounds[n] + 1):rc_bounds[n + 1]
    votes[rowns, colns] = rollcalls[[n]]$votes
    ## # put the votes in a standard format ?
    ## codes = unlist(rc$codes)
    ## dict1 = setNames(c(1, 0, NA, 9),
    ##                  c('yea', 'nay', 'notInLegis', 'missing'))
    ## dict2 = setNames(gsub('[0-9]', '', names(codes)),
    ##                  codes)
    ## dict = setNames(dict1[dict2], names(dict2))
    # what to do about identical bill names?
  }
  sources = unique(unlist(lapply(rollcalls, function(x) x$source)))
  if (is.null(by)) {
    legs = legs0[match(rows, row.names(legs0)), ]
  } else {
    legs = legs0[match(rows, legs0[, by]), ]
  }
  vote_data = do.call(rbind, lapply(rollcalls, function(x) x$vote.data))

  rc = pscl::rollcall(votes, yea=rc1$codes$yea,
                      nay=rc1$codes$nay,
                      missing=rc1$codes$missing,
                      notInLegis=rc1$codes$notInLegis,
                      legis.names=row.names(votes),
                      vote.names=colnames(votes),
                      legis.data=legs,
                      source=sources)
  if (!is.null(vote_data)) rc$vote.data = vote_data
  rc
}
