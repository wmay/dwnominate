# an interface to Keith Poole's DW-NOMINATE FORTRAN77 program.

# limitations:
# 600 legislators per session
# 3600 votes per session


# get legislator names from rollcall objects
get_leg_names = function(x) row.names(x$legis.data)

fix_string = function(string) gsub("[^A-Za-z ()'-]", '', string)

format_column = function(df, name, format, alternative=NULL) {
  if (name %in% names(df)) {
    if (class(df[, name]) == 'character') {
      sprintf(format, fix_string(df[, name]))
    } else {
      sprintf(format, df[, name])
    }
  } else {
    sprintf(format, alternative)
  }
}

write_rc_data_file = function(rc_list, lid) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    rcl = rc$legis.data
    votes = rc$votes
    codes = rc$codes
    sessions = format_column(rcl, 'sessionID', '%4d', session)
    leg_ids = sprintf('%6d', rcl[, lid])
    state_num = format_column(rcl, 'icpsrState', '%3d', 0)
    district = format_column(rcl, 'cd', '%2d', 0)
    state_name = format_column(rcl, 'state', '%-7s', 'NA')
    parties = format_column(rcl, 'party', '%4d', 0)
    leg_names = sprintf('%-11.11s', fix_string(rownames(votes)))
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9
    
    # a vector of very long lists of numbers, one for each legislator
    vote_nums =
      apply(votes, 1, FUN=function(x) paste(x, collapse=''))

    # the lines to be written to the file
    'I4,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,3600I1'
    lines = paste0(sessions, leg_ids, state_num, district, ' ',
        state_name, ' ', parties, ' ', leg_names, vote_nums)
    all_lines = c(all_lines,  lines)
  }
  
  writeLines(all_lines, 'rollcall_matrix.vt3')
}

write_transposed_rc_data_file = function(rc_list) {
  session_rows = sapply(rc_list, function(x) ncol(x$votes))
  session_cols = sapply(rc_list, function(x) nrow(x$votes))
  total_rcs = sum(session_rows)
  max_rcs = max(session_cols)
  m1 = matrix(FALSE, nrow=total_rcs, ncol=max_rcs)
  m9 = matrix(FALSE, nrow=total_rcs, ncol=max_rcs)
  ## all_lines = vector()
  start_row = 1
  for (session in 1:length(rc_list)) {
    votes = t(rc_list[[session]]$votes)
    codes = rc_list[[session]]$codes
    ## sessions = sprintf('%4d', session)
    ## vote_ids = sprintf('%5d', 1:ncol(votes))
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9

    end_row = start_row + session_rows[session] - 1
    end_col = session_cols[session]
    m1[start_row:end_row, 1:end_col] = votes == 1
    m9[start_row:end_row, 1:end_col] = votes == 9

    start_row = end_row + 1

    ## # a vector of very long lists of numbers, one for each legislator
    ## vote_nums =
    ##   apply(votes, 2, FUN=function(x) paste(x, collapse=''))

    ## # the lines to be written to the file
    ## 'I4,I5,1X,600I1'
    ## lines = paste0(sessions, vote_ids, ' ', vote_nums)
    ## all_lines = c(all_lines, lines)
  }
  ## writeLines(all_lines, 'transposed_rollcall_matrix.vt3')
  list(rcvotet1=m1, rcvotet9=m9)
}

write_leg_file = function(rc_list, start, dims, lid) {
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
    rcl = rc$legis.data
    n_rcl = nrow(rcl)
    all_sessions = c(all_sessions, rep(session, n_rcl))
    all_leg_ids = c(all_leg_ids, rcl[, lid])
    all_state_nums = c(all_state_nums,
                       ifelse(is.na(rcl$icpsrState), 0, rcl$icpsrState))
    all_districts = c(all_districts, ifelse(is.na(rcl$cd), 0, rcl$cd))
    ksta = c(ksta, as.character(rcl$state))
    all_parties = c(all_parties, ifelse(is.na(rcl$party), 0, rcl$party))
    lnames = c(lnames, rownames(rc$votes))
    matches = match(rcl[, lid], start$legislators[, lid])
    coords = as.matrix(start$legislators[matches, coordcols])
    # DW-NOMINATE hates NA's
    coords[is.na(coords)] = 0
    all_coords = rbind(all_coords, coords)
  }
  list(ksta=ksta, lname=lnames,
       ncong=as.integer(all_sessions),
       id1=as.integer(all_leg_ids),
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
  weights = c(1, w, beta)
  list(nomstart_in=params1, weights=weights)
}

write_input_files = function(rc_list, start, sessions, dims,
                             model, niter, beta, w, lid) {
  # write the data files needed to run DW-NOMINATE
  message('Writing DW-NOMINATE input files...\n')
  write_rc_data_file(rc_list, lid)
  params3 = write_transposed_rc_data_file(rc_list)
  params4 = write_leg_file(rc_list, start, dims, lid)
  params1 = write_bill_file(rc_list, start, dims)
  params2 = write_session_file(rc_list)
  params = write_start_file(rc_list, sessions, dims, model,
                            niter, beta, w)
  c(params, params1, params2, params3, params4)
}

read_output_files = function(party_dict, dims, iters, nunlegs,
                             nunrcs) {
  # column names based on DW-NOMINATE fortran code and info found
  # here: http://voteview.com/rohde.htm
  nas = c('**', '***', '****', '*****', '******',
          '*******', '************')
  coords = paste0('coord', 1:dims, 'D')
  ses = paste0('se', 1:dims, 'D')
  vars = paste0('var', 1:dims, 'D')
  if (dims == 1) {
    legnames = c('session', 'ID', 'stateID', 'district',
                 'partyID', coords, 'loglikelihood',
                 'loglikelihood_check', 'numVotes', 'numVotes_check',
                 'numErrors', 'numErrors_check', 'GMP', 'GMP_check')
    fdims1 = 'F7'
  } else {
    legnames = c('session', 'ID', 'stateID', 'district',
                 'partyID', coords, ses, vars, 'loglikelihood',
                 'loglikelihood_check', 'numVotes', 'numVotes_check',
                 'numErrors', 'numErrors_check', 'GMP', 'GMP_check')
    fdims1 = paste0(dims * 3, 'F7')
  }

  format1 = c('I4', 'I6', 'I3', 'I2',
              'I4', fdims1, '2F12', '4I5', '2F7')
  legs = utils::read.fortran('legislator_output.dat', format1,
                             col.names=legnames, na.strings=nas,
                             # ignore earlier iterations
                             skip=(iters[2] - iters[1]) * nunlegs)

  legs$party = names(party_dict)[legs$partyID]

  rcdims = paste0(c('midpoint', 'spread'),
                   rep(1:dims, each=2), 'D')
  rcnames = c('session', 'ID', rcdims)
  fdims2 = paste0(dims * 2, 'F7')
  format2 = c('I3', 'I5', fdims2)
  rcs = utils::read.fortran('rollcall_output.dat', format2,
                            col.names=rcnames, na.strings=nas,
                            # ignore earlier iterations
                            skip=(iters[2] - iters[1]) * nunrcs)

  nas = which(rcs$spread1D == 0 & rcs$midpoint1D == 0 &
              rcs$spread2D == 0 & rcs$midpoint2D == 0)
  nacols = 1:(dims * 2) + 2
  rcs[nas, nacols] = NA

  list(legislators=legs, rollcalls=rcs)
}

#' Run DW-NOMINATE
#'
#' @useDynLib dwnominate dwnom
#' @param rc_list A list of \code{rollcall} objects from the
#'   \code{pscl} package, in chronological order.
#' @param id Column name in the rollcall objects' \code{legis.data}
#'   data frames providing a unique legislator ID. If not specified
#'   legislator names will be used.
#' @param start A \code{wnominate} or \code{oc} result object (class
#'   \code{nomObject} or \code{OCobject}) providing starting estimates
#'   of legislator ideologies. If not provided, dwnominate will run
#'   \code{wnominate} or \code{oc} to get starting values.
#' @param sessions A vector of length 2 providing the first and last
#'   sessions to include. Defaults to \code{c(1, length(rc_list))}.
#' @param dims The number of dimensions to estimate. Can be either 1
#'   or 2.
#' @param model The degree of the polynomial representing changes in
#'   legislator ideology over time. \code{0} is constant, \code{1} is
#'   linear, \code{2} is quadratic and \code{3} is cubic.
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
#' @param ... Arguments passed to \code{wnominate} (if dims==1) or
#'   \code{oc} (if dims>1).
#' @return A list of class \code{dwnominate} containing: \itemize{
#'   \item{legislators} {A data frame of legislator information}
#'   \item{rollcalls} {A data frame of rollcall information}
#'   \item{start} {The \code{wnominate} or \code{oc} results used as
#'   starting points for DW-NOMINATE} }
#' @references Keith Poole. 2005. 'Spatial Models of Parliamentary
#'   Voting.' New York: Cambridge University Press.
#' @examples
#' # US Senate data originally from voteview.com
#' data(senate)
#' results <- dwnominate(senate)
#' plot(results)
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
  if (is.null(id)) {
    # make some ID's to make DW-NOMINATE happy
    leg_names = unique(unlist(lapply(rc_list, get_leg_names)))
    id = 'dwnomID'
    for (n in 1:length(rc_list)) {
      rc_list[[n]]$legis.data[, id] =
        match(get_leg_names(rc_list[[n]]), leg_names)
    }
  }
  if (!dims %in% 1:2) stop('dims must be either 1 or 2')
  if (!model %in% 0:3) stop('model must be between 0 and 3')
  nrc = length(rc_list)
  if (nrc < 2)
    stop('rc_list must contain at least 2 rollcall objects')
  if (is.null(sessions))
    sessions = c(1, nrc)
  if (any(sessions < 1) || any(sessions > nrc))
    stop('sessions must be between 1 and length(rc_list)')
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
  if (!is.null(start)) {
    if (class(start) == 'OCobject' && start$dimensions == 1) {
      warning("Can't use optimal classification results if estimated with only one dimension. Getting new starting estimates...")
      get_start = T
    }
    if (start$dimensions < dims) {
      warning('Too few dimensions in oc/wnominate object. Getting new starting estimates...')
      get_start = T
    }
  } else {
    scale_type = ifelse(dims > 1, 'Optimal Classification', 'W-NOMINATE')
    message(paste('Running', scale_type, 'to get starting estimates...'))
  }
  if (get_start) {
    rc_all = merge.rollcall(rc_list=rc_list)
    scale_func = ifelse(dims > 1, oc::oc, wnominate::wnominate)
    if (!is.null(polarity)) {
      if (length(polarity) != dims && length(polarity) != 1)
        stop('polarity should have a length of 1 or dims')
      polarityn = match(polarity, rc_all$legis.data[, id])
      if (any(is.na(polarityn)))
        polarityn = match(polarity, get_leg_names(rc_all))
      if (any(is.na(polarityn)))
        stop('polarity values must be either IDs or legislator names')
      if (length(polarityn) == 1)
        polarityn = rep(polarityn, dims)
    } else {
      polarityn = rep(1, dims)
    }
    # phwew.
    start = scale_func(rc_all, dims=dims, polarity=polarityn, ...)
  }
  
  params = write_input_files(rc_list, start, sessions, dims,
                             model, iters, beta, w, id)
  
  # run DW-NOMINATE
  # change line 40 of DW-NOMINATE.FOR !!
  nomstart = file.path(getwd(), 'DW-NOMSTART.DAT')
  start_time = Sys.time()
  .Fortran('dwnom',
           ## control file (DW-NOMSTART.DAT) params:
           params$nomstart_in, params$weights,
           ## bill file (rollcall_input.dat) params:
           length(params$icong), params$icong, params$inum,
           params$dyn, params$zmid,
           ## session file (session_info.num) params:
           params$mcong,
           ## transposed rollcall file
           nrow(params$rcvotet1), ncol(params$rcvotet1),
           params$rcvotet1, params$rcvotet9,
           ## legislator input file
           length(params$ncong), params$ncong, params$id1,
           params$istate, params$idist, params$iparty,
           params$xdata)
  runtime = Sys.time() - start_time
  units(runtime) = 'mins'
  message(paste('DW-NOMINATE took', round(runtime, 1),
                'minutes.\n'))
  
  # get the results
  nunlegs = sum(sapply(rc_list, function(x) x$n))
  nunrcs = sum(sapply(rc_list, function(x) x$m))
  results = read_output_files(party_dict, dims, iters, nunlegs,
                              nunrcs)
  ## add states and legislator names to legislator results
  results$legislators = cbind(params$ksta, params$lname, results$legislators)
  names(results$legislators)[1:2] = c('state', 'name')
  results$start = start
  class(results) = 'dwnominate'
  results
}

#' Plot party means over time
#'
#' @param x An object returned by \code{dwnominate()}.
#' @param ... Arguments passed to \code{plot.default()}
#' @examples
#' # US Senate data from voteview.com
#' data(senate)
#' dwnom <- dwnominate(senate)
#' plot(dwnom)
#' @importFrom graphics grid legend plot points
#' @importFrom stats aggregate setNames
#' @export
plot.dwnominate = function(x, ...) {
  pch = 18
  legs = x$legislators
  uniq_parties = unique(legs$party)
  col_dict = setNames(grDevices::rainbow(length(uniq_parties)),
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

#' Sessions 109-113 of the US Senate
#'
#' Rollcall votes of Senators in the 109-113th sessions of the US
#' Senate.
#'
#' @format A list of rollcall objects from the pscl package
#'
#' @source \href{http://www.voteview.com/dwnl.html}{voteview.com}
#'
#' @examples
#' # US Senate data from voteview.com
#' data(senate)
#'
#' library(wnominate)
#' wnom <- wnominate(senate[[1]], polarity = 1:2)
#' plot(wnom)
#'
#' dwnom <- dwnominate(senate)
#' plot(dwnom)
'senate'


#' Merge rollcall objects
#'
#' Merge x and y rollcall objects, or a list of rollcall objects.
#' 
#' @param x An object of class \code{dwnominate()}.
#' @param y An object of class \code{dwnominate()}.
#' @param rc_list An list of object of \code{dwnominate()} objects.
#' @param by The column in legis.data to use as an ID for matching
#'   legislators. If not provided legislators are matched based on
#'   name.
#' @examples
#' library(pscl) # for the rollcall summary method
#' data(senate)
#' # merge using ICPSR legislator ID's
#' combined_rcs = merge(senate[[1]], senate[[2]], by='icpsrLegis')
#' summary(combined_rcs)
#' @export
merge.rollcall = function(x=NULL, y=NULL, rc_list=NULL,
                          by=NULL) {
  if (is.null(rc_list)) rc_list = list(x, y)
  rc1 = rc_list[[1]]
  # check that the codes are the same

  # merge them all!
  if (is.null(by)) {
    # use legislator names as ID
    leg_ids = lapply(rc_list, get_leg_names)
  } else {
    leg_ids = lapply(rc_list, function(x) x$legis.data[, by])
  }
  rows = unique(unlist(leg_ids))
  
  legs0 = do.call(rbind, lapply(rc_list, function(x) x$legis.data))
  cols = unlist(lapply(1:length(rc_list), function(x)
    paste0('RC', x, ' ', colnames(rc_list[[x]]$votes))))
  # default vote values are notInLegis
  votes = matrix(rc1$codes$notInLegis[1],
                 nrow=length(rows), ncol=length(cols),
                 dimnames=list(rows, cols))
  rc_bounds = cumsum(unlist(lapply(rc_list, function(x) ncol(x$votes))))
  rc_bounds = c(0, rc_bounds)
  for (n in 1:length(rc_list)) {
    if (is.null(by)) {
      rowns = match(leg_ids[[n]], rows)
    } else {
      rowns = match(rc_list[[n]]$legis.data[, by], rows)
    }
    colns = (rc_bounds[n] + 1):rc_bounds[n + 1]
    votes[rowns, colns] = rc_list[[n]]$votes
    ## # put the votes in a standard format ?
    ## codes = unlist(rc$codes)
    ## dict1 = setNames(c(1, 0, NA, 9),
    ##                  c('yea', 'nay', 'notInLegis', 'missing'))
    ## dict2 = setNames(gsub('[0-9]', '', names(codes)),
    ##                  codes)
    ## dict = setNames(dict1[dict2], names(dict2))
    # what to do about identical bill names?
  }
  sources = unique(unlist(lapply(rc_list, function(x) x$source)))
  if (is.null(by)) {
    legs = legs0[match(rows, row.names(legs0)), ]
  } else {
    legs = legs0[match(rows, legs0[, by]), ]
  }
  vote_data = do.call(rbind, lapply(rc_list, function(x) x$vote.data))

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
