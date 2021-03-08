# Write input data files to, and read output files from, the original format
# needed to run DW-NOMINATE as a standalone command line program

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
    parties = format_column(rcl, 'notparty', '%4d', 0)
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
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    votes = rc_list[[session]]$votes
    codes = rc_list[[session]]$codes
    sessions = sprintf('%4d', session)
    vote_ids = sprintf('%5d', 1:ncol(votes))
    
    votes[votes %in% codes$yea] = 1
    votes[votes %in% codes$nay] = 6
    votes[votes %in% c(codes$notInLegis, codes$missing)] = 9

    # a vector of very long lists of numbers, one for each legislator
    vote_nums =
      apply(votes, 2, FUN=function(x) paste(x, collapse=''))

    # the lines to be written to the file
    'I4,I5,1X,600I1'
    lines = paste0(sessions, vote_ids, ' ', vote_nums)
    all_lines = c(all_lines, lines)
  }
  writeLines(all_lines, 'transposed_rollcall_matrix.vt3')
}

write_leg_file = function(rc_list, wnom, dims, lid) {
  all_lines = vector()
  coordcols = paste0('coord', 1:dims, 'D')
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    rcl = rc$legis.data
    'I4,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,2F7.3,I5'
    sessions = format_column(rcl, 'sessionID', '%4d', session)
    leg_ids = sprintf('%6d', rcl[, lid])
    state_num = format_column(rcl, 'icpsrState', '%3d', 0)
    district = format_column(rcl, 'cd', '%2d', 0)
    state_name = format_column(rcl, 'state', '%-7s', 'NA')
    parties = format_column(rcl, 'notparty', '%4d', 0)
    leg_names = sprintf('%-11.11s', fix_string(rownames(rc$votes)))
    # find the corresponding starting coordinates for each legislator
    matches = match(rcl[, lid], wnom$legislators[, lid])
    coords = as.matrix(wnom$legislators[matches, coordcols])
    # DW-NOMINATE hates NA's
    coords[is.na(coords)] = 0
    coords = sprintf('%7.3f', coords)
    coords = matrix(coords, ncol=dims)
    coords = apply(coords, 1, paste0, collapse='') # finally!

    # the lines to be written to the file
    'I4,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,2F7.3,I5'
    lines = paste0(sessions, leg_ids, state_num, district, ' ',
                   state_name, ' ', parties, ' ', leg_names,
                   coords)
    all_lines = c(all_lines, lines)
  }
  writeLines(all_lines, 'legislator_input.dat')
}

write_bill_file = function(rc_list, wnom, dims) {
  all_lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    'I3,I5,4F7.3'
    sessions = sprintf('%3d', session)
    bill_ids = sprintf('%5d', 1:ncol(rc$votes))
    spreadmids = paste(rep(sprintf('%7.3f', 0), 2 * dims),
                       collapse='')
    # the lines to be written to the file
    lines = paste0(sessions, bill_ids, spreadmids)
    all_lines = c(all_lines, lines)
  }
  ## all_lines = paste0(all_lines, spreadmids)
  writeLines(all_lines, 'rollcall_input.dat')
}

write_session_file = function(rc_list) {
  lines = vector()
  for (session in 1:length(rc_list)) {
    rc = rc_list[[session]]
    session_num = sprintf('%3d', session)
    rollcalls = sprintf('%4d', rc$m)
    legislators = sprintf('%3d', rc$n)

    # the lines to be written to the file
    lines[session] = paste(session_num, rollcalls, legislators)
  }
  writeLines(lines, 'session_info.num')
}

write_start_file = function(rc_list, sessions, dims, model,
                            iters, beta, w) {
  filenames = c('rollcall_input.dat', 'rollcall_output.dat',
      'legislator_input.dat', 'legislator_output.dat',
      'session_info.num', 'rollcall_matrix.vt3',
      'transposed_rollcall_matrix.vt3')
  params1 = c(dims, model, sessions, iters)
  params1s = paste(sprintf('%5d', params1), collapse='')
  betas = sprintf('%8.4f', beta)
  ws = paste(sprintf('%8.4f', w), collapse='')
  lines = filenames
  lines[8] = 'NOMINAL DYNAMIC-WEIGHTED MULTIDIMENSIONAL UNFOLDING '
  lines[9] = params1s
  lines[10] = paste0(betas, ws)
  writeLines(lines, 'DW-NOMSTART.DAT')
}

write_input_files = function(rc_list, wnom, sessions, dims,
                             model, niter, beta, w, lid) {
  # write the data files needed to run DW-NOMINATE
  message('Writing DW-NOMINATE input files...\n')
  write_rc_data_file(rc_list, lid)
  write_transposed_rc_data_file(rc_list)
  write_leg_file(rc_list, wnom, dims, lid)
  write_bill_file(rc_list, wnom, dims)
  write_session_file(rc_list)
  write_start_file(rc_list, sessions, dims, model,
                   niter, beta, w)
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
    legnames = c('session', 'ID', 'stateID', 'district', 'state',
                 'partyID', 'name', coords, 'loglikelihood',
                 'loglikelihood_check', 'numVotes', 'numVotes_check',
                 'numErrors', 'numErrors_check', 'GMP', 'GMP_check')
    fdims1 = 'F7'
  } else {
    legnames = c('session', 'ID', 'stateID', 'district', 'state',
                 'partyID', 'name', coords, ses, vars, 'loglikelihood',
                 'loglikelihood_check', 'numVotes', 'numVotes_check',
                 'numErrors', 'numErrors_check', 'GMP', 'GMP_check')
    fdims1 = paste0(dims * 3, 'F7')
  }

  format1 = c('I4', 'I6', 'I3', 'I2', 'X', 'A7', 'X',
              'I4', 'X', 'A11', fdims1, '2F12', '4I5', '2F7')
  legs = utils::read.fortran('legislator_output.dat', format1,
                             col.names=legnames, na.strings=nas,
                             # ignore earlier iterations
                             skip=(iters[2] - iters[1]) * nunlegs)

  # legs$party = names(party_dict)[legs$partyID] # doesn't matter
  legs$party = 'party'

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
