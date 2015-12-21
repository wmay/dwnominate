# an interface to Keith Poole's DW-NOMINATE FORTRAN77 program.

library(pscl)
library(wnominate)

write_rc_data_file = function(rc_list) {
  lines = vector()
  session = 1
  for (n in 1:length(rc_list)) {
    rc = rc_list[[n]]
    votes = rc$votes
    sessions = sprintf("%4.0f", session)
    leg_ids = sprintf("%5.0f", 1:nrow(votes))
    state_num = sprintf("%2.0f", 1)
    district = sprintf("%1.0f", 0)
    state_name = sprintf("%-10s", "Bob")
    parties = sprintf("%1.1s", rc$legis.data[["party"]])
    leg_names = sprintf("%-10.10s",
        attr(rc$votes, "dimnames")$"Legislator ID")
    
    # a dictionary to translate votes into the appropriate numbers
    # required by DW-NOMINATE
    vote_dict = setNames(c(1, 6, 9, 0), c("yes", "no", "other", "NA"))
    votes[is.na(votes)] = "NA"
    # instead of doing that vote dictionary, replace text with nums
    # directly:

    # a vector of very long lists of numbers, one for each legislator
    vote_nums = apply(votes, 1, FUN = function(x) paste0(vote_dict[x],
                                    collapse = ""))

    # the lines to be written to the file
    lines = paste(sessions, leg_ids, state_num, district, state_name,
        parties, leg_names, vote_nums)
    session = session + 1
  }
  
  writeLines(lines, "rollcall_matrix.vt3")
}

write_transposed_rc_data_file = function(rc_list) {
  lines = vector()
  session = 1
  for (n in 1:length(rc_list)) {
    rc = rc_list[[n]]
    votes = rc$votes
    vote_ids = sprintf("%5.0f", 1:ncol(votes))
    sessions = sprintf("%4.0f", session)
    
    # a dictionary to translate votes into the appropriate numbers
    # required by DW-NOMINATE
    vote_dict = setNames(c(1, 6, 9, 0), c("yes", "no", "other", "NA"))
    votes[is.na(votes)] = "NA"
    # instead of doing that vote dictionary, replace text with nums
    # directly:

    # a vector of very long lists of numbers, one for each legislator
    vote_nums = apply(votes, 2, FUN = function(x) paste0(vote_dict[x],
                                    collapse = ""))

    # the lines to be written to the file
    lines = paste(sessions, vote_ids, vote_nums)
    session = session + 1
  }
  
  writeLines(lines, "transposed_rollcall_matrix.vt3")
}

write_leg_file = function(rc_list) {
  for (session in 1:length(rc_list)) {
    ## flipx = F
    ## flipy = F
    rc = rc_list[[session]]
    votes = rc$votes
    sessions = sprintf("%4.0f", session)
    leg_ids = sprintf("%5.0f", 1:nrow(votes))
    state_num = sprintf("%2.0f", 1)
    district = sprintf("%1.0f", 0)
    state_name = sprintf("%-10s", "Bob")
    parties = sprintf("%1.1s", rc$legis.data[["party"]])
    leg_names = sprintf("%-10.10s",
        attr(rc$votes, "dimnames")$"Legislator ID")
    
    ## # a dictionary to translate votes into the appropriate numbers
    ## # required by DW-NOMINATE
    ## vote_dict = setNames(c(1, 6, 9, 0), c("yes", "no", "other", "NA"))
    ## votes[is.na(votes)] = "NA"
    ## # instead of doing that vote dictionary, replace text with nums
    ## # directly:

    ## # a vector of very long lists of numbers, one for each legislator
    ## vote_nums = apply(votes, 1, FUN = function(x) paste0(vote_dict[x],
    ##                                 collapse = ""))

    # the lines to be written to the file
    lines = paste(sessions, leg_ids, state_num, district, state_name,
        parties, leg_names, xvotes, yvotes,
        " 0.000  0.000  0.000  0.000     0.00000     0.00000",
        numVotes*2 + numErrors*2 + dw_format(data[6])*2)
  }
  writeLines(lines, "legislator_input.dat")
}

write_input_files = function(rc_list, wnom_list) {
  # write the data files needed to run DW-NOMINATE
  write_rc_data_file(rc_list)
  write_transposed_rc_data_file(rc_list)
  session = 1
  
}

dwnominate = function(vote_matrix_list, wnom_list = NULL) {
  
}
