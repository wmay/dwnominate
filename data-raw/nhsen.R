# Get voting records from the New Hampshire Senate, a small legislative body
# ideal for documentation examples and unit testing. The New Hampshire General
# Court provides convenient access to its SQL Server legislative database, but
# it's missing info for past senators, so for now I'm scraping the website.
library(rvest)
library(pscl)

# These pages contain nested tables where each row contains a table (but with
# only one row!). This function restores table sanity by removing the redundant
# tags.
unnest_tables = function(s) {
  gsub('<td[^/]*<tr>|</tr>\\s*</table>\\s*(</b>)?(</font>)?</td>', '', s)
}

get_votes = function(pg) {
  pg$response %>%
    as.character %>%
    unnest_tables %>%
    read_html %>%
    html_node('#dlVotes') %>%
    html_table(header = TRUE, fill = TRUE) %>%
    # last row contains other info
    head(-1)
}

node_text = function(css, pg) {
  pg %>% html_node(css) %>% html_text
}

get_bio = function(pg) {
  list(name = node_text('#lblName', pg),
       party = node_text('#lblParty', pg),
       district = node_text('#lblLocation', pg)) %>%
    lapply(function(x) gsub('^[^ ]* ', '', x))
}

get_senator_info = function(senator, pg) {
  senator_pg = pg %>%
    html_node('#form1') %>%
    html_form %>%
    set_values(lbLegislators = senator) %>%
    submit_form(pg, ., submit = 'btnLegislatorSearch')
  list(bio = get_bio(senator_pg),
       votes = get_votes(senator_pg))
}

# Translate votes to pscl notation. The possible values from the server are
# 1: Yea
# 2: Nay
# 3: Excused
# 4: Not Excused
# 5: Conflict of Interest
# 6: Presiding
# 7: <blank> [possibly 'present'?]
# I'm translating 3-7 as missing (NA).
vote_dict = c(Yea = 1, Nay = 0)

get_year_rc = function(year, pg) {
  message('Collecting ', year, '...')
  year_pg = pg %>%
    html_node('#form1') %>%
    html_form %>%
    set_values(lbSenateSessionYears = year) %>%
    submit_form(pg, ., submit = 'btnSenateSearch')
  year_senators = year_pg %>%
    html_nodes('#lbLegislators option') %>%
    html_text
  info_list = lapply(year_senators, get_senator_info, pg = year_pg)
  legdf = data.frame(name = sapply(info_list, function(x) x$bio$name),
                     party = sapply(info_list, function(x) x$bio$party),
                     district = sapply(info_list, function(x) x$bio$district))
  rcdf = do.call(rbind, lapply(info_list, function(x) x$votes)) %>%
    subset(!duplicated(`Vote #`), select = -Vote)
  # Default is 9, code for "not in legislature". For all other cases I will
  # collect the vote (or absence reason) and overwrite the 9, either with 1 or 0
  # or NA.
  vote_mat = matrix(9, nrow = nrow(legdf), ncol = nrow(rcdf),
                    dimnames = list(Senator = legdf$name,
                                    `Vote #` = rcdf$`Vote #`))
  # fill in the vote matrix
  for (n in 1:nrow(legdf)) {
    svotes = info_list[[n]]$votes
    vote_mat[legdf$name[n], as.character(svotes$`Vote #`)] =
      vote_dict[svotes$Vote]
  }
  desc = paste0('Roll call votes from the New Hampshire Senate (', year, ')')
  rollcall(vote_mat, legis.names = legdf$name, vote.names = rcdf$`Vote #`,
           legis.data = legdf, vote.data = rcdf, desc = desc,
           source = 'http://gencourt.state.nh.us/nhgcrollcalls/')
}

start_pg = html_session('http://gencourt.state.nh.us/nhgcrollcalls/')
start_year = start_pg %>%
  html_nodes('#lbSenateSessionYears option') %>%
  html_text %>%
  as.integer %>%
  min
nhsen = lapply(start_year:2019, get_year_rc, pg = start_pg)
usethis::use_data(nhsen)


# Old SQL Server code, just in case they get it all working:

# # The New Hampshire General Court provides convenient access to its SQL Server
# # legislative database.

# # library(DBI)
# library(odbc)

# nhgc = dbConnect(odbc(), Driver = 'FreeTDS', Server = '66.211.150.69',
#                  Port = 1433, Database = 'NHLegislatureDB', UID = 'publicuser',
#                  PWD = 'PublicAccess')
# dbDisconnect(nhgc)
# # nhgcm = dbConnect(odbc(), Driver = 'FreeTDS', Server = '66.211.150.69',
# #                   Port = 1433, Database = 'master', UID = 'publicuser',
# #                   PWD = 'PublicAccess')
# # dbDisconnect(nhgcm)
# # nhgct = dbConnect(odbc(), Driver = 'FreeTDS', Server = '66.211.150.69',
# #                   Port = 1433, Database = 'tempdb', UID = 'publicuser',
# #                   PWD = 'PublicAccess')
# # dbDisconnect(nhgct) # ugh worthless
# # Important-sounding tables: tbllegislators (legislators), tbllsrs (bills),
# # tblrollcallsummary (roll calls), tblrollcallhistory (votes)

# dbListTables(nhgc)
# #  [1] "test"                                          "2019LSRs"                                     
# #  [3] "BodyStatusCodes"                               "CommitteeMembers"                             
# #  [5] "Committees"                                    "County"                                       
# #  [7] "District"                                      "Docket"                                       
# #  [9] "DocumentVersion"                               "GeneralStatusCodes"                           
# # [11] "Legislation"                                   "LegislationText"                              
# # [13] "Legislators"                                   "NH_RSA"                                       
# # [15] "RollCallHistory"                               "RollCallSummary"                              
# # [17] "Sponsors"                                      "StatStudDetails"                              
# # [19] "StatStudMeetings"                              "StatStudMembers"                              
# # [21] "Subject"                                       "Towns"                                        
# # [23] "vStatStudTemp"
# dbListFields(nhgc, 'Legislators')
# #  [1] "PersonID"        "LastName"        "FirstName"       "Employeeno"      "MiddleName"     
# #  [6] "LegislativeBody" "Active"          "seatno"          "countycode"      "District"       
# # [11] "party"           "Expr1"           "Address"         "address2"        "city"           
# # [16] "Zipcode"         "Expr2"           "EMailAddress"    "GenderCode"      "SecretaryID"    
# # [21] "database"
# dbListFields(nhgc, 'Legislation')
# #  [1] "legislationnbr"              "documenttypecode"            "sessionyear"                
# #  [4] "lsr"                         "LSRTitle"                    "DateLSREntered"             
# #  [7] "LegislativeBody"             "BillType"                    "AppropriationCode"          
# # [10] "FiscalImpactCode"            "LocalCode"                   "FullLSR"                    
# # [13] "SubjectCode"                 "ExpandedBillNo"              "CondensedBillNo"            
# # [16] "DateLSRBill"                 "ChapterNo"                   "SessionType"                
# # [19] "HouseCommitteeReferralCode"  "HouseCurrentCommitteeCode"   "HouseDateIntroduced"        
# # [22] "HouseStatusCode"             "HouseStatusDate"             "houseduedate"               
# # [25] "housefloordate"              "houseamended"                "SenateCommitteeReferralCode"
# # [28] "SenateCurrentCommitteeCode"  "SenateDateIntroduced"        "SenateStatusCode"           
# # [31] "SenateStatusDate"            "SenateDueDate "              "SenateFloorDate"            
# # [34] "SenateAmended"               "GeneralStatusCode"           "GeneralStatusDate"          
# # [37] "EffectiveDate"               "AdditionalEffectiveDates"    "Rereferred"                 
# # [40] "username"                    "DateModified"                "CurrentLSRStatus"           
# # [43] "LatestCommitteeHearingCode"  "LatestCommitteeHearingDate"  "LatestCommitteeHearingPlace"
# # [46] "Retained"                    "Database"                    "legislationID"
# dbListFields(nhgc, 'RollCallSummary')
# #  [1] "SessionYear"        "LegislativeBody"    "VoteSequenceNumber" "VoteDate"          
# #  [5] "CondensedBillNo"    "Yeas"               "Nays"               "Present"           
# #  [9] "Absent"             "AbbreviatedTitle1"  "AbbreviatedTitle2"  "Question_Motion"   
# # [13] "Title1"             "Title2"             "UserName"           "DateModified"      
# # [17] "Verified"           "CalendarItemID"
# dbListFields(nhgc, 'RollCallHistory')
# # [1] "EmployeeNumber"     "SessionYear"        "LegislativeBody"    "VoteSequenceNumber"
# # [5] "CondensedBillNo"    "Vote"               "UserName"           "DateModified"      
# # [9] "CalendarItemID"
# dbGetQuery(nhgc, 'select min(VoteDate), max(VoteDate) from RollCallSummary')
# # 1 1999-01-28 10:29:00 2020-03-13 03:14:34
# dbGetQuery(nhgc, 'select top 5 AbbreviatedTitle1, AbbreviatedTitle2, Question_Motion, Title1, Title2 from RollCallSummary')
# # looks like there's really only title1 and question_motion
# dbGetQuery(nhgc, 'select distinct LegislativeBody from RollCallSummary')
# # H and S, easy!

# # Vote codes aren't documented. Here's what I found from looking them up on the website:
# # 1: Yea
# # 2: Nay
# # 3: Excused
# # 4: Not Excused
# # 5: Conflict of Interest
# # 6: Presiding
# # 7: <blank> [is this 'present'?]

# # Sheila Roberge appears to be missing, which is odd
# legq =
#   "select Employeeno,
#           FirstName,
#           LastName,
#           party,
#           District,
#           GenderCode
#      from Legislators
#     where Employeeno in (select distinct EmployeeNumber
#                            from RollCallHistory
#                           where LegislativeBody='S')"
# legdf = dbGetQuery(nhgc, legq)
# rcq =
#   "select SessionYear,
#           VoteSequenceNumber,
#           VoteDate,
#           CondensedBillNo,
#           Title1,
#           Question_Motion
#      from RollCallSummary
#     where LegislativeBody='S'"
# rcdf = dbGetQuery(nhgc, rcq)
# voteq =
#   "select EmployeeNumber,
#           SessionYear,
#           VoteSequenceNumber,
#           Vote
#      from RollCallHistory
#     where LegislativeBody='S'"
# votedf = dbGetQuery(nhgc, voteq)
