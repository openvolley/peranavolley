#' Read Perana Sports volleyball data file
#'
#' @references \url{http://peranasports.com/}
#' @param filename string: path to file
#' @param insert_technical_timeouts logical: if \code{TRUE}, insert technical timeouts at 8 and 16 points
#' @param do_warn logical: should we issue warnings about the contents of the file as we read it?
#' @param raw_only logical: if \code{TRUE}, just decompress the file, don't parse it
#' @param eventgrades tibble: a tibble that defines the interpretations of \code{eventgrade} values; see \code{\link{pv_default_eventgrades}}
#' @param extra_validation numeric: should we run some extra validation checks on the file? 0=no extra validation, 1=check only for major errors, 2=somewhat more extensive, 3=the most extra checking
#' @param errortypes tibble: a tibble that defines the interpretations of \code{errortype} values; see \code{\link{pv_default_errortypes}}
#' @param subevents tibble: a tibble that defines the interpretations of \code{subevent} values; see \code{\link{pv_default_subevents}}
#' @param setting_zones named character: if the data file has been scouted using setting zones, then each attack will have its associated setting zone (numbered 1 to 5). The setting zone names are not stored in the file, so they can be provided here as a character vector. This can either be an un-named character vector, in which case it must be of length 5; otherwise if only a subset of the five setting zones are being used then it can be provided as a named character vector(e.g. \code{setting_zones = c("1" = "X1", "3" = "X7", "4" = "medium/fast", "5" = "high")}). The values in this vector will be used to populate the \code{attack_code} column of the returned plays data
#'
#' @return A named list with several elements. \code{raw} contains the extracted but unparsed text from the psvb file, \code{meta} provides match metadata, \code{plays} the play-by-play data in the form of a data.frame, and \code{messages} is a data.frame describing any inconsistencies found in the file.
#'
#' @seealso \code{\link{pv_validate}}, \code{\link{pv_default_eventgrades}}, \code{\link{pv_default_errortypes}}
#'
#' @examples
#' filename <- pv_example_file()
#' x <- pv_read(filename)
#'
#' x <- pv_read(filename, setting_zones = c("X1", "X2", "X7", "medium/fast", "high"))
#'
#' @export
pv_read <- function(filename, insert_technical_timeouts = FALSE, do_warn = FALSE, extra_validation = 2, raw_only = FALSE, eventgrades = pv_default_eventgrades(), errortypes = pv_default_errortypes(), subevents = pv_default_subevents(), setting_zones) {
    assert_that(is.string(filename))
    assert_that(is.flag(do_warn), !is.na(do_warn))
    assert_that(inherits(eventgrades, "data.frame"))
    assert_that(is.numeric(extra_validation), extra_validation %in% 0:3)
    assert_that(is.flag(raw_only), !is.na(raw_only))
    if (!missing(setting_zones)) {
        assert_that(is.character(setting_zones))
        if (length(setting_zones) == 5 && is.null(names(setting_zones))) names(setting_zones) <- as.character(1:5)
        assert_that(length(setting_zones) > 0, length(setting_zones) <= 5)
        assert_that(!any(is.na(setting_zones)))
        if (is.null(names(setting_zones)) || !all(names(setting_zones) %in% as.character(1:5))) stop("the names of the setting_zones parameter must in the range \"1\" to \"5\"")
    }
    x <- readLines(filename, warn = FALSE)
    x <- base64enc::base64decode(x)
    if (length(x) < 6 || !identical(x[5:6], as.raw(c(31, 8*16+11))))
        stop("cannot read file")
    ## first four bytes are the buffer size
    ##fsize <- readBin(x[1:4], "int")
    ## x <- memDecompress(from = x[5:length(x)], asChar = TRUE) ## nope
    tf <- tempfile()
    on.exit(unlink(tf))
    writeBin(x[5:length(x)], con = tf)
    gzcon <- gzfile(tf)
    x <- readLines(gzcon)
    close(gzcon)
    names(x) <- vapply(x, function(z) sub("~.*", "", z), FUN.VALUE = "", USE.NAMES = FALSE)
    if (raw_only) {
        list(raw = unname(x))
    } else {
        out <- pv_parse(x, eventgrades = eventgrades, errortypes = errortypes, subevents = subevents, setting_zones = setting_zones, do_warn = do_warn)
        out$meta$filename <- filename
        out$raw <- unname(x)
        if (!is.null(out$messages)) {
            if (nrow(out$messages) > 0) {
                out$messages <- out$messages[out$messages$severity <= extra_validation, ]
            }
            out$messages <- out$messages[, setdiff(names(out$messages), "severity")]
        }
        if (extra_validation > 0) {
            moreval <- pv_validate(out, validation_level = extra_validation)
            if (!is.null(moreval) && nrow(moreval) > 0) {
                out$messages <- bind_rows(out$messages, moreval)
            }
        }
        if (is.null(out$messages)) out$messages <- data.frame(file_line_number = integer(), video_time = numeric(), message = character(), file_line = character(), stringsAsFactors = FALSE)
        if (nrow(out$messages)>0) {
            out$messages <- distinct(out$messages)
            out$messages$file_line_number <- as.integer(out$messages$file_line_number)
            out$messages <- out$messages[order(out$messages$file_line_number, na.last = FALSE),]
            row.names(out$messages) <- NULL
        }
        class(out) <- c("peranavolley", class(out))
        out
    }
}

#filename <- "~/untan.gl/volleyball/data/perana/20170916_2017 AVL MEN'S_COEM 17_vs_QPM 17.psvb"
#x <- pv_read(filename)
#xx <- datavolley::read_dv("~/untan.gl/volleyball/data/italy_2015-16/ANDATA/10_GIORNATA/&2015-12-08 10a civ-mon 3-0 r.dvw")

## temp <- unname(sapply(x, function(z) sub("~.*", "", z)))
## table(temp)
## 
## DTAP DTHP    E    M  MAP  MHP   PA   PH PSVB    Q   SS   TA  TAP   TH  THP   TO    V   VE 
##   40   48  639    1   10   12   18   19    1    4   88    1   18    1   19    1    1    1

pv_parse <- function(x, eventgrades, errortypes, subevents, setting_zones, do_warn) {
    debug <- TRUE
    as_dv <- TRUE
    msgs <- list()
    pparse <- function(z, df = TRUE) {
        temp <- sub("^[A-Z]+~", "", z)
        if (grepl("^\\(?null\\)?", temp, ignore.case = TRUE)) {
            if (df) tibble() else NULL
        } else {
            jsonlite::fromJSON(temp)
        }
    }
    pparse_df <- function(z) fixnames(bind_rows(lapply(z, pparse)))
    parse_players <- function(z) {
        ## sometimes 'nickname' is duplicated?
        ## since we don't use it anyway, drop it
        z <- z[, setdiff(names(z), c("nickname"))]
        out <- dplyr::rename(z, player_id = "guid")
        ## don't assign roles here: they aren't strict in psvb files, and tend to be used to describe *all* of the positions a given player plays
        ## setter and libero we get from the lineup
        out <- mutate(out, name = paste(.data$firstname, .data$lastname),
                      special_role = "",##case_when(grepl("libero", .data$positionsstring, ignore.case = TRUE) ~ "L", TRUE ~ ""),
                      role = NA_character_)##case_when(special_role %eq% "L" ~ "libero"))
        dplyr::select(out, -"thumbnaildata", -"positionsstring")
    }

    known_event_types <- c("Block", "Defense", "Pass", "Serve", "Set", "Spike", "Substitution", "Timeout", "")

    file_meta <- tibble(fileformat = "PSVB", file_type = "perana_indoor")
    meta <- list()
    temp_mm <- pparse_df(x[names(x) == "M"]) ## match metadata
    temp_to <- pparse_df(x[names(x) == "TO"]) ## tournament metadata
    temp_mm$date <- as.Date(NA)
    temp_mm$time <- as.duration(NA)
    try({
        temp_mm$trainingdate <- ymd_hms(temp_mm$trainingdate)
        temp_mm$date <- as.Date(temp_mm$trainingdate)
        temp_mm$time <- hms(format(temp_mm$trainingdate, "%H:%M:%S"))
    }, silent = TRUE)
    meta$match_id <- temp_mm$guid
    meta$match <- mutate(temp_mm[, c("date", "time")], season = NA_character_, league = temp_to$name, text_encoding = NA_character_, zones_or_cones = "Z")
    temp_ve <- pparse_df(x[names(x) == "VE"]) ## venue
    if (nrow(temp_ve) < 1) temp_ve <- tibble(name = NA_character_)
    meta$more <- data.frame(referees = NA_character_, city = tryCatch(temp_ve$name, error = function(e) NA_character_), arena = NA_character_, scout = NA_character_)

    temp_th <- pparse_df(x[names(x) == "TH"])
    temp_ta <- pparse_df(x[names(x) == "TA"])
    meta$teams <- tibble(team_id = as.character(c(temp_th$guid, temp_ta$guid)),
                         team = c(temp_th$name, temp_ta$name),
                         sets_won = c(temp_mm$homescore, temp_mm$awayscore),
                         coach = NA_character_,
                         assistant = NA_character_,
                         home_away_team = c("*", "a"))
    meta$teams$won_match <- c(meta$teams$sets_won[1] > meta$teams$sets_won[2], meta$teams$sets_won[2] > meta$teams$sets_won[1])

    ## PH = home player, PA = away player
    meta$players_h <- parse_players(pparse_df(x[names(x) %eq% "PH"]))
    meta$players_v <- parse_players(pparse_df(x[names(x) %eq% "PA"]))
    if (length(intersect(meta$players_h$player_id, meta$players_v$player_id)))
        stop("non-unique player ids")

    ## team player numbers (PlayerTeamLink)
    ## homeaway~player_number~guid
    parse_plnum <- function(z) {
        temp <- str_match(z, "T.P~([[:digit:]]+)~(.*)")
        tibble(number = as.integer(temp[, 2]), player_id = temp[, 3])
    }
    temp <- distinct(parse_plnum(x[names(x) %eq% "THP"]))
    if (any(duplicated(temp$player_id))) {
        stop("Home team has duplicate player_id in THP sections")
    }
    if (any(duplicated(temp$number))) {
        this_msg <- paste0("Home team players have duplicate numbers defined in THP sections")
        if (do_warn) warning(this_msg)
        msgs <<- collect_messages(msgs, this_msg, NA_integer_, NA_character_, severity = 2) ## TODO add line num and line text, and player ID/nums
    }
    if (!all(temp$player_id %in% meta$players_h$player_id)) {
        for (pid in setdiff(temp$player_id, meta$players_h$player_id)) {
            this_msg <- paste0("Home team player appears in THP section but not PH section (player id ", pid, ")")
            if (do_warn) warning(this_msg)
            msgs <<- collect_messages(msgs, this_msg, NA_integer_, NA_character_, severity = 2) ## TODO add line num and line text
        }
    }
    chk <- nrow(meta$players_h)
    meta$players_h <- left_join(meta$players_h, temp, by = "player_id")
    if (nrow(meta$players_h) != chk) stop("error with home player lineup")
    ## THP~9~16ED63EA-D11B-494D-821B-E6C53A6035D2-3664-00025E7380ACFEC2
    temp <- distinct(parse_plnum(x[names(x) %eq% "TAP"]))
    if (any(duplicated(temp$player_id))) {
        stop("Visiting team has duplicate player_id in TAP sections")
    }
    if (any(duplicated(temp$number))) {
        this_msg <- paste0("Visiting team players have duplicate numbers defined in TAP sections")
        if (do_warn) warning(this_msg)
        msgs <<- collect_messages(msgs, this_msg, NA_integer_, NA_character_, severity = 2) ## TODO add line num and line text, and player ID/nums
    }
    if (!all(temp$player_id %in% meta$players_v$player_id)) {
        ##stop("mismatch in visiting players list")
        for (pid in setdiff(temp$player_id, meta$players_v$player_id)) {
            this_msg <- paste0("Visiting team player appears in TAP section but not PA section (player id ", pid, ")")
            if (do_warn) warning(this_msg)
            msgs <<- collect_messages(msgs, this_msg, NA_integer_, NA_character_, severity = 2) ## TODO add line num and line text
        }
    }
    chk <- nrow(meta$players_v)
    meta$players_v <- left_join(meta$players_v, temp, by = "player_id")
    if (nrow(meta$players_v) != chk) stop("error with visiting player lineup")

    ## "match players"
    ## MHP, MAP ??

    ## drill players
    ## DTHP, DTAP ??

    ## Q has set info for each set
    set_meta <- pparse_df(x[names(x) %eq% "Q"])
    if (nrow(set_meta) < 1) stop("file contains no data?")
    meta$result <-  dplyr::select(set_meta, score_home_team = "homescore", score_visiting_team = "awayscore")
    meta$result$duration <- if ("drillduration" %in% names(set_meta)) as.integer(set_meta$drillduration) else NA_integer_
    meta$result$duration[meta$result$duration == 0L] <- NA_integer_
    meta$result$score <- paste0(meta$result$score_home_team, "-", meta$result$score_visiting_team)

    ## DV files have attack codes X5, XP, etc. Perana has 5 set zones which perform an analogous function
    ## note that using these is optional
    meta$attacks <- tibble(code = as.integer(1:5), attacker_position = NA_character_, side = NA_character_, type = NA_character_, description = NA_character_, set_type = NA_character_)

    ## E~{"PlayerGuid":"60828D75-8CBE-47B7-B888-86E390079B5B-88820-000188325C496FD8","videoDuration":0,"EventString":"Serve","BallEndString":"0, 0|0","EventGrade":0,"TimeStamp":"2017-09-16T23:03:32.000Z","ErrorType":0,"OppositionScore":0,"SubEvent2":0,"TeamScore":0,"SubEvent":0,"EventId":"1370","EventType":1,"Row":6,"BallMidString":"0, 0|0","BallStartString":"0, 0|0"}

    ## SS = session stats

    ## E = events
    ## each set starts with Q entry
    qidx <- c(which(names(x) %eq% "Q"), length(x) + 1)
    evidx <- which(names(x) %eq% "E") ## all event entries
    plays <- tibble()
    this_home_team <- meta$teams$team[1]
    this_visiting_team <- meta$teams$team[2]
    this_home_team_id <- meta$teams$team_id[1]
    this_visiting_team_id <- meta$teams$team_id[2]
    not_action_skills <- c("Timeout", "Substitution", "Technical timeout")

    ## helper func to update player role in metadata
    update_metadata_player_role <- function(player_id, role, team, set_number) {
        team <- match.arg(team, c("home", "visiting"))
        this_pidx <- if (team == "home") which(meta$players_h$player_id %eq% player_id) else which(meta$players_v$player_id %eq% player_id)
        if (length(this_pidx) < 1) {
            this_msg <- paste0("Set ", set_number, ": ", team, " ", role, " player ", player_id, " in starting lineup does not appear in team list")
            if (do_warn) warning(this_msg)
            msgs <<- collect_messages(msgs, this_msg, qidx[set_number], x[qidx[set_number]], severity = 1)
            this_pidx <- NULL
        } else if (length(this_pidx) > 1) {
            this_msg <- paste0("Set ", set_number, ": ", team, " ", role, " player ", player_id, " in starting lineup matches multiple players in team list")
            if (do_warn) warning(this_msg)
            msgs <<- collect_messages(msgs, this_msg, qidx[set_number], x[qidx[set_number]], severity = 1)
            this_pidx <- NULL
        } else {
            ## don't warn on these: the roles in the psvb metadata are not strict, and tend to be used to describe *all* of the positions a given player plays
            ##existing_role <- if (team == "home") meta$players_h$role[this_pidx] else meta$players_v$role[this_pidx]
            ##if (!is.na(existing_role) && nzchar(existing_role) && !existing_role %eq% role) {
            ##    this_msg <- paste0("Set ", set_number, ": ", team, " ", role, " player ", player_id, " in starting lineup is listed as role ", existing_role, " in team list")
            ##    if (do_warn) warning(this_msg)
            ##    msgs <<- collect_messages(msgs, this_msg, qidx[set_number], x[qidx[set_number]], severity = 1)
            ##}
            if (team == "home") {
                meta$players_h$role[this_pidx] <<- role
                if (role %eq% "libero" && !grepl("L", meta$players_h$special_role[this_pidx])) meta$players_h$special_role[this_pidx] <<- paste0(meta$players_h$special_role[this_pidx], "L")
            } else {
                meta$players_v$role[this_pidx] <<- role
                if (role %eq% "libero" && !grepl("L", meta$players_v$special_role[this_pidx])) meta$players_v$special_role[this_pidx] <<- paste0(meta$players_v$special_role[this_pidx], "L")
            }
        }
        this_pidx
    }

    ## player and team info to join to plays
    all_players <- bind_rows(mutate(meta$players_h, team = this_home_team, team_id = this_home_team_id),
                             mutate(meta$players_v, team = this_visiting_team, team_id = this_visiting_team_id))
    all_players <- dplyr::select(all_players, playerguid = "player_id", "team", "team_id", player_number = "number", player_name = "name")
    this_ptid <- 0
    for (si in seq_len(length(qidx) - 1)) {
        xidx <- seq(qidx[si]+1, qidx[si+1]-1, by = 1)
        ## events associated with this set
        this_plays <- pparse_df(x[intersect(xidx, evidx)])
        this_plays$file_line_number <- intersect(xidx, evidx)
        this_plays$timestamp <- ymd_hms(this_plays$timestamp)
        if (any(!nzchar(this_plays$eventstring))) {
            stop("empty event string")
        }
        if (!all(this_plays$eventstring %in% known_event_types)) {
            stop("unexpected eventstrings: ", paste(setdiff(this_plays$eventstring, known_event_types)))
        }
        this_plays$end_of_set <- FALSE
        this_plays <- bind_rows(this_plays, tibble(end_of_set = TRUE))
        ## add player and team info into plays
        chk <- nrow(this_plays)
        this_plays <- left_join(this_plays, all_players, by = "playerguid")
        if (nrow(this_plays) != chk) stop("error merging players into events")

        chk <- !this_plays$team_id %in% c(this_home_team_id, this_visiting_team_id) & !this_plays$eventstring %in% c("Timeout") & !this_plays$end_of_set
        if (any(chk)) {
            ##cat(str(this_plays[chk, ]))
            stop("unmatched player/team")
        }
        chk <- nrow(this_plays)
        this_plays <- left_join(this_plays, eventgrades, by = c(eventstring = "skill", "eventgrade"))
        if (nrow(this_plays) != chk) stop("error merging eventgrades into events")
        this_plays$special_code <- NA_character_
        ## errortype mostly (only?) applies to attacks
        this_plays$errortype <- as.character(this_plays$errortype)
        aidx <- this_plays$eventstring %eq% "Spike"
        temp <- errortypes[errortypes$skill %eq% "Spike", ]
        this_plays$errortype[aidx] <- dmapvalues(this_plays$errortype[aidx], temp$errortype, temp$evaluation)
        if (!all(errortypes$skill %eq% "Spike")) warning("errortypes associated with events other than 'Spike' have not been processed")

        ## subevent goes into skill_subtype for attacks, set zone goes to attack_code
        this_plays$skill_subtype <- NA_character_
        this_plays$attack_code <- NA_character_
        temp <- subevents[subevents$skill %eq% "Spike", ]
        aidx <- this_plays$eventstring %eq% "Spike"
        ## some subevents are of the form X11Y where X is the subevent 0-4 and Y is the setting zone 1-5
        this_ss <- as.numeric(this_plays$subevent[aidx])
        ## setting zone goes to attack_code
        this_plays$attack_code[aidx] <- case_when(this_ss %in% c(111:115, 1111:1115, 2111:2115, 3111:3115, 4111:4115) ~ as.character(this_ss - floor(this_ss/10)*10))
        if (!missing(setting_zones)) {
            this_plays$attack_code[aidx] <- dmapvalues(as.character(this_plays$attack_code[aidx]), from = names(setting_zones), to = setting_zones)
        }
        this_ss <- case_when(this_ss %in% 0:4 ~ this_ss,
                             this_ss %in% c(111:115, 1111:1115, 2111:2115, 3111:3115, 4111:4115) ~ floor(this_ss/1000),
                             TRUE ~ this_ss)
        this_plays$skill_subtype[aidx] <- dmapvalues(as.character(this_ss), as.character(temp$subevent), temp$evaluation)
        if (!all(errortypes$skill %eq% "Spike")) warning("subtypes associated with events other than 'Spike' have not been processed")

        if (as_dv) {
            ## don't treat blocked attack as an error
            this_plays <- mutate(this_plays, evaluation = case_when(errortype %eq% "Blocked" & eventstring %eq% "Spike" & evaluation %eq% "Error" ~ "Blocked", TRUE ~ evaluation))
            ## attack errortype is recorded in special_code for dv
            this_plays <- mutate(this_plays, special_code = case_when(eventstring %eq% "Spike" ~ errortype, TRUE ~ special_code))
        }
        ## add set-specific info
        this_plays$set_number <- as.integer(set_meta$gamenumber[si])
        ## players in positions
        this_plays[, paste0("home_player_id", 1:6)] <- NA_character_
        this_plays[, paste0("visiting_player_id", 1:6)] <- NA_character_
        h_lineup <- strsplit(set_meta$startinglineup[si], ",")[[1]]
        ## entries 1-6 are pos 1-6, entry 7 = libero (??), entry 8 = ??
        ##temp <- tibble(player_id = h_lineup) %>% left_join(meta$players_h)
        ##cat(str(temp), "\n")
        if (length(h_lineup) < 6 || length(h_lineup) > 8) stop("unexpected home team lineup")
        last_hl <- h_lineup[1:6]
        this_plays[1, paste0("home_player_id", 1:6)] <- last_hl
        home_setter_id <- set_meta$primarysetterguid[si]
        pidx <- update_metadata_player_role(home_setter_id, "setter", team = "home", set_number = si)
        # needs thought, what if the team is playing 6-2 or 4-2
        #if (!is.null(pidx)) {
        #    ## assign opposite, as well
        #    update_metadata_player_role(h_lineup[((pidx+3) -1) %% 6 + 1], "opposite", team = "home", set_number = si)
        #}
        if (!home_setter_id %in% last_hl) {
            this_msg <- paste0("Set ", si, ": the player listed as the home team primary setter is not in the starting lineup")
            if (do_warn) warning(this_msg)
            msgs <- collect_messages(msgs, this_msg, qidx[si], x[qidx[si]], severity = 1)
        }
        if (length(h_lineup) > 6) {
            ##cat("home liberos: ", h_lineup[-6:-1], "\n")
            for (pid in setdiff(h_lineup[-6:-1], 0)) { ## ignore pid 0, means that no second lib was assigned (or possibly first lib as well??)
                update_metadata_player_role(pid, "libero", team = "home", set_number = si)
                ##this_pidx <- which(meta$players_h$player_id %eq% pid)
                ##if (length(this_pidx) < 1) {
                ##    this_msg <- paste0("Set ", si, ": home libero player ", pid, " in starting lineup does not appear in team list")
                ##    if (do_warn) warning(this_msg)
                ##    msgs <- collect_messages(msgs, this_msg, qidx[si], x[qidx[si]], severity = 1)
                ##} else if (length(this_pidx) > 1) {
                ##    this_msg <- paste0("Set ", si, ": home libero player ", pid, " in starting lineup matches multiple players in team list")
                ##    if (do_warn) warning(this_msg)
                ##    msgs <- collect_messages(msgs, this_msg, qidx[si], x[qidx[si]], severity = 1)
                ##} else {
                ##    meta$players_h$special_role[this_pidx] <- "L"
                ##    existing_role <- meta$players_h$role[this_pidx]
                ##    if (!is.na(existing_role) && nzchar(existing_role) && !existing_role %eq% "libero") {
                ##        this_msg <- paste0("Set ", si, ": home libero player ", pid, " in starting lineup is listed as role ", existing_role, " in team list")
                ##        if (do_warn) warning(this_msg)
                ##        msgs <- collect_messages(msgs, this_msg, qidx[si], x[qidx[si]], severity = 1)
                ##    }
                ##    meta$players_h$role[this_pidx] <- "libero"
                ##}
            }
        }
        v_lineup <- strsplit(set_meta$oppositionstartinglineup[si], ",")[[1]]
        if (length(v_lineup) < 6 || length(v_lineup) > 8) stop("unexpected visiting team lineup")
        last_vl <- v_lineup[1:6]
        this_plays[1, paste0("visiting_player_id", 1:6)] <- last_vl
        visiting_setter_id <- set_meta$oppprimarysetterguid[si]
        update_metadata_player_role(visiting_setter_id, "setter", team = "visiting", set_number = si)
        if (!visiting_setter_id %in% last_vl) {
            this_msg <- paste0("Set ", si, ": the player listed as the visiting team primary setter is not in the starting lineup")
            if (do_warn) warning(this_msg)
            msgs <- collect_messages(msgs, this_msg, qidx[si], x[qidx[si]], severity = 1)
        }
        if (length(v_lineup) > 6) {
            ##cat("visiting liberos: ", v_lineup[-6:-1], "\n")
            for (pid in setdiff(v_lineup[-6:-1], 0)) update_metadata_player_role(pid, "libero", team = "visiting", set_number = si)
        }
        this_plays$home_setter_position <- NA_integer_
        this_plays$visiting_setter_position <- NA_integer_
        this_home_setter_pos <- if (home_setter_id %in% last_hl) which(last_hl == home_setter_id) else NA_integer_
        if (length(this_home_setter_pos) != 1) this_home_setter_pos <- NA_integer_
        this_plays$home_setter_position[1] <- this_home_setter_pos
        this_visiting_setter_pos <- if (visiting_setter_id %in% last_vl) which(last_vl == visiting_setter_id) else NA_integer_
        if (length(this_visiting_setter_pos) != 1) this_visiting_setter_pos <- NA_integer_
        this_plays$visiting_setter_position[1] <- this_visiting_setter_pos
        this_plays$point_id <- NA_integer_
        this_plays$serving_team <- NA_character_
        last_hts <- 0; last_vts <- 0 ## prev team scores
        if (!this_plays$eventstring[1] %eq% "Serve") stop("Set ", si, " did not start with serve")
        last_stid <- this_plays$team_id[1] ## prev serving team
        this_plays$serving_team[1] <- this_plays$team[1]
        this_plays[, c("timeout", "substitution")] <- FALSE
        this_plays[, c("point_won_by", "code", "skill_type", "end_subzone", "attack_description", "set_code", "set_description", "set_type", "num_players")] <- NA_character_
        this_plays[, c("start_zone", "end_zone", "num_players_numeric", "home_team_score", "visiting_team_score")] <- NA_integer_
        ## note that home_team_score and visiting_team_score are (per DataVolley conventions) the score at the end of the point, not the score at the start of the point (as with Perana)
        this_ptid <- this_ptid + 1
        this_plays$point_id[1] <- this_ptid
        my_last_hl <- last_hl; my_last_vl <- last_vl
        for (ei in seq_len(nrow(this_plays))[-1]) {
            if (this_plays$eventstring[ei] %eq% "Substitution") {
                ## outgoing player is in playerguid
                ## incoming player is in userdefined01
#                if (debug) cat("Sub ")
                out_pl <- this_plays$playerguid[ei]
                in_pl <- this_plays$userdefined01[ei]
                if (this_plays$team_id[ei] %eq% this_home_team_id) {
#                    cat("(home): ", this_plays$playerguid[ei], " out for ", this_plays$userdefined01[ei], "\n")
#                    cat("pre: ", last_hl, "\n")
                    last_hl[last_hl %eq% out_pl] <- in_pl
                    my_last_hl[my_last_hl %eq% out_pl] <- in_pl
#                    cat("post: ", last_hl, "\n")
                    if (out_pl %eq% home_setter_id) home_setter_id <- in_pl
                } else if (this_plays$team_id[ei] %eq% this_visiting_team_id) {
#                    cat("(visiting): ", this_plays$playerguid[ei], " out for ", this_plays$userdefined01[ei], "\n")
#                    cat("pre: ", last_vl, "\n")
                    last_vl[last_vl %eq% out_pl] <- in_pl
                    my_last_vl[my_last_vl %eq% out_pl] <- in_pl
#                    cat("post: ", last_vl, "\n")
                    if (out_pl %eq% visiting_setter_id) visiting_setter_id <- in_pl
                }
                this_ptid <- this_ptid + 1
                this_plays$substitution[ei] <- TRUE
            } else if (this_plays$eventstring[ei] %eq% "Timeout") {
                ## subevent on timeouts is number already called
                ## 0 = first timeout for that team
                ## 1 = second
                if (this_plays$userdefined01[ei] %eq% this_home_team_id) {
                    this_plays$team[ei] <- this_home_team
                    this_plays$team_id[ei] <- this_home_team_id
                } else {
                    this_plays$team[ei] <- this_visiting_team
                    this_plays$team_id[ei] <- this_visiting_team_id
                }
                this_ptid <- this_ptid + 1
                this_plays$timeout[ei] <- TRUE
            } else if (this_plays$end_of_set[ei]) {
                this_plays$code[ei] <- paste0("**", si, "set")
                this_plays$timestamp[ei] <- this_plays$timestamp[ei-1]
                this_ptid <- this_ptid + 1
            } else {
                if (this_plays$eventstring[ei] %eq% "Serve") {
                    ## new point
                    this_ptid <- this_ptid + 1
                    this_plays$serving_team[ei] <- this_plays$team[ei]
                    ## teamscore, oppositionscore are the scores at the start of this (new) point; assign these to (home|visiting)_team_score
                    this_plays$home_team_score[ei-1] <- this_plays$teamscore[ei]
                    this_plays$visiting_team_score[ei-1] <- this_plays$oppositionscore[ei]
                    if (this_plays$teamscore[ei] > last_hts) {
                        ## home team won last point
                        ## make sure this gets assigned to last actual point, not last timeout or sub
                        for (slow_backwards in rev(seq_len(ei-1))) {
                            if (!this_plays$eventstring[slow_backwards] %in% not_action_skills) {
                                this_plays$point_won_by[slow_backwards] <- this_home_team
                                break
                            }
                        }
                        if (!last_stid %eq% this_plays$team_id[ei]) {
                            ## rotate
                            my_last_hl <- rot_one(my_last_hl)
                        }
                    } else {
                        ## visiting team won last point
                        ##this_plays$point_won_by[ei-1] <- this_visiting_team
                        for (slow_backwards in rev(seq_len(ei-1))) {
                            if (!this_plays$eventstring[slow_backwards] %in% not_action_skills) {
                                this_plays$point_won_by[slow_backwards] <- this_visiting_team
                                break
                            }
                        }
                        if (!last_stid %eq% this_plays$team_id[ei]) {
                            ## rotate
                            my_last_vl <- rot_one(my_last_vl)
                        }
                    }
                    if (this_plays$team_id[ei] %eq% this_home_team_id) {
                        ## home team serving
                        ## align last_hl to serving player
                        last_hl <- tryCatch(rot_p1(last_hl, this_plays$playerguid[ei]),
                                            error = function(e) {
                                                fln <- this_plays$file_line_number[ei]
                                                this_msg  <- paste0("Home team serving player ", this_plays$playerguid[ei], "on line ", fln, " is not in team lineup on line ", qidx[si])
                                                if (do_warn) warning(this_msg)
                                                msgs <<- collect_messages(msgs, this_msg, fln, x[fln], severity = 1)
                                                my_last_hl
                                            })
                    } else {
                        last_vl <- tryCatch(rot_p1(last_vl, this_plays$playerguid[ei]),
                                            error = function(e) {
                                                fln <- this_plays$file_line_number[ei]
                                                this_msg  <- paste0("Visiting team serving player ", this_plays$playerguid[ei], "on line ", fln, " is not in team lineup on line ", qidx[si])
                                                if (do_warn) warning(this_msg)
                                                msgs <<- collect_messages(msgs, this_msg, fln, x[fln], severity = 1)
                                                my_last_vl
                                            })
                    }
                    ## TODO check that the serving player is the one we expect!
                    ## currently these are popping up because of score mis-assignment, so comment out for now
                    #if (!identical(last_vl, my_last_vl)) {
                    #    warning("visiting team serving player is not who we expect at point_id ", this_ptid)
                    #}
                    #if (!identical(last_hl, my_last_hl)) {
                    #    warning("home team serving player is not who we expect at point_id ", this_ptid)
                    #}
                    last_stid <- this_plays$team_id[ei]
                } else {
                    ## event is part of same point
                }
                last_hts <- this_plays$teamscore[ei]
                last_vts <- this_plays$oppositionscore[ei]
            }
            this_plays$point_id[ei] <- this_ptid
            this_plays[ei, c(paste0("home_player_id", 1:6), paste0("visiting_player_id", 1:6))] <- c(last_hl, last_vl)
            this_home_setter_pos <- if (home_setter_id %in% last_hl) which(last_hl == home_setter_id) else NA_integer_
            if (length(this_home_setter_pos) != 1) this_home_setter_pos <- NA_integer_
            this_plays$home_setter_position[ei] <- this_home_setter_pos
            this_visiting_setter_pos <- if (visiting_setter_id %in% last_vl) which(last_vl == visiting_setter_id) else NA_integer_
            if (length(this_visiting_setter_pos) != 1) this_visiting_setter_pos <- NA_integer_
            this_plays$visiting_setter_position[ei] <- this_visiting_setter_pos
        }
        ## point_won_by for last point, which will be second-last row because we inserted an end-of-set marker
        ## also home_team_score & visiting_team_score
        if (nrow(this_plays) > 1) {
            if (this_plays$teamscore[ei-1] > this_plays$oppositionscore[ei-1]) {
                this_plays$point_won_by[ei-1] <- this_home_team
                this_plays$home_team_score[ei-1] <- this_plays$teamscore[ei-1]+1 ## max(this_plays$home_team_score, na.rm = TRUE) + 1
            } else {
                this_plays$point_won_by[ei-1] <- this_visiting_team
                this_plays$visiting_team_score[ei-1] <- this_plays$oppositionscore[ei-1]+1
            }
        }
        ## fill in scores on end_of_set lines
        this_plays$home_team_score[ei] <- max(this_plays$home_team_score, na.rm = TRUE)
        this_plays$visiting_team_score[ei] <- max(this_plays$visiting_team_score, na.rm = TRUE)
        ## populate all rows with serving_team, point_won_by info
        temp <- distinct(this_plays[!is.na(this_plays$serving_team), c("point_id", "serving_team")])
        if (any(duplicated(temp$point_id))) {
            warning("multiple serving teams in at least one point!")
            temp <- temp[!duplicated(temp$point_id), ]
        }
        chk <- nrow(this_plays)
        this_plays <- left_join(dplyr::select(this_plays, -"serving_team"), temp, by = "point_id")
        if (nrow(this_plays) != chk) stop("error expanding serving_team entries")
        ## and for point_won_by
        temp <- distinct(this_plays[!is.na(this_plays$point_won_by), c("point_id", "point_won_by")])
        if (any(duplicated(temp$point_id))) {
            warning("multiple point_won_by in at least one point!")
            temp <- temp[!duplicated(temp$point_id), ]
        }
        chk <- nrow(this_plays)
        this_plays <- left_join(dplyr::select(this_plays, -"point_won_by"), temp, by = "point_id")
        if (nrow(this_plays) != chk) stop("error expanding point_won_by entries")

        ## home_team_score, visiting_team_score
        for (vv in c("home_team_score", "visiting_team_score")) {
            temp <- distinct(this_plays[!is.na(this_plays[[vv]]), c("point_id", vv)])
            if (any(duplicated(temp$point_id))) {
                warning("multiple ", vv, " in at least one point!")
                temp <- temp[!duplicated(temp$point_id), ]
            }
            chk <- nrow(this_plays)
            this_plays <- left_join(this_plays[, setdiff(names(this_plays), vv)], temp, by = "point_id")
            if (nrow(this_plays) != chk) stop("error expanding ", vv, " entries")
        }
        ## iterate backwards over scores to fill in NAs on timeouts, subs
        for (ii in rev(seq_len(nrow(this_plays)))[-1]) {
            if (is.na(this_plays$home_team_score[ii])) this_plays$home_team_score[ii] <- this_plays$home_team_score[ii+1]
            if (is.na(this_plays$visiting_team_score[ii])) this_plays$visiting_team_score[ii] <- this_plays$visiting_team_score[ii+1]
        }
        ## to check: set_meta number of timeouts per team matches events
        plays <- bind_rows(plays, this_plays)
    }
    ## some processing on all plays
    plays <- dplyr::rename(plays, skill = "eventstring", player_id = "playerguid", time = "timestamp", video_time = "videoduration")
    ##  "errortype" applies only to attacks?
    ## "subevent2" "subevent" "eventid" "row" "userdefined01"
    plays <- dplyr::select(plays, -"eventtype")
    plays <- mutate(plays, match_id = meta$match_id,
                    home_team = this_home_team, visiting_team = this_visiting_team,
                    home_team_id = meta$teams$team_id[1], visiting_team_id = meta$teams$team_id[2],
                    custom_code = NA_character_,
                    skill = case_when(skill %eq% "Defense" ~ "Dig",
                                      skill %eq% "Pass" ~ "Reception",
                                      skill %eq% "Spike" ~ "Attack",
                                      TRUE ~ skill),
                    winning_attack = case_when(skill %eq% "Attack" & win_loss > 0 ~ TRUE,
                                               TRUE ~ FALSE))

    ##plays %>% count(skill, eventgrade, evaluation)
    ##temp <- setNames(read.csv(text=gsub("|", ", ", plays$ballstartstring, fixed = TRUE), header = FALSE), c("x", "y", "z"))
    temp <- gsub("\\|0[[:space:]]*$", "", plays$ballstartstring)
    if (any(grepl("|", temp, fixed = TRUE))) {
        warning("unexpected format of ballstartstring, skipping")
    } else {
        temp <- setNames(read.csv(text = temp, header = FALSE), c("x", "y"))
        zidx <- abs(temp$x) < 0.001 & abs(temp$y) < 0.001
        temp$x[zidx] <- NA_real_
        temp$y[zidx] <- NA_real_
        plays$start_coordinate_x <- temp$x*0.03 + 0.5
        plays$start_coordinate_y <- (200-temp$y)*0.03 + 0.5
    }
    temp <- gsub("\\|0[[:space:]]*$", "", plays$ballmidstring)
    if (any(grepl("|", temp, fixed = TRUE))) {
        warning("unexpected format of ballmidstring, skipping")
    } else {
        temp <- setNames(read.csv(text = temp, header = FALSE), c("x", "y"))
        zidx <- abs(temp$x) < 0.001 & abs(temp$y) < 0.001
        temp$x[zidx] <- NA_real_
        temp$y[zidx] <- NA_real_
        plays$mid_coordinate_x <- temp$x*0.03 + 0.5
        plays$mid_coordinate_y <- (200-temp$y)*0.03 + 0.5
    }
    temp <- gsub("\\|0[[:space:]]*$", "", plays$ballendstring)
    if (any(grepl("|", temp, fixed = TRUE))) {
        warning("unexpected format of ballendstring, skipping")
        cat(plays$ballendstring, "\n", sep = "#")
    } else {
        temp <- setNames(read.csv(text = temp, header = FALSE), c("x", "y"))
        zidx <- abs(temp$x) < 0.001 & abs(temp$y) < 0.001
        temp$x[zidx] <- NA_real_
        temp$y[zidx] <- NA_real_
        plays$end_coordinate_x <- temp$x*0.03 + 0.5
        plays$end_coordinate_y <- (200-temp$y)*0.03 + 0.5
    }
    ## check that e.g. both serve and reception have start = serve loc and end = reception loc
    ## TODO check what happen when a reception has ball path entered - presumably this gives pass loc (start, should same as serve end loc) and set loc (end, should be same as set loc start if it was entered)
    idx <- plays$skill %eq% "Reception" & lag(plays$skill) %eq% "Serve" & !lag(plays$team) %eq% plays$team ##& is.na(plays$start_coordinate_x) & !is.na(lag(plays$start_coordinate_x))
    plays$start_coordinate_x[idx] <- lag(plays$start_coordinate_x)[idx]
    plays$start_coordinate_y[idx] <- lag(plays$start_coordinate_y)[idx]
    plays$mid_coordinate_x[idx] <- lag(plays$mid_coordinate_x)[idx]
    plays$mid_coordinate_y[idx] <- lag(plays$mid_coordinate_y)[idx]
    plays$end_coordinate_x[idx] <- lag(plays$end_coordinate_x)[idx]
    plays$end_coordinate_y[idx] <- lag(plays$end_coordinate_y)[idx]

    ## convert those back to single-index coordinates, too
    plays$start_coordinate <- dv_xy2index(plays$start_coordinate_x, plays$start_coordinate_y)
    plays$mid_coordinate <- dv_xy2index(plays$mid_coordinate_x, plays$mid_coordinate_y)
    plays$end_coordinate <- dv_xy2index(plays$end_coordinate_x, plays$end_coordinate_y)

    ## and to zones, which will all be NA at this point
    plays$start_zone <- as.integer(plays$start_zone)
    plays$end_zone <- as.integer(plays$end_zone)
    plays$end_subzone <- as.character(plays$end_subzone)
    idx <- !is.na(plays$start_coordinate_x) & !is.na(plays$start_coordinate_y) & plays$skill %in% c("Serve", "Reception")
    plays$start_zone[idx] <- xy2zone(plays$start_coordinate_x[idx], plays$start_coordinate_y[idx], as_for_serve = TRUE)
    idx <- !is.na(plays$start_coordinate_x) & !is.na(plays$start_coordinate_y) & !plays$skill %in% c("Serve", "Reception")
    plays$start_zone[idx] <- xy2zone(plays$start_coordinate_x[idx], plays$start_coordinate_y[idx], as_for_serve = FALSE)
    idx <- !is.na(plays$end_coordinate_x) & !is.na(plays$end_coordinate_y)
    plays$end_zone[idx] <- xy2zone(plays$end_coordinate_x[idx], plays$end_coordinate_y[idx], as_for_serve = FALSE)
    plays$end_subzone[idx] <- xy2subzone(plays$end_coordinate_x[idx], plays$end_coordinate_y[idx])
    

    ##ggplot(xp$plays, aes(start_coordinate_x, start_coordinate_y, colour = as.factor(start_zone))) + geom_point() + datavolley::ggcourt()
    ##ggplot(xp$plays, aes(end_coordinate_x, end_coordinate_y, colour = as.factor(end_zone))) + geom_point() + datavolley::ggcourt()

    ##if (FALSE) {
    ##    plays <- expand.grid(start_coordinate_x = seq(0.2, 3.8, by = 0.01), start_coordinate_y = seq(0.2, 6.7, by = 0.01))
    ##    plays$skill <- "x"
    ##    plays <- mutate(plays, start_zone = case_when(is.na(.data$start_coordinate_x) | is.na(.data$start_coordinate_y) ~ NA_integer_,
    ##                                              .data$skill %in% c("Serve", "Reception") ~ xy2zone(.data$start_coordinate_x, .data$start_coordinate_y, as_for_serve = TRUE),
    ##                                              TRUE ~ xy2zone(.data$start_coordinate_x, .data$start_coordinate_y, as_for_serve = FALSE)))
    ##    ggplot(plays, aes(start_coordinate_x, start_coordinate_y, fill = as.factor(start_zone))) + geom_tile() + datavolley::ggcourt()
    ##
    ##    plays$skill <- "Serve"
    ##    plays <- mutate(plays, start_zone = case_when(is.na(.data$start_coordinate_x) | is.na(.data$start_coordinate_y) ~ NA_integer_,
    ##                                              .data$skill %in% c("Serve", "Reception") ~ xy2zone(.data$start_coordinate_x, .data$start_coordinate_y, as_for_serve = TRUE),
    ##                                              TRUE ~ xy2zone(.data$start_coordinate_x, .data$start_coordinate_y, as_for_serve = FALSE)))
    ##    ggplot(plays, aes(start_coordinate_x, start_coordinate_y, fill = as.factor(start_zone))) + geom_tile() + datavolley::ggcourt()
    ##}

    for (pn in 1:6) {
        plays[[paste0("home_p", pn)]] <- dmapvalues(plays[[paste0("home_player_id", pn)]], meta$players_h$player_id, meta$players_h$number)
        plays[[paste0("visiting_p", pn)]] <- dmapvalues(plays[[paste0("visiting_player_id", pn)]], meta$players_v$player_id, meta$players_v$number)
    }

    ## add team_touch_id - an identifier of consecutive touches by same team in same point - e.g. a dig-set-attack sequence by one team is a "team touch"
    tid <- 0
    temp_ttid <- rep(NA, nrow(plays))
    temp_ttid[1] <- tid
    temp_team <- plays$team_id
    temp_ptid <- plays$point_id
    for (k in seq_len(nrow(plays))[-1]) {
        if (!identical(temp_team[k], temp_team[k-1]) || !identical(temp_ptid[k], temp_ptid[k-1]))  {
            tid <- tid+1
        }
        temp_ttid[k] <- tid
    }
    plays$team_touch_id <- temp_ttid
    plays$phase <- datavolley::play_phase(plays)

    ## populate skill_type with e.g. "Unknown serve reception type"
    plays$skill_type <- paste0("Unknown ", gsub("reception", "serve reception", tolower(plays$skill)), " type")

    ## num_players on block, and also propagated back one to the attack
    plays$num_players_numeric <- case_when(plays$skill %eq% "Block" & plays$eventgrade %eq% 2 ~ 1L,
                                           plays$skill %eq% "Block" & plays$eventgrade %eq% 3 ~ 2L, ## actually this is 2 or 3 players
                                           TRUE ~ plays$num_players_numeric)
    plays$num_players_numeric <- case_when(plays$skill %eq% "Attack" & lead(plays$skill) %eq% "Block" ~ lead(plays$num_players_numeric),
                                           TRUE ~ plays$num_players_numeric)
    plays$num_players <- case_when(plays$skill %eq% "Block" & plays$eventgrade %eq% 2 ~ "1 player block",
                                   plays$skill %eq% "Block" & plays$eventgrade %eq% 3 ~ "Multiplayer block",
                                   TRUE ~ plays$num_players)
    plays$num_players <- case_when(plays$skill %eq% "Attack" & lead(plays$skill) %eq% "Block" ~ lead(plays$num_players),
                                   TRUE ~ plays$num_players)
    
    ## these cols present but not populated (special_code, num_players, num_players_numeric skill_subtype partly pop)
## "skill_type" "attack_code" "attack_description" "set_code"
## [17] "set_description" "set_type" "start_zone" "end_zone"
## [21] "end_subzone" "num_players" "num_players_numeric"
## [25] "special_code" "point"

    nms <- c("match_id", "point_id", "time", "video_time", "code", "team", "player_number", "player_name", "player_id", "skill", "skill_type", "evaluation_code", "evaluation", "attack_code", "attack_description", "set_code", "set_description", "set_type", "start_zone", "end_zone", "end_subzone", "skill_subtype", "num_players", "num_players_numeric", "special_code", "timeout", "end_of_set", "substitution", "point", "home_team_score", "visiting_team_score", "home_setter_position", "visiting_setter_position", "custom_code", "file_line_number", "home_p1", "home_p2", "home_p3", "home_p4", "home_p5", "home_p6", "visiting_p1", "visiting_p2", "visiting_p3", "visiting_p4", "visiting_p5", "visiting_p6", "start_coordinate", "mid_coordinate", "end_coordinate", "start_coordinate_x", "start_coordinate_y", "mid_coordinate_x", "mid_coordinate_y", "end_coordinate_x", "end_coordinate_y", "home_player_id1", "home_player_id2", "home_player_id3", "home_player_id4", "home_player_id5", "home_player_id6", "visiting_player_id1", "visiting_player_id2", "visiting_player_id3", "visiting_player_id4", "visiting_player_id5", "visiting_player_id6", "set_number", "team_touch_id", "home_team", "visiting_team", "home_team_id", "visiting_team_id", "team_id", "point_won_by", "winning_attack", "serving_team", "phase")
    ## reorder cols
    plays <- plays[, c(intersect(nms, names(plays)), setdiff(names(plays), nms))]
    ##class(plays) <- c("datavolleyplays", class(plays))
    msgs <- do.call(rbind, lapply(msgs, as_tibble))
    list(meta = meta, file_meta = file_meta, messages = msgs, plays = plays)
}

#' A simple summary of a volleyball match
#'
#' @param object peranavolley: peranavolley object as returned by \code{pv_read}
#' @param ... : additional arguments (currently these have no effect)
#'
#' @return list of summary items
#'
#' @seealso \code{\link{pv_read}}
#' @examples
#' x <- pv_read(pv_example_file())
#' summary(x)
#'
#' @method summary peranavolley
#' @export
summary.peranavolley <- function(object, ...) {
    out <- list(date = object$meta$match$date, league = object$meta$match$league)
    out$teams <- object$meta$teams[, c("team", "coach", "assistant", "sets_won")]
    temp <- object$meta$result$score_home_team > object$meta$result$score_visiting_team
    out$set_scores <- object$meta$result[, c("score_home_team", "score_visiting_team")]
    ## make extra sure that set_scores has home team assigned correctly
    if (object$meta$teams$home_away_team[1] != "*") out$set_scores <- out$set_scores[, 2:1]
    out$set_scores <- na.omit(out$set_scores)
    out$duration <- sum(object$meta$result$duration, na.rm = FALSE)
    class(out) <- "summary.peranavolley"
    out
}

#' Print method for summary.peranavolley
#'
#' @param x summary.peranavolley: a summary.peranavolley object as returned by \code{summary.peranavolley}
#' @param ... : additional arguments (currently these have no effect)
#' @seealso \code{\link{summary.peranavolley}}
#' @method print summary.peranavolley
#' @export
print.summary.peranavolley <- function(x, ...) {
    out <- sprintf("Match summary:\nDate: %s\nLeague: %s\n", x$date, x$league)
    coaches1 <- paste(Filter(Negate(is.na), c(x$teams$coach[1], x$teams$assistant[1])), collapse = "/")
    coaches1 <- if (length(coaches1) > 0 && nzchar(coaches1)) paste0(" (", coaches1, ")") else ""
    coaches2 <- paste(Filter(Negate(is.na), c(x$teams$coach[2], x$teams$assistant[2])), collapse = "/")
    coaches2 <- if (length(coaches2) > 0 && nzchar(coaches2)) paste0(" (", coaches2, ")") else ""
    out <- sprintf("%sTeams: %s%s\n       vs\n       %s%s\n", out, x$teams$team[1], coaches1, x$teams$team[2], coaches2)
    out <- sprintf("%sResult: %d-%d (%s)\n", out, x$teams$sets_won[1], x$teams$sets_won[2], paste(x$set_scores$score_home_team, x$set_scores$score_visiting_team, sep = "-", collapse = ", "))
    out <- if (is.na(x$duration)) sprintf("%sDuration: unknown\n", out) else sprintf("%sDuration: %d minutes\n", out, x$duration)
    cat(out)
    invisible(out)
}

#' Extract the plays component from a peranavolley object
#'
#' @param x peranavolley: a peranavolley object as returned by \code{pv_read}
#'
#' @return The plays component of x (a data.frame)
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#' \dontrun{
#'   x <- pv_read(pv_example_file())
#'   inspect(plays(x))
#' }
#' @export
plays <- function(x) {
    if ("plays" %in% names(x)) x$plays else stop("input has no plays component")
}
