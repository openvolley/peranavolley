#' Generate more meaningful entries in plays data
#'
#' Notes:
#' \itemize{
#'   \item the rules are applied in order, so that a row in \code{x} that matches multiple rows in \code{remap$conditions} will have the first matching row applied to it. Similarly, if \code{remap} is provided as a list of remaps, then these will be applied in order and only the first matching one will be applied to any given row
#' }
#'
#' @param x data.frame or tibble: a peranavolley object as returned by \code{pv_read}, or the plays component thereof
#' @param remap list: a remapping is defined by a list with two data.frames. The data.frame named "conditions" defines the conditions to match on, and the one named "values" provides the new values to use. See the example provided and \code{link{pv_tas_remap}} for another. The \code{remap} parameter to \code{pv_recode} can be provided as one such remapping, or it can be provided as a list of remappings (i.e. a list of lists). In this case the list should be named, and those names will be used in the "changes" attribute (see \code{log_changes})
#' @param log_changes logical: if \code{TRUE}, the returned object will have a "changes" attribute describing the changes that were made
#'
#' @return A copy of \code{x} with new values applied
#'
#' @examples
#'
#' ## read file
#' x <- pv_read(pv_example_file())
#'
#' ## construct the remapping
#' my_remap <- list(conditions =
#'                    data.frame(attack_code = c("1", NA_character_, "4", "5"),
#'                               start_zone = c(NA, 3, 2, 2),
#'                               stringsAsFactors = FALSE),
#'                  values =
#'                    data.frame(attack_code = c("X1", "Z3", "X6", "V6"),
#'                               stringsAsFactors = FALSE))
#'
#' ## meaning that: any attack with attack_code 1 will be recoded as an "X1" attack
#' ##               any attack from start_zone 3 will be recoded as an "Z3" attack
#' ##               any attack from start_zone 2 with attack_code 4 will be recoded as an "X6" attack
#' ##               any attack from start_zone 2 with attack_code 5 will be recoded as an "V6" attack
#'
#' x <- pv_recode(x, remap = my_remap)
#'
#' @export
pv_recode <- function(x, remap, log_changes = FALSE) {
    if (is.list(x) && !is.data.frame(x)) {
        if ("plays" %in% names(x)) {
            xp <- x$plays
        } else {
            stop("x must be a list with 'plays' component, or a data.frame")
        }
    } else {
        xp <- x
    }
    if (!is.data.frame(xp)) {
        stop("x must be a list with 'plays' component, or a data.frame")
    }
    assert_that(is.flag(log_changes), !is.na(log_changes))
    chng <- tibble(change = character(), n = integer(), rows = list())
    dolog <- function(change, rows) {
        if (log_changes) {
            if (is.logical(rows)) rows <- which(rows)
            if (length(rows) > 0) chng <<- bind_rows(chng, tibble(change = change, n = length(rows), rows = list(rows)))
        }
        invisible(TRUE)
    }
    check_remap_ok(remap)
    if (!remap_is_list(remap)) {
        remap_list <- list("Unnamed remappings" = remap)
    } else {
        remap_list <- remap
        if (is.null(names(remap_list))) names(remap_list) <- paste0("Unnamed remapping ", seq_along(remap_list))
    }
    have_modified <- rep(FALSE, nrow(xp))
    for (rmi in seq_along(remap_list)) {
        remap <- remap_list[[rmi]]
        conds <- remap$conditions
        if (ncol(conds) < 1 || nrow(conds) < 1) stop("no data in remap$conditions")
        vals <- remap$values
        if (nrow(conds) != nrow(vals)) stop("number of rows in remap$conditions does not match number of rows in remap$values")
        if (!all(names(conds) %in% names(xp))) stop("one or more columns in remap$conditions missing from x (", paste(setdiff(names(conds), names(xp)), collapse = ", "), ")")
        ## in case remap was constructed using data.frame and stringsAsFactors = TRUE
        for (ci in seq_len(ncol(conds))) {
            if (is.factor(conds[[ci]])) conds[[ci]] <- as.character(conds[[ci]])
        }
        for (ci in seq_len(ncol(vals))) {
            if (is.factor(vals[[ci]])) vals[[ci]] <- as.character(vals[[ci]])
        }
        ## now go through each row in remap and apply the rules
        cond_cols <- names(conds)
        val_cols <- names(vals)
        for (ri in seq_len(nrow(conds))) {
            idx <- lapply(cond_cols, function(cond) (!is.na(conds[[cond]][ri]) & x[[cond]] %eq% conds[[cond]][ri]) | is.na(conds[[cond]][ri]))
            idx <- do.call(cbind, idx)
            idx <- apply(idx, 1, all) & !have_modified
            have_modified <- have_modified | idx
            if (any(idx)) {
                rulename <- names(remap_list)[rmi]
                if (nrow(conds) > 1) rulename <- paste0(rulename, " (row ", ri, ")")
                dolog(rulename, rows = idx)
                for (vc in val_cols) xp[[vc]][idx] <- vals[[vc]][ri]
            }
        }
    }
    if (is.list(x) && !is.data.frame(x) && "plays" %in% names(x)) {
        x$plays <- xp
        if (log_changes) attr(x, "changes") <- chng
        x
    } else {
        if (log_changes) attr(xp, "changes") <- chng
        xp
    }
}

## remap is a list of remap lists
remap_is_list <- function(remap) {
    is.list(remap) && all(vapply(remap, is.list, FUN.VALUE = TRUE)) && !any(vapply(remap, is.data.frame, FUN.VALUE = TRUE))
}

check_remap_ok <- function(remap) {
    if (remap_is_list(remap)) {
        return(all(vapply(remap, check_remap_ok, FUN.VALUE = TRUE)))
    }
    if (!is.list(remap) || !setequal(names(remap), c("conditions", "values")) || !is.data.frame(remap$conditions) || !is.data.frame(remap$values))
        stop("remap should be a list with 'conditions' and 'values' elements, each of which must be a data.frame (or it can be a list of such lists)")
    invisible(TRUE)
}

#' An opinionated recoding
#'
#' Note that this function is only likely to be useful if you have scouted your VBStats files following the conventions described at https://raymondben.github.io/scouting-notes/
#'
#' This function modifies the plays component of a peranavolley object, following the scouting conventions described at https://raymondben.github.io/scouting-notes/. The following changes are made:
#' \itemize{
#'   \item attacks marked as "Setter tip" ("Dump" in VBStats) by a setter are mapped to attack_code "PP"
#'   \item any "Setter tip" by a non-setter are treated as freeballs. Their skill entry is changed to "Freeball", and attack code to NA
#'   \item attack_code 1 to "X1"
#'   \item attack_code 2 to "X2"
#'   \item attack_code 3 to "X7"
#'   \item attack_code 4 to "CF" (slide)
#'   \item attack code 5 treated as highball, with new attack code determined by start_zone (V5, V6, etc)
#'   \item any other attack (with NA attack code) treated as standard tempo with new attack code determined by start_zone (X5, X6, etc)
#'   \item any dig following a freeball over is changed to skill "Freeball"
#' }
#'
#' @param x data.frame or tibble: a peranavolley object as returned by \code{pv_read}, or the plays component thereof
#' @param remap list: a list with components "conditions" and "values" that define the remapping. See \code{\link{pv_tas_remap}} for an example
#' @param log_changes logical: if \code{TRUE}, the returned object will have a "changes" attribute describing the changes that were made
#'
#' @return A modified version of \code{x}.
#'
#' @examples
#'
#' ## Note: this is a silly example, because the example VBStats file was NOT
#' ##       scouted with the conventions expected by this function
#'
#' x <- pv_read(pv_example_file())
#' x <- pv_tas_recode(x, remap = pv_tas_remap(), log_changes = TRUE)
#'
#' @export
pv_tas_recode <- function(x, remap = pv_tas_remap(), log_changes = FALSE) {
    if (is.list(x) && !is.data.frame(x)) {
        if ("plays" %in% names(x)) {
            xp <- x$plays
        } else {
            stop("x must be a list with 'plays' component, or a data.frame")
        }
    } else {
        xp <- x
    }
    if (!is.data.frame(xp)) {
        stop("x must be a list with 'plays' component, or a data.frame")
    }
    if (!all(c("skill") %in% names(xp))) stop("x must contain at least the column 'skill' - is it a peranavolley object or the plays component of one?")
    check_remap_ok(remap)
    assert_that(is.flag(log_changes), !is.na(log_changes))
    chng <- tibble(change = character(), n = integer(), rows = list())
    dolog <- function(change, rows) {
        if (log_changes) {
            if (is.logical(rows)) rows <- which(rows)
            if (length(rows) > 0) chng <<- bind_rows(chng, tibble(change = change, n = length(rows), rows = list(rows)))
        }
        invisible(TRUE)
    }

    ## figure out where setter is
    temp <- t(as.matrix(xp[, paste0("home_player_id", 1:6)])) ## 6 x n
    xp$ZZZ_home_setter_id <- temp[xp$home_setter_position + (seq_len(nrow(xp))-1)*6]
    temp <- t(as.matrix(xp[, paste0("visiting_player_id", 1:6)])) ## 6 x n
    xp$ZZZ_visiting_setter_id <- temp[xp$visiting_setter_position + (seq_len(nrow(xp))-1)*6]
    xp <- mutate(xp, ZZZ_player_role = case_when(.data$team %eq% .data$home_team & .data$player_id %eq% .data$ZZZ_home_setter_id ~ "setter",
                                                 .data$team %eq% .data$visiting_team & .data$player_id %eq% .data$ZZZ_visiting_setter_id ~ "setter"))
    if (remap_is_list(remap)) {
        for (rmi in seq_along(remap)) {
            names(remap[[rmi]]$conditions)[names(remap[[rmi]]$conditions) %eq% "player_role"] <- "ZZZ_player_role"
        }
    } else {
        names(remap$conditions)[names(remap$conditions) %eq% "player_role"] <- "ZZZ_player_role"
    }
    xp <- pv_recode(xp, remap = remap, log_changes = log_changes)
    if (log_changes) {
        chng <- bind_rows(chng, attr(xp, "changes"))
        attr(xp, "changes") <- NULL
    }
    ## fix evaluations of freeballs
    ## because they start scouted as attacks, they end up as
    ## = Error; / Blocked; ~ Spike in play; # Winning attack
    fidx <- xp$skill %eq% "Freeball"
    xp$evaluation[fidx & xp$evaluation_code %eq% "~"] <- "Freeball in play"
    xp$evaluation[fidx & xp$evaluation_code %eq% "#"] <- "Winning freeball"
    ## what was an A# will now be a F#, with evaluation "Winning freeball"
    ## This *should* be followed by a dig error, but the scout might not have entered that
    ## digs on freeballs over are treated as freeball passes
    idx <- xp$skill %eq% "Dig" & lag(xp$skill) %eq% "Freeball" & !lag(xp$team) %eq% xp$team
    xp$skill[idx] <- "Freeball"
    xp$skill_type[idx] <- "Unknown freeball type"
    xp$evaluation[idx] <- sub(" dig", " freeball", xp$evaluation[idx])
    dolog(change = "Digs following freeballs over changed to freeballs", rows = idx)

    ## attack on first team contact to PR
    ## this one is particularly prone to missed actions, leave for now
    ##idx <- xp$skill %eq% "Attack" & !lag(xp$team) %eq% xp$team
    ##xp$skill_type[idx] <- "Other attack"
    ##xp$attack_code[idx] <- "PR"
    ##dolog(change = "First-contact attacks changed to attack code PR", rows = idx)

    ## other possibilities:
    ## "spike in play" followed by opposite team block (control) then another touch by the attacking team
    ##  should be given evaluation "Blocked for reattack" with evaluation_code "!"

    ## drop those extra cols in case they interfere with downstream processing
    xp <- xp[, setdiff(names(xp), c("ZZZ_home_setter_id", "ZZZ_visiting_setter_id", "ZZZ_player_role"))]
    if (is.list(x) && !is.data.frame(x) && "plays" %in% names(x)) {
        x$plays <- xp
        if (log_changes) attr(x, "changes") <- chng
        x
    } else {
        if (log_changes) attr(xp, "changes") <- chng
        xp
    }
}

#' @rdname pv_tas_recode
#' @export
pv_tas_live_recode <- function(x, remap = pv_tas_remap(), log_changes = FALSE) {
    x <- pv_tas_recode(x, remap = remap, log_changes = log_changes)
    ## need to fix phase: first fix team_touch_id
    plays <- x$plays
    tid <- 0
    temp_ttid <- rep(NA, nrow(plays))
    temp_ttid[1] <- tid
    temp_team <- plays$team_id
    temp_ptid <- plays$point_id
    temp_skill <- plays$skill
    had_attack <- FALSE
    for (k in seq_len(nrow(plays))[-1]) {
        ## the team touch has ended if this skill is by a different team, or the point_id has changed, or we have already seen an attack from this team during this touch (in which case the first attack was the reception attack, and now we are seeing the end-of-point attack by the same team)
        if (!identical(temp_team[k], temp_team[k-1]) || !identical(temp_ptid[k], temp_ptid[k-1]) || (had_attack && temp_skill[k] %eq% "Attack"))  {
            tid <- tid+1
            had_attack <- FALSE
        } else {
            if (temp_skill[k] %eq% "Attack") had_attack <- TRUE
        }
        temp_ttid[k] <- tid
    }
    plays$team_touch_id <- temp_ttid
    plays$phase <- datavolley::play_phase(plays)
    x$plays <- plays
    x
}

# this in the old all-in-one-tibble format, can be deleted later
# @export
# @rdname pv_tas_recode
#pv_tas_remap <- function() {
#    temp <- tribble(~skill, ~attack_code, ~start_zone, ~player_role, ~skill_subtype, ~new_attack_code, ~new_skill, ~new_skill_type,
#                    "Attack", NA, NA, "setter",      "Setter tip", "PP",            "Attack",   "Other attack",
#                    "Attack", NA, NA, NA_character_, "Setter tip",  NA_character_,  "Freeball", "Unknown freeball type",
#                    "Attack", 1,  NA, NA_character_, NA_character_, "X1",           "Attack",   "Quick ball attack",
#                    "Attack", 2,  NA, NA_character_, NA_character_, "X2",           "Attack",   "Quick ball attack",
#                    "Attack", 3,  NA, NA_character_, NA_character_, "X7",           "Attack",   "Quick ball attack",
#                    "Attack", 4,  NA, NA_character_, NA_character_, "CF",           "Attack",   "Quick ball attack",
#                    "Attack", 5,  3,  NA_character_, NA_character_, "V3",           "Attack",   "Other attack",
#                    "Attack", 5,  4,  NA_character_, NA_character_, "V5",           "Attack",   "High ball attack",
#                    "Attack", 5,  2,  NA_character_, NA_character_, "V6",           "Attack",   "High ball attack",
#                    "Attack", 5,  7,  NA_character_, NA_character_, "V0",           "Attack",   "High ball attack",
#                    "Attack", 5,  5,  NA_character_, NA_character_, "V0",           "Attack",   "High ball attack",
#                    "Attack", 5,  8,  NA_character_, NA_character_, "VP",           "Attack",   "High ball attack",
#                    "Attack", 5,  6,  NA_character_, NA_character_, "VP",           "Attack",   "High ball attack",
#                    "Attack", 5,  9,  NA_character_, NA_character_, "V8",           "Attack",   "High ball attack",
#                    "Attack", 5,  1,  NA_character_, NA_character_, "V8",           "Attack",   "High ball attack",
#                    "Attack", 5,  NA, NA_character_, NA_character_, NA_character_,  "Attack",   "Unknown attack type",## unknown
#                    "Attack", NA, 4,  NA_character_, NA_character_, "X5",           "Attack",   "Head ball attack",
#                    "Attack", NA, 2,  NA_character_, NA_character_, "X6",           "Attack",   "Head ball attack",
#                    "Attack", NA, 7,  NA_character_, NA_character_, "X0",           "Attack",   "Head ball attack",
#                    "Attack", NA, 5,  NA_character_, NA_character_, "X0",           "Attack",   "Head ball attack",
#                    "Attack", NA, 8,  NA_character_, NA_character_, "XP",           "Attack",   "Head ball attack",
#                    "Attack", NA, 6,  NA_character_, NA_character_, "XP",           "Attack",   "Half ball attack",
#                    "Attack", NA, 9,  NA_character_, NA_character_, "X8",           "Attack",   "Head ball attack",
#                    "Attack", NA, 1,  NA_character_, NA_character_, "X8",           "Attack",   "Head ball attack",
#                    "Attack", NA, NA, NA_character_, NA_character_, NA_character_,  "Attack",   "Unknown attack type")
#    vals <- temp[, grepl("^new_", names(temp))]
#    names(vals) <- sub("^new_", "", names(vals))
#    list(conditions = temp[, !grepl("^new_", names(temp))], values = vals)
#
#}

#' @export
#' @rdname pv_tas_recode
pv_tas_remap <- function() {
list("Setter (high) dumps to freeball over" = list(conditions = tibble(skill = "Attack", player_role = "setter", skill_subtype = "Setter tip", attack_code = 5),
                                                   values = tibble(attack_code = NA_character_, skill = "Freeball", skill_type = "Unknown freeball type", skill_subtype = NA_character_)),
     "Setter dumps to PP attacks" = list(conditions = tibble(skill = "Attack", player_role = "setter", skill_subtype = "Setter tip"),
                                         values = tibble(attack_code = "PP", skill_type = "Other attack")),
     "Dumps by non-setter to freeball over" = list(conditions = tibble(skill = "Attack", skill_subtype = "Setter tip"),
                                                   values = tibble(attack_code = NA_character_, skill = "Freeball", skill_type = "Unknown freeball type", skill_subtype = NA_character_)),
     "Setting zone 1 attacks to X1" = list(conditions = tibble(skill = "Attack", attack_code = 1),
                                           values = tibble(attack_code = "X1", skill_type = "Quick ball attack")),
     "Setting zone 2 attacks to X2" = list(conditions = tibble(skill = "Attack", attack_code = 2),
                                           values = tibble(attack_code = "X2", skill_type = "Quick ball attack")),
     "Setting zone 3 attacks to X7" = list(conditions = tibble(skill = "Attack", attack_code = 3),
                                           values = tibble(attack_code = "X7", skill_type = "Quick ball attack")),
     "Setting zone 4 attacks to CF" = list(conditions = tibble(skill = "Attack", attack_code = 4),
                                           values = tibble(attack_code = "CF", skill_type = "Quick ball attack")),
     "Setting zone 5 attacks to high ball equivalents" = list(conditions = tribble(~skill, ~attack_code, ~start_zone,
                                                                                   "Attack", 5,  3,
                                                                                   "Attack", 5,  4,
                                                                                   "Attack", 5,  2,
                                                                                   "Attack", 5,  7,
                                                                                   "Attack", 5,  5,
                                                                                   "Attack", 5,  8,
                                                                                   "Attack", 5,  6,
                                                                                   "Attack", 5,  9,
                                                                                   "Attack", 5,  1,
                                                                                   "Attack", 5,  NA),
                                                              values = tribble(~attack_code, ~skill_type,
                                                                               "V3", "Other attack",
                                                                               "V5", "High ball attack",
                                                                               "V6", "High ball attack",
                                                                               "V0", "High ball attack",
                                                                               "V0", "High ball attack",
                                                                               "VP", "High ball attack",
                                                                               "VP", "High ball attack",
                                                                               "V8", "High ball attack",
                                                                               "V8", "High ball attack",
                                                                               NA_character_, "Unknown attack type")),
     "Attacks without setting zone to normal-tempo equivalents" = list(conditions = tribble(~skill, ~start_zone,
                                                                                            "Attack", 4,
                                                                                            "Attack", 2,
                                                                                            "Attack", 7,
                                                                                            "Attack", 5,
                                                                                            "Attack", 8,
                                                                                            "Attack", 6,
                                                                                            "Attack", 9,
                                                                                            "Attack", 1),
                                                                       values = tribble(~attack_code, ~skill_type,
                                                                                        "X5", "Head ball attack",
                                                                                        "X6", "Head ball attack",
                                                                                        "X0", "Head ball attack",
                                                                                        "X0", "Head ball attack",
                                                                                        "XP", "Head ball attack",
                                                                                        "XP", "Half ball attack",
                                                                                        "X8", "Head ball attack",
                                                                                        "X8", "Head ball attack")))
}


# hard-deprecated in favour of pv_recode
# Generate more meaningful attack_code values
#
# Notes:
# \itemize{
#   \item "setting zones" (terminology from the VBStats app) are called "attack codes" here for consistency with the datavolley package
#   \item the rules are applied in order, so that an attack in \code{x} that matches multiple rows in \code{remap} will have the first matching row applied to it
# }
#
# @param x data.frame or tibble: a peranavolley object as returned by \code{pv_read}, or the plays component thereof
# @param apply_to_skills character: rules will only be applied to rows corresponding to these skills. This function is nominally intended for recoding \code{attack_code} values (hence \code{apply_to_skills = "Attacks"} is the default). But it might be useful for other purposes, in which case other skills might need to be specified here
# @param remap data.frame or tibble: this data.frame must contain the column \code{new_attack_code}, and one or more columns that define the conditions to match on. See the example below for more detail
#
# @return A character vector with new \code{attack_code} values
#
# @examples
#
# ## read file
# x <- pv_read(pv_example_file())
#
# ## construct the remapping
# remap <- data.frame(attack_code = c("1", NA_character_, "4", "5"),
#                     start_zone = c(NA, 3, 2, 2),
#                     new_attack_code = c("X1", "Z3", "X6", "V6"),
#                     stringsAsFactors = FALSE)
# ## meaning that: any attack with attack_code 1 will be recoded as an "X1" attack
# ##               any attack from start_zone 3 will be recoded as an "Z3" attack
# ##               any attack from start_zone 2 with attack_code 4 will be recoded as an "X6" attack
# ##               any attack from start_zone 2 with attack_code 5 will be recoded as an "V6" attack
#
# x$plays$attack_code <- pv_recode_attacks(x, remap = remap)
#
# @export
#pv_recode_attacks <- function(x, apply_to_skills = "Attack", remap) {
#    if (is.list(x) && !is.data.frame(x)) {
#        if ("plays" %in% names(x)) {
#            x <- x$plays
#        } else {
#            stop("x must be a list with 'plays' component, or a data.frame")
#        }
#    }
#    if (!is.data.frame(x)) {
#        stop("x must be a list with 'plays' component, or a data.frame")
#    }
#    if (!all(c("skill") %in% names(x))) stop("x must contain at least the column 'skill' - is it a peranavolley object or the plays component of one?")
#    assert_that(is.data.frame(remap))
#    if (!all(c("new_attack_code") %in% names(remap)) || ncol(remap) < 2) {
#        stop("remap must contain the column 'new_attack_code' plus at least one condition column")
#    }
#    ## in case remap was constructed using data.frame and stringsAsFactors = TRUE
#    for (ci in seq_len(ncol(remap))) {
#        if (is.factor(remap[[ci]])) remap[[ci]] <- as.character(remap[[ci]])
#    }
#    assert_that(is.character(remap$new_attack_code))
#    new_attack_code <- x$attack_code
#    ## now go through each row in remap and apply the rules
#    have_warned <- FALSE
#    have_modified <- rep(FALSE, nrow(x))
#    cond_cols <- setdiff(names(remap), "new_attack_code")
#    if (length(cond_cols) < 1) stop("no conditions in remap") ## should already have been caught by the check above
#    skill_matched <- x$skill %in% apply_to_skills
#    for (ri in seq_len(nrow(remap))) {
#        if (all(is.na(remap[ri, cond_cols])) && !is.na(remap$new_attack_code[ri])) {
#            warning("remap has at least one row with all NA values: this will match all rows in x that have a skill in apply_to_skills. Did you mean this?")
#            have_warned <- TRUE
#        }
#        idx <- lapply(cond_cols, function(cond) (!is.na(remap[[cond]][ri]) & x[[cond]] %eq% remap[[cond]][ri]) | is.na(remap[[cond]][ri]))
#        if (ri < 2) {
#            ## check on first iteration that all cols in remap are present in x
#            if (any(vapply(idx, length, FUN.VALUE = 0) < 1)) stop("x is missing at least one column that is present in remap")
#        }
#        idx <- do.call(cbind, idx)
#        idx <- apply(idx, 1, all) & skill_matched & !have_modified
#        have_modified <- have_modified | idx
#        new_attack_code[idx] <- remap$new_attack_code[ri]
#    }
#    new_attack_code
#}


# @export
# @rdname pv_tas_recode
#pv_tas_attack_remap <- function() {
#    tribble(~attack_code, ~start_zone, ~player_role, ~skill_subtype, ~new_attack_code,
#            NA, NA, "setter", "Setter tip", "PP",
#            NA, NA, NA_character_, "Setter tip", NA_character_,
#            1, NA, NA_character_, NA_character_, "X1",
#            2, NA, NA_character_, NA_character_, "X2",
#            3, NA, NA_character_, NA_character_, "X7",
#            4, NA, NA_character_, NA_character_, "CF",
#            5, 3, NA_character_, NA_character_, "V3",
#            5, 4, NA_character_, NA_character_, "V5",
#            5, 2, NA_character_, NA_character_, "V6",
#            5, 7, NA_character_, NA_character_, "V0",
#            5, 5, NA_character_, NA_character_, "V0",
#            5, 8, NA_character_, NA_character_, "VP",
#            5, 6, NA_character_, NA_character_, "VP",
#            5, 9, NA_character_, NA_character_, "V8",
#            5, 1, NA_character_, NA_character_, "V8",
#            5, NA, NA_character_, NA_character_, NA_character_, ## unknown
#            NA, 4, NA_character_, NA_character_, "X5",
#            NA, 2, NA_character_, NA_character_, "X6",
#            NA, 7, NA_character_, NA_character_, "X0",
#            NA, 5, NA_character_, NA_character_, "X0",
#            NA, 8, NA_character_, NA_character_, "XP",
#            NA, 6, NA_character_, NA_character_, "XP",
#            NA, 9, NA_character_, NA_character_, "X8",
#            NA, 1, NA_character_, NA_character_, "X8",
#            NA, NA, NA_character_, NA_character_, NA_character_)
#}
### backwards compatibility, unexported
#ex_attack_code_remap <- pv_tas_attack_remap
