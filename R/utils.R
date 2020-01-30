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
#' @param home_team_rotation string: fixed rotation of the home team, either "SHM" or "SMH". This will be used to infer missing attack \code{start_zone} values (assuming the standard attack by role - e.g. the default attack for an opposite is X6 when they are front row (except in P1 reception when it is X5), and X8 when they are back row). If \code{home_team_rotation} is \code{NULL} this will not be done
#' @param visiting_team_rotation string: as for \code{home_team_rotation}, but for the visiting team
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
    chng <- tibble(change = character(), n = integer(), rows = list())
    if (!is.null(xp) && nrow(xp) > 0) {
        if (!all(c("skill") %in% names(xp))) stop("x must contain at least the column 'skill' - is it a peranavolley object or the plays component of one?")
        check_remap_ok(remap)
        assert_that(is.flag(log_changes), !is.na(log_changes))
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
        ## add freeball_over col
        xp$freeball_over <- xp$skill %eq% "Freeball"
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
        ## this handled (commented out) for middles in pv_tas_recode BUT IT WON'T WORK YET BECAUSE WE DON'T HAVE PLAYER ROLES OTHER THAN SETTER/LIBERO

        ## - when attack skill_subtype is "Spike off the block" (subevent=4) and it's NOT a kill AND there's no following block THEN insert a block control (unknown player)
        ##idx <- xp$skill %eq% "Attack" & xp$skill_subtype %eq% "Spike off the block" & evaluation %eq% "Spike in play" & !lead(xp$skill) %eq% "Block"
        ## TODO

        ## other possibilities:
        ## "spike in play" followed by opposite team block (control) then another touch by the attacking team
        ##  should be given evaluation "Blocked for reattack" with evaluation_code "!"

        ## drop those extra cols in case they interfere with downstream processing
        xp <- xp[, setdiff(names(xp), c("ZZZ_home_setter_id", "ZZZ_visiting_setter_id", "ZZZ_player_role"))]

        ## fill in end_cone from end coordinate and start zone
        aidx <- xp$skill %eq% "Attack"
        xp$end_cone[aidx] <- dv_xy2cone(xp$end_coordinate[aidx], start_zones = xp$start_zone[aidx])
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

#' @rdname pv_tas_recode
#' @export
pv_tas_live_recode <- function(x, remap = pv_tas_remap(), home_team_rotation = NULL, visiting_team_rotation = NULL, log_changes = FALSE) {
    if (!inherits(x, c("peranavolley"))) stop("x must be a peranavolley object")
    ## if home or visiting team rotations have been supplied, use them to infer attack start positions
    ## do this before calling pv_tas_recode, so that the right attack codes get assigned
    if (nrow(x$plays) < 1 || is.null(x$plays)) return(x) ## nothing to do
    rx <- NULL
    vrx <- NULL
    if (!is.null(home_team_rotation)) {
        assert_that(is.string(home_team_rotation))
        home_team_rotation <- match.arg(toupper(home_team_rotation), c("SHM", "SMH"))
        if (home_team_rotation %in% c("SHM", "SMH")) {
            rx <- infer_playing_positions_by_rotation(x$plays, rotation = home_team_rotation, method = "standard", team = "home")
        }
    }
    if (!is.null(visiting_team_rotation)) {
        assert_that(is.string(visiting_team_rotation))
        visiting_team_rotation <- match.arg(toupper(visiting_team_rotation), c("SHM", "SMH"))
        if (visiting_team_rotation %in% c("SHM", "SMH")) {
            vrx <- infer_playing_positions_by_rotation(x$plays, rotation = visiting_team_rotation, method = "standard", team = "visiting")
        }
    }
    rx <- bind_rows(rx, vrx)
    if (!is.null(rx) && nrow(rx) > 0) {
        rx$playing_position[rx$playing_position %eq% 5] <- 7L
        rx$playing_position[rx$playing_position %eq% 6] <- 8L
        rx$playing_position[rx$playing_position %eq% 1] <- 9L
        temp <- distinct(rx[, c("match_id", "point_id", "player_id", "playing_position")])
        chk0 <- nrow(x$plays)
        x$plays <- left_join(x$plays, temp, by = c("match_id", "point_id", "player_id"))
        if (nrow(x$plays) != chk0) warning("inferring player positions added ", nrow(x$plays) - chk0, " data rows")
        ## now fill in missing attack start zones
        idx <- x$plays$skill %eq% "Attack" & is.na(x$plays$start_zone)
        x$plays$start_zone[idx] <-  x$plays$playing_position[idx]
        x$plays <- dplyr::select(x$plays, -"playing_position")
    }
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


#' Augment partially-scouted data
#'
#' @param x peranavolley: a peranavolley object as returned by \code{\link{pv_read}}
#'
#' @return A peranavolley object with additional rows added to its \code{plays} component.
#'
#' @seealso \code{\link{pv_read}}, \code{\link{pv_tas_recode}}, \code{\link{pv_tas_live_recode}}
#'
#' @export
pv_tas_data_augment <- function(x) {
    assert_that(inherits(x, "peranavolley"))
    plays <- dplyr::filter(mutate(x$plays, gameIndex = row_number()),
                           .data$skill != "Set" | (.data$skill == "Set" & .data$evaluation == "Error") | is.na(.data$skill))
    plays_try <- NULL
    for (teamNo in 1:2) {
        scoutedTeam <- x$meta$teams$team[teamNo]
        scoutedTeam_id <- x$meta$teams$team_id[x$meta$teams$team %in% scoutedTeam]
        otherTeam <- x$meta$teams$team[x$meta$teams$team != scoutedTeam]
        otherTeam_id <- x$meta$teams$team_id[x$meta$teams$team != scoutedTeam]
        unassigned_player_id <- paste0(toupper(stringi::stri_rand_strings(7, 6, pattern = "[A-Za-z0-9]")), collapse = "-")
        unassigned_player_id_scouted <- paste0(toupper(stringi::stri_rand_strings(7, 6, pattern = "[A-Za-z0-9]")), collapse = "-")

        ## Add Serve for other team prior to scoutingTeam reception (when missing)
        plays_tmp_serve <- dplyr::filter(plays, .data$skill == "Reception" & .data$team %in% scoutedTeam & lag(.data$skill) != "Serve")
        plays_tmp_serve <- mutate(plays_tmp_serve, team = otherTeam, team_id = otherTeam_id,
                                  gameIndex = .data$gameIndex - 0.1,
                                  skill = "Serve", skill_type = "Unknown serve type",
                                  evaluation = case_when(.data$evaluation == "Error" ~ "Ace",
                                                         .data$evaluation == "Negative/poor pass" ~ "Positive, opponent some attack",
                                                         .data$evaluation == "OK, no first tempo possible" ~ "OK, no first tempo possible",
                                                         .data$evaluation ==  "Perfect/positive pass" ~ "Negative, opponent free attack"),
                                  evaluation_code = case_when(.data$evaluation == "Ace" ~ "#",
                                                              .data$evaluation == "Positive, opponent some attack" ~ "+",
                                                              .data$evaluation == "OK, no first tempo possible" ~ "!",
                                                              .data$evaluation ==  "Negative, opponent free attack" ~ "-"),
                                  ## positions for serve might have been entered on reception, so inherit all these
                                  ## BUT note that pv_read already shuffles the serve/reception coords, so by this point any reception
                                  ##  coordinates scouted without a corresponding serve will have been lost - FIX TODO
                                  ##start_zone = NA_integer_, end_zone = NA_integer_, end_subzone = NA_character_,
                                  ##start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                                  ##start_coordinate_x = NA, start_coordinate_y = NA_real_,
                                  ##mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                                  ##mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                                  ##ballstartstring = NA_character_, ballmidstring = NA_character_, ballendstring = NA_character_,
                                  player_name = "unassigned", player_id = unassigned_player_id, player_number = NA_integer_,
                                  team_touch_id = NA_integer_,
                                  phase = "Reception",
                                  oppositionscore_tmp = .data$teamscore, teamscore = .data$oppositionscore,
                                  win_loss = case_when(.data$evaluation == "Error" ~ -1,
                                                       .data$evaluation == "Ace" ~ 1,
                                                       TRUE ~ 0))
        plays_tmp_serve <- dplyr::rename(dplyr::select(plays_tmp_serve, -"oppositionscore"), oppositionscore = "oppositionscore_tmp")

        ## Add Reception for other team after scoutedTeam serve (when missing)
        plays_tmp_reception <- dplyr::filter(plays, .data$skill == "Serve" & .data$team %in% scoutedTeam & .data$evaluation != "Error" & !(lead(.data$skill) == "Reception" & lead(.data$team) == otherTeam))
        plays_tmp_reception <- mutate(plays_tmp_reception, team = otherTeam, team_id = otherTeam_id,
                                      gameIndex = .data$gameIndex + 0.1,
                                      skill = "Reception", skill_type = paste(.data$skill_type, "reception"),
                                      evaluation = case_when(.data$evaluation == "Ace" ~ "Error",
                                                             .data$evaluation == "Positive, opponent some attack" ~ "Negative/poor pass",
                                                             .data$evaluation == "OK, no first tempo possible" ~ "OK, no first tempo possible",
                                                             .data$evaluation == "Negative, opponent free attack" ~ "Perfect/positive pass"),
                                      evaluation_code =  case_when(.data$evaluation == "Error" ~ "=",
                                                                   .data$evaluation == "Negative/poor pass" ~ "-/",
                                                                   .data$evaluation == "OK, no first tempo possible" ~ "!",
                                                                   .data$evaluation == "Perfect/positive pass" ~ "#+"),
                                      ## start and end locations of reception are identical to the serve, so let these be inherited
                                      ##start_zone = .data$start_zone, end_zone = NA_integer_, end_subzone = NA_character_,
                                      ##start_coordinate = .data$start_coordinate, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                                      ##start_coordinate_x = .data$start_coordinate_x, start_coordinate_y = .data$start_coordinate_y,
                                      ##mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                                      ##mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                                      ##ballstartstring = .data$ballstartstring, ballmidstring = NA_character_, ballendstring = NA_character_,
                                      player_name = "unassigned", player_id = unassigned_player_id,
                                      player_number = NA_integer_,
                                      team_touch_id = NA_integer_,
                                      phase = "Reception",
                                      oppositionscore_tmp = .data$teamscore, teamscore = .data$oppositionscore,
                                      win_loss = case_when(.data$evaluation == "Error" ~ -1,
                                                           TRUE ~ 0))
        plays_tmp_reception <- dplyr::rename(dplyr::select(plays_tmp_reception, -"oppositionscore"), oppositionscore = "oppositionscore_tmp")

        ## Add Attack for otherTeam before dig or block from scoutedTeam
        plays_tmp_attack <- dplyr::filter(plays, .data$skill %in% c("Dig", "Block") & .data$team %in% scoutedTeam & !(lag(.data$skill) %in% c("Attack", "Freeball")) & lag(.data$team) %in% otherTeam)
        plays_tmp_attack <- mutate(plays_tmp_attack, team = otherTeam, team_id = otherTeam_id,
                                   gameIndex = .data$gameIndex - 0.1,
                                   skill = "Attack", skill_type = "Unknown attack type", skill_subtype = "Unknown attack subtype",
                                   evaluation = "Spike in play", evaluation_code =  "~", ## TODO what if block kill/error/invasion or dig error
                                   start_zone = NA_integer_, end_zone = NA_integer_, end_subzone = NA_character_,
                                   start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                                   start_coordinate_x = NA_real_, start_coordinate_y = NA_real_,
                                   mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                                   mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                                   player_name = "unassigned", player_id = unassigned_player_id,
                                   player_number = NA_integer_,
                                   team_touch_id = NA_integer_,
                                   phase = NA_character_,
                                   ballstartstring = NA_character_, ballmidstring = NA_character_, ballendstring = NA_character_,
                                   oppositionscore_tmp = .data$teamscore, teamscore = .data$oppositionscore,
                                   win_loss = 0)
        plays_tmp_attack <- dplyr::rename(dplyr::select(plays_tmp_attack, -"oppositionscore"), oppositionscore = "oppositionscore_tmp")

        ## Add Dig for other team after in play attack from scoutedTeam
        plays_tmp_dig <- dplyr::filter(plays, .data$skill == "Attack" & .data$team %in% scoutedTeam & .data$evaluation == "Spike in play" & lead(.data$skill) != "Dig")
        plays_tmp_dig <- mutate(plays_tmp_dig, team = otherTeam, team_id = otherTeam_id,
                                gameIndex = .data$gameIndex + 0.1,
                                skill = "Dig", skill_type = "Unknown dig type", skill_subtype = "Unknown dig subtype",
                                evaluation = "Unscouted dig quality", evaluation_code =  "?",
                                attack_code = NA_character_, attack_description = NA_character_,
                                start_zone = .data$end_zone, end_zone = NA_integer_, end_subzone = NA_character_,
                                start_coordinate = .data$end_coordinate, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                                start_coordinate_x = .data$end_coordinate_x, start_coordinate_y = .data$end_coordinate_y,
                                mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                                mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                                player_name = "unassigned", player_id = unassigned_player_id, player_number = NA_integer_,
                                team_touch_id = NA_integer_,
                                phase = "Transition",
                                ballstartstring = .data$ballendstring, ballmidstring = NA_character_, ballendstring = NA_character_,
                                oppositionscore_tmp = .data$teamscore, teamscore = .data$oppositionscore,
                                win_loss = 0)
        plays_tmp_dig <- dplyr::rename(dplyr::select(plays_tmp_dig, -"oppositionscore"), oppositionscore = "oppositionscore_tmp")

        ## Infer freeball attack from otherTeam
        plays_tmp_fb <- dplyr::filter(plays, (.data$skill %in% c("Attack", "Freeball") & .data$team %in% scoutedTeam) & !(lag(.data$skill) %in% c("Reception", "Dig", "Set")) & lag(.data$team) %in% scoutedTeam)
        plays_tmp_fb <- mutate(plays_tmp_fb, team = otherTeam, team_id = otherTeam_id,
                               gameIndex = .data$gameIndex - 0.1,
                               skill = "Freeball",  skill_type = "Unknown freeball type", skill_subtype = "Unknown freeball subtype",
                               evaluation = "Freeball in play", evaluation_code =  "~",
                               attack_code = NA_character_, attack_description = NA_character_,
                               start_zone = NA_integer_, end_zone = NA_integer_, end_subzone = NA_character_,
                               start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                               start_coordinate_x = NA_real_, start_coordinate_y = NA_real_,
                               mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                               mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                               player_name = "unassigned", player_id = unassigned_player_id, player_number = NA_integer_,
                               team_touch_id = NA_integer_,
                               phase = NA_character_,
                               ballstartstring = NA_character_, ballmidstring = NA_character_, ballendstring = NA_character_,
                               oppositionscore_tmp = .data$teamscore, teamscore = .data$oppositionscore,
                               win_loss = 0)
        plays_tmp_fb <- dplyr::rename(dplyr::select(plays_tmp_fb, -"oppositionscore"), oppositionscore = "oppositionscore_tmp")

        ## Infer freeball digs for scoutedTeam
        plays_tmp_fb_dig <- dplyr::filter(plays, .data$skill %in% c("Attack", "Freeball") & .data$team %in% scoutedTeam & !(lag(.data$skill) %in% c("Reception", "Dig", "Set")) & lag(.data$team) %in% scoutedTeam)
        plays_tmp_fb_dig <- mutate(plays_tmp_fb_dig, team = scoutedTeam, team_id = scoutedTeam_id,
                                   gameIndex = .data$gameIndex - 0.05,
                                   skill = "Dig", skill_type = "Freeball dig", skill_subtype = "Unknown dig subtype",
                                   evaluation = "Unscouted dig quality", evaluation_code =  "?",
                                   attack_code = NA_character_, attack_description = NA_character_,
                                   start_zone = NA_integer_, end_zone = NA_integer_, end_subzone = NA_character_,
                                   start_coordinate = NA_integer_, mid_coordinate = NA_integer_, end_coordinate = NA_integer_,
                                   start_coordinate_x = NA_real_, start_coordinate_y = NA_real_,
                                   mid_coordinate_x = NA_real_, end_coordinate_x = NA_real_,
                                   mid_coordinate_y = NA_real_, end_coordinate_y = NA_real_,
                                   player_name = "unassigned", player_id = unassigned_player_id_scouted, player_number = NA_integer_,
                                   team_touch_id = NA_integer_,
                                   phase = "Transition",
                                   ballstartstring = NA_character_, ballmidstring = NA_character_, ballendstring = NA_character_,
                                   oppositionscore = .data$oppositionscore, teamscore = .data$teamscore,
                                   win_loss = 0)
 
        ## Bind everything together
        plays_try <- bind_rows(plays_try, bind_rows(plays_tmp_serve, plays_tmp_reception, plays_tmp_attack, plays_tmp_dig, plays_tmp_fb, plays_tmp_fb_dig))
    }

    plays_try <- dplyr::arrange(bind_rows(plays, plays_try), .data$gameIndex)
    plays_try <- mutate(plays_try,
                        phase = case_when(!lag(.data$skill) %in% c("Reception") & .data$skill %in% c("Attack", "Freeball") ~ "Transition",
                                          lag(.data$skill) %in% c("Reception") & .data$skill %in% c("Attack", "Freeball") ~ "Reception"),
                        lag_team = lag(.data$team, default = dplyr::first(.data$team)),
                        new_value = .data$lag_team != .data$team,
                        num_new_value = case_when(is.na(.data$new_value) | .data$new_value | .data$skill == "Serve" ~ 1,
                                                  TRUE ~ 0))
    plays_try <- mutate(group_by_at(plays_try, "team"), team_touch_id = cumsum(.data$num_new_value))
    x$plays <- ungroup(plays_try)
    x
}

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
##     "Middle-hitter attacks without setting zone to PR" = list(conditions = tibble(skill = "Attack", player_role = "middle"),
##                                                   values = tibble(attack_code = "PR", attack_description = "Attack on opponent overpass", skill_type = "Other attack")),
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

## internal functions needed for pv_tas_live_recode

infer_playing_positions_by_rotation <- function(x, rotation, method = "standard", team = "both") {
    assert_that(is.string(rotation))
    rotation <- match.arg(toupper(rotation), c("SHM", "SMH"))
    assert_that(is.string(method))
    method <- match.arg(tolower(method), c("standard"))
    assert_that(is.string(team))
    team <- match.arg(tolower(team), c("both", "home", "visiting"))

    if (team %in% c("both", "home")) {
        hrx <- infer_roles_by_rotation(x, target_team = home_team(x), rotation = rotation)
        hrx$point_id <- x$point_id
        hrx$match_id <- x$match_id
        hrx <- distinct(hrx)
        hrx <- tidyr::gather(hrx, key = "position", value = "role", setdiff(names(hrx), c("point_id", "match_id")))
        hrx$position <- sub("player_role", "", hrx$position, fixed = TRUE)
        hrx$team <- home_team(x)
        hrid <- tidyr::gather(distinct(x[, c("match_id", "point_id", paste0("home_player_id", 1:6))]), key = "position", value = "player_id", paste0("home_player_id", 1:6))
        hrid$position <- sub("home_player_id", "", hrid$position, fixed = TRUE)
        hrx <- left_join(hrx, hrid, by = c("match_id", "point_id", "position"))
        hrx <- hrx %>% left_join(x %>% group_by_at(c("match_id", "point_id")) %>% slice(1L) %>% ungroup %>% dplyr::mutate(p1_reception = .data$visiting_team %eq% .data$serving_team & .data$home_setter_position %eq% 1) %>% dplyr::select_at(c("match_id", "point_id", "p1_reception")), by = c("match_id", "point_id"))
    } else {
        hrx <- NULL
    }
    if (team %in% c("both", "visiting")) {
        vrx <- infer_roles_by_rotation(x, target_team = visiting_team(x), rotation = rotation)
        vrx$point_id <- x$point_id
        vrx$match_id <- x$match_id
        vrx <- distinct(vrx)
        vrx <- tidyr::gather(vrx, key = "position", value = "role", setdiff(names(vrx), c("point_id", "match_id")))
        vrx$position <- sub("player_role", "", vrx$position, fixed = TRUE)
        vrx$team <- visiting_team(x)
        vrid <- tidyr::gather(distinct(x[, c("match_id", "point_id", paste0("visiting_player_id", 1:6))]), key = "position", value = "player_id", paste0("visiting_player_id", 1:6))
        vrid$position <- sub("visiting_player_id", "", vrid$position, fixed = TRUE)
        vrx <- left_join(vrx, vrid, by = c("match_id", "point_id", "position"))
        vrx <- vrx %>% left_join(x %>% group_by_at(c("match_id", "point_id")) %>% slice(1L) %>% ungroup %>% dplyr::mutate(p1_reception = .data$home_team %eq% .data$serving_team & .data$visiting_setter_position %eq% 1) %>% dplyr::select_at(c("match_id", "point_id", "p1_reception")), by = c("match_id", "point_id"))
    } else {
        vrx <- NULL
    }
    ## TODO check for duplicate rows or other failures
    rx <- bind_rows(hrx, vrx)
    switch(method,
           "standard" = mutate(rx, playing_position = case_when(.data$role %eq% "setter" & .data$position %in% c(2:4) ~ 2L,
                                                                .data$role %eq% "outside" & .data$position %in% c(2:4) & !p1_reception ~ 4L,
                                                                .data$role %eq% "outside" & .data$position %in% c(2:4) & p1_reception ~ 2L,
                                                                .data$role %eq% "opposite" & .data$position %in% c(2:4) & !p1_reception ~ 2L,
                                                                .data$role %eq% "opposite" & .data$position %in% c(2:4) & p1_reception ~ 4L,
                                                                .data$role %eq% "middle" & .data$position %in% c(2:4) ~ 3L,
                                                                .data$role %eq% "setter" & .data$position %in% c(1, 6, 5) ~ 1L,
                                                                .data$role %eq% "outside" & .data$position %in% c(1, 6, 5) ~ 6L,
                                                                .data$role %eq% "opposite" & .data$position %in% c(1, 6, 5) ~ 1L,
                                                                .data$role %in% c("middle", "libero") & .data$position %in% c(1, 6, 5) ~ 5L)),
           stop("unexpected method: ", method))
}

infer_roles_by_rotation <- function(x, target_team, rotation) {
    is_target_team <- if (is.character(target_team)) function(z) z %eq% target_team else target_team
    assert_that(is.function(is_target_team))
    assert_that(is.string(rotation))
    rotation <- match.arg(toupper(rotation), c("SHM", "SMH"))
    rx <- matrix(NA_character_, nrow = nrow(x), ncol = 6)
    ttidx <- is_target_team(x$home_team) & !is.na(x$home_setter_position)
    for (sp in 1:6) {
        ridx <- ttidx & x$home_setter_position %eq% sp
        if (identical(rotation, "SHM")) {
            rx[ridx, sp] <- "setter"
            rx[ridx, rot_forward(sp, 1L)] <- "outside"
            rx[ridx, rot_forward(sp, 2L)] <- "middle"
            rx[ridx, rot_forward(sp, 3L)] <- "opposite"
            rx[ridx, rot_forward(sp, 4L)] <- "outside"
            rx[ridx, rot_forward(sp, 5L)] <- "middle"
            ## except libero instead of middle in back court, but not when serving; but we don't care because this is just blockers
        } else if (identical(rotation, "SMH")) {
            rx[ridx, sp] <- "setter"
            rx[ridx, rot_forward(sp, 1L)] <- "middle"
            rx[ridx, rot_forward(sp, 2L)] <- "outside"
            rx[ridx, rot_forward(sp, 3L)] <- "opposite"
            rx[ridx, rot_forward(sp, 4L)] <- "middle"
            rx[ridx, rot_forward(sp, 5L)] <- "outside"
        } else {
            stop("expecting rotation to be 'SMH' or 'SHM'")
        }
    }
    ttidx <- is_target_team(x$visiting_team) & !is.na(x$visiting_setter_position)
    for (sp in 1:6) {
        ridx <- ttidx & x$visiting_setter_position %eq% sp
        if (identical(rotation, "SHM")) {
            rx[ridx, sp] <- "setter"
            rx[ridx, rot_forward(sp, 1L)] <- "outside"
            rx[ridx, rot_forward(sp, 2L)] <- "middle"
            rx[ridx, rot_forward(sp, 3L)] <- "opposite"
            rx[ridx, rot_forward(sp, 4L)] <- "outside"
            rx[ridx, rot_forward(sp, 5L)] <- "middle"
        } else if (identical(rotation, "SMH")) {
            rx[ridx, sp] <- "setter"
            rx[ridx, rot_forward(sp, 1L)] <- "middle"
            rx[ridx, rot_forward(sp, 2L)] <- "outside"
            rx[ridx, rot_forward(sp, 3L)] <- "opposite"
            rx[ridx, rot_forward(sp, 4L)] <- "middle"
            rx[ridx, rot_forward(sp, 5L)] <- "outside"
        } else {
            stop("expecting rotation to be 'SMH' or 'SHM'")
        }
    }
    setNames(as.data.frame(rx, stringsAsFactors = FALSE), paste0("player_role", 1:6))
}

