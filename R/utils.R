#' Generate more meaningful attack_code values
#'
#' Notes:
#' \itemize{
#'   \item "setting zones" (terminology from the VBStats app) are called "attack codes" here for consistency with the datavolley package
#'   \item the rules are applied in order, so that an attack in \code{x} that matches multiple rows in \code{remap} will have the first matching row applied to it
#' }
#'
#' @param x data.frame or tibble: a peranavolley object as returned by \code{pv_read}, or the plays component thereof
#' @param remap data.frame or tibble: this data.frame must contain the column \code{new_attack_code}, and one or more columns that define the conditions to match on. See the example below for more detail
#'
#' @return A character vector with new \code{attack_code} values
#'
#' @examples
#'
#' ## read file
#' x <- pv_read(pv_example_file())
#'
#' ## construct the remapping
#' remap <- data.frame(attack_code = c("1", NA_character_, "4", "5"),
#'                     start_zone = c(NA, 3, 2, 2),
#'                     new_attack_code = c("X1", "Z3", "X6", "V6"),
#'                     stringsAsFactors = FALSE)
#' ## meaning that: any attack with attack_code 1 will be recoded as an "X1" attack
#' ##               any attack from start_zone 3 will be recoded as an "Z3" attack
#' ##               any attack from start_zone 2 with attack_code 4 will be recoded as an "X6" attack
#' ##               any attack from start_zone 2 with attack_code 5 will be recoded as an "V6" attack
#'
#' x$plays$attack_code <- pv_recode_attacks(x, remap = remap)
#'
#' @export
pv_recode_attacks <- function(x, apply_to_skills = "Attack", remap) {
    if (is.list(x) && !is.data.frame(x)) {
        if ("plays" %in% names(x)) {
            x <- x$plays
        } else {
            stop("x must be a list with 'plays' component, or a data.frame")
        }
    }
    if (!is.data.frame(x)) {
        stop("x must be a list with 'plays' component, or a data.frame")
    }
    if (!all(c("skill") %in% names(x))) stop("x must contain at least the column 'skill' - is it a peranavolley object or the plays component of one?")
    assert_that(is.data.frame(remap))
    if (!all(c("new_attack_code") %in% names(remap)) || ncol(remap) < 2) {
        stop("remap must contain the column 'new_attack_code' plus at least one condition column")
    }
    ## in case remap was constructed using data.frame and stringsAsFactors = TRUE
    for (ci in seq_len(ncol(remap))) {
        if (is.factor(remap[[ci]])) remap[[ci]] <- as.character(remap[[ci]])
    }
    assert_that(is.character(remap$new_attack_code))
    new_attack_code <- x$attack_code
    ## now go through each row in remap and apply the rules
    have_warned <- FALSE
    have_modified <- rep(FALSE, nrow(x))
    cond_cols <- setdiff(names(remap), "new_attack_code")
    if (length(cond_cols) < 1) stop("no conditions in remap") ## should already have been caught by the check above
    skill_matched <- x$skill %in% apply_to_skills
    for (ri in seq_len(nrow(remap))) {
        if (all(is.na(remap[ri, cond_cols])) && !is.na(remap$new_attack_code[ri])) {
            warning("remap has at least one row with all NA values: this will match all rows in x that have a skill in apply_to_skills. Did you mean this?")
            have_warned <- TRUE
        }
        idx <- lapply(cond_cols, function(cond) (!is.na(remap[[cond]][ri]) & x[[cond]] %eq% remap[[cond]][ri]) | is.na(remap[[cond]][ri]))
        if (ri < 2) {
            ## check on first iteration that all cols in remap are present in x
            if (any(vapply(idx, length, FUN.VALUE = 0) < 1)) stop("x is missing at least one column that is present in remap")
        }
        idx <- do.call(cbind, idx)
        idx <- apply(idx, 1, all) & skill_matched & !have_modified
        have_modified <- have_modified | idx
        new_attack_code[idx] <- remap$new_attack_code[ri]
    }
    new_attack_code
}


## unexported
ex_attack_code_remap <- function(which) {
    ## a remap table that does the following:
    ## - attacks marked as "Setter tip" ("Dump" in VBStats) by a setter are mapped to "PP"
    ## - any other use of "Setter tip" ("Dump" in VBStats) are mapped to NA, because these will be treated as freeballs over. Note that this requires an extra step that changes the skill in these rows to "Freeball"
    ## - attack_code 1 to "X1"
    ## - attack_code 2 to "X2"
    ## - attack_code 3 to "X7"
    ## - attack_code 4 to "CF" (slide)
    ## - attack code 5 treated as highball, with new attack code determined by start_zone (V5, V6, etc)
    ## - any other attack (with NA attack code) treated as standard tempo with new attack code determined by start_zone (X5, X6, etc)
    tribble(~attack_code, ~start_zone, ~player_role, ~skill_subtype, ~new_attack_code,
            NA, NA, "setter", "Setter tip", "PP",
            NA, NA, NA_character_, "Setter tip", NA_character_,
            1, NA, NA_character_, NA_character_, "X1",
            2, NA, NA_character_, NA_character_, "X2",
            3, NA, NA_character_, NA_character_, "X7",
            4, NA, NA_character_, NA_character_, "CF",
            5, 3, NA_character_, NA_character_, "V3",
            5, 4, NA_character_, NA_character_, "V5",
            5, 2, NA_character_, NA_character_, "V6",
            5, 7, NA_character_, NA_character_, "V0",
            5, 5, NA_character_, NA_character_, "V0",
            5, 8, NA_character_, NA_character_, "VP",
            5, 6, NA_character_, NA_character_, "VP",
            5, 9, NA_character_, NA_character_, "V8",
            5, 1, NA_character_, NA_character_, "V8",
            5, NA, NA_character_, NA_character_, NA_character_, ## unknown
            NA, 4, NA_character_, NA_character_, "X5",
            NA, 2, NA_character_, NA_character_, "X6",
            NA, 7, NA_character_, NA_character_, "X0",
            NA, 5, NA_character_, NA_character_, "X0",
            NA, 8, NA_character_, NA_character_, "XP",
            NA, 6, NA_character_, NA_character_, "XP",
            NA, 9, NA_character_, NA_character_, "X8",
            NA, 1, NA_character_, NA_character_, "X8",
            NA, NA, NA_character_, NA_character_, NA_character_)
}

##xp <- x$plays
##temp <- t(as.matrix(xp[, paste0("home_player_id", 1:6)])) ## 6 x n
##xp$home_setter_id <- temp[xp$home_setter_position + (seq_len(nrow(xp))-1)*6]
##temp <- t(as.matrix(xp[, paste0("visiting_player_id", 1:6)])) ## 6 x n
##xp$visiting_setter_id <- temp[xp$visiting_setter_position + (seq_len(nrow(xp))-1)*6]
##xp <- mutate(xp, player_role = case_when(team %eq% home_team & player_id %eq% home_setter_id ~ "setter",
##                                         team %eq% visiting_team & player_id %eq% visiting_setter_id ~ "setter"))
##
##nac <- pv_recode_attacks(xp, remap = peranavolley:::ex_attack_code_remap())
####idx <- xp$skill %eq% "Attack" & tolower(x$skill_subtype) %eq% "setter tip" & is.na(xp$attack_code)
#### x$skill[idx] <- "Freeball"

