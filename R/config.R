#' Define eventgrade interpretations
#'
#' Define the interpretation of eventgrades associated with events.
#'
#' @return a tibble with columns "skill", "eventgrade", "evaluation_code" (the equivalent DataVolley code, if there is one), "evaluation", and "win_loss"
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#' pv_default_eventgrades()
#'
#' @export
pv_default_eventgrades <- function() {
    tribble(~skill, ~eventgrade, ~evaluation_code, ~evaluation, ~win_loss,
            "Serve", 0L, "=", "Error", -1L,
            "Serve", 1L, "-", "Negative, opponent free attack", 0L, ## opp perfect pass
            "Serve", 2L, "!", "OK, no first tempo possible", 0L,
            "Serve", 3L, "#", "Ace", 1L,
            "Serve", 4L, "+", "Positive, opponent some attack", 0L,
            "Pass", 0L, "=", "Error", -1L,
            "Pass", 1L, "-", "Negative, limited attack", 0L,
            "Pass", 2L, "+", "Positive, attack", 0L,
            "Pass", 3L, "#", "Perfect pass", 0L,
            "Spike", 0L, "=", "Error", -1L,
            "Spike", 1L, NA_character_, "Spike in play", 0L,
            "Spike", 3L, "#", "Winning attack", 1L,
            "Block", 0L, "=", "Error", -1L, ## net touch or invasion
            "Block", 1L, "+", "Positive, block touch", 0L,
            "Block", 2L, "#", "Winning block", 1L, ## solo block
            "Block", 3L, "#", "Winning block", 1L, ## multiplayer block
            "Defense", 0L, "=", "Error", -1L,
            ##"Defense", /, "Ball directly back over net", 0L,
            "Defense", 1L, NA_character_, "Dig in play", 0L, ## to check
            ##"Defense", 2L, "Good dig", 0L,
            ##"Defense", 3L, "Perfect dig", 0L,
            "Set", 0L, "=", "Error", -1L,
            "Set", 1L, NA_character_, "Set in play", 0L,
            "Set", 2L, NA_character_, "Assist", 0L)##,
            ##"Freeball", 0L, "Error", -1L,
            ##"Freeball", 1L, "Poor", 0L,
            ##"Freeball", 2L, "Good", 0L,
            ##"Freeball", 3L, "Perfect", 0L)
}

#' Define errortype interpretations
#'
#' Define the interpretation of errortypes associated with events.
#'
#' @return a tibble with columns "skill", "errortype", and "evaluation"
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#' pv_default_errortypes()
#'
#' @export
pv_default_errortypes <- function() {
    tribble(~skill, ~errortype, ~evaluation,
            "Spike", 0L, NA_character_, ##"No error", ## should this be error type not recorded?
            "Spike", 1L, "Attack out",
            "Spike", 2L, "Blocked",
            "Spike", 3L, "Attack in net",
            "Spike", 4L, "Net contact")
}

#' Define subevent interpretations
#'
#' Define the interpretation of subevents associated with events.
#'
#' @return a tibble with columns "skill", "subevent", and "evaluation"
#'
#' @seealso \code{\link{pv_read}}
#'
#' @examples
#' pv_default_subevents()
#'
#' @export
pv_default_subevents <- function() {
    tribble(~skill, ~subevent, ~evaluation,
            "Spike", 0L, "Hard spike", ## unspecified, defaults to hard spike
            "Spike", 1L, "Soft spike/topspin",
            "Spike", 2L, "Setter tip",
            "Spike", 3L, "Hard spike",
            "Spike", 4L, "Spike off the block")
}
