context("read")
test_that("pv_read function works ok", {
    fn <- pv_example_file()
    xp <- pv_read(fn)

    check <- xp$plays %>% dplyr::filter(skill %eq% "Serve") %>% mutate(p1 = case_when(team %eq% home_team ~ home_player_id1, TRUE ~ visiting_player_id1)) %>% mutate(chk = p1 %eq% player_id) %>% dplyr::select(p1, player_id, chk)
    expect_true(all(check$chk))
})

test_that("data logic is consistent", {
    xp <- pv_read(pv_example_file())

    single_unique_or <- function(z, or = "[multiple values]") {
        if (is.na(or)) or <- as(NA, class(z))
        tmp <- unique(na.omit(z))
        if (length(tmp) == 1) tmp else if (length(tmp) > 1) or else as(NA, class(z))
    }
    chk <- xp$plays %>% group_by(.data$point_id) %>% mutate(skl = single_unique_or(.data$skill, or = NA)) %>% ungroup %>%
        dplyr::filter(!.data$end_of_set) %>% dplyr::select(.data$set_number, .data$point_id, .data$skl, .data$home_team_score, .data$visiting_team_score, .data$point_won_by, .data$serving_team) %>% distinct %>% dplyr::arrange(.data$point_id)

    chk <- chk %>% mutate(ptinc = case_when(pmax(.data$home_team_score, .data$visiting_team_score) == 1 ~ .data$point_won_by,
                                            lag(.data$set_number) != .data$set_number ~ NA_character_,
                                            .data$skl %in% c("Substitution", "Timeout", "Technical timeout") ~ NA_character_,
                                            lag(.data$home_team_score) < .data$home_team_score ~ home_team(xp),
                                            lag(.data$visiting_team_score) < .data$visiting_team_score ~ visiting_team(xp),
                                            TRUE ~ "wtf"))

    ## point should have incremented for the point_won_by team, except in subs/timeouts
##    expect_equal(nrow(chk %>% dplyr::filter(!skl %in% c("Substitution", "Timeout", "Technical timeout") & !ptinc %eq% point_won_by)), 0L)
    ## for this example file, there are two inconsistencies due to scouting silliness
    expect_equal(nrow(chk %>% dplyr::filter(!skl %in% c("Substitution", "Timeout", "Technical timeout") & !ptinc %eq% point_won_by)), 2L)

    ## serving team should have been the team that won the previous point
##    expect_equal(nrow(chk %>% dplyr::filter(!skl %in% c("Substitution", "Timeout", "Technical timeout")) %>% dplyr::arrange(.data$point_id) %>% dplyr::filter(lag(.data$set_number) %eq% .data$set_number & !lag(.data$point_won_by) %eq% .data$serving_team)), 0L)
    ## again, scouting errors in this example file mean that there are 3 mismatched rows here, no 0
    expect_equal(nrow(chk %>% dplyr::filter(!skl %in% c("Substitution", "Timeout", "Technical timeout")) %>% dplyr::arrange(.data$point_id) %>% dplyr::filter(lag(.data$set_number) %eq% .data$set_number & !lag(.data$point_won_by) %eq% .data$serving_team)), 3L)
})
