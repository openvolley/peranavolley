context("read")
test_that("pv_read function works ok", {
    fn <- pv_example_file()
    xp <- pv_read(fn)

    check <- xp$plays %>% dplyr::filter(skill %eq% "Serve") %>% mutate(p1 = case_when(team %eq% home_team ~ home_player_id1, TRUE ~ visiting_player_id1)) %>% mutate(chk = p1 %eq% player_id) %>% dplyr::select(p1, player_id, chk)
    expect_true(all(check$chk))
})
