source(knitr::purl(file.path('..', 'lab.Rmd'), 
                   output = file.path('..', 'lab.R')), 
       chdir = TRUE)

library(testthat)
testthat::local_edition(3)

test_that('1.1. Variables of interest', {
    expect_contains(vars_of_interest, 
                    c('id', 
                      'year', 
                      'polviews', 
                      'consci', 
                      'coneduc', 
                      'conlegis'))
})
test_that('1.9. Analysis dataframe', {
    expect_equal(nrow(dataf), 64195L)
    expect_contains(names(dataf), 
                    c('id', 'year', 'polviews', 
                      'consci', 'coneduc', 'conlegis'))
    expect_s3_class(dataf$polviews, 'factor')
    expect_s3_class(dataf$consci, 'factor')
    expect_equal(sum(is.na(dataf$consci)), 22110)
})

test_that('2. Calculating rates', {
    expect_equal(prob2.4, 0.4348105025543542723199)
    expect_equal(get_rate(dataf$consci), 0.4348105025543542723199)
    expect_equal(get_rate(dataf$consci, na.rm = FALSE), NA_real_)
})


test_that('3. Split-apply-combine', {
    expect_equal(nrow(rate_df), 90L)
    expect_contains(names(rate_df), 
                    c('year', 'polviews', 'consci', 'coneduc', 'conlegis'))
    expect_equal(mean(rate_df$coneduc), 0.28741053265379185033)
    expect_null(attr(rate_df, 'groups'))
})


test_that('4. Reshaping for ggplot', {
    rate_long_df_fltd = rate_long_df |> 
        filter(institution %in% c('education', 'Congress', 'science'))
    expect_equal(nrow(rate_long_df_fltd), 270)
    expect_named(rate_long_df, 
                 c('year', 'polviews', 'institution', 'rate'), 
                 ignore.order = TRUE)
    expect_equal(mean(rate_long_df_fltd$rate), 0.28238216678798866432)
})

test_that('5. Plotting with facets', {
    warning('This problem is not automatically checked')
})

test_that('6. Extension', {
    warning('This problem is not automatically checked')
})
