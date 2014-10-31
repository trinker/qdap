context("Checking colSplit")

test_that("dispersion_plot outputs a ggplot object",{

    m <- dispersion_plot(raj$dialogue, c(" love ", "love", " night ", "night"), plot=FALSE)
    expect_true(all(class(m) == c("gg", "ggplot")))

})

