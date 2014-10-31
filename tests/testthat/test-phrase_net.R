context("Checking phrase_net")


test_that("phrase_net gives a `phrase_net` and `igraph` object",{

    sents <- c("Questions must be at least 2 days old to be eligible for a bounty.", 
        "There can only be 1 active bounty per question at any given time.", 
        "Users must have at least 75 reputation to offer a bounty, and may only have a maximum of 3 active bounties at any given time.", 
        "The bounty period lasts 7 days.", "Bounties must have a minimum duration of at least 1 day.", 
        "After the bounty ends, there is a grace period of 24 hours to manually award the bounty.", 
        "If you do not award your bounty within 7 days, the highest voted answer created after the bounty started with at least 2 upvotes will be awarded half the bounty amount.", 
        "If there's no answer meeting that criteria, the bounty is not awarded to anyone.", 
        "If the bounty was started by the question owner, and the question owner accepts an answer during the bounty period, and the bounty expires without an explicit award - we assume the bounty owner liked the answer they accepted and award it the full bounty amount at the time of bounty expiration.", 
        "In any case, you will always give up the amount of reputation specified in the bounty, so if you start a bounty, be sure to follow up and award your bounty to the best answer!", 
        "As an additional bonus, bounty awards are immune to the daily reputation cap and community wiki mode."
        )


    out <- phrase_net(sents, r=.5)
    expect_true(inherits(out, "phrase_net"))
    expect_true(inherits(out, "igraph"))

})

