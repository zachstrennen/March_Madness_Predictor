# Just a giant, iterative way to find the ideal tuning parameters to predict upsets
# All based on the upsets of the 2024 season
for (i in 2:5) {
  print("depth: ")
  print(i)
  max_depth <- i
  for (j in 1:6) {
    print("eta: ")
    print(j / 10)
    eta <- j / 10
    for (k in 0:2) {
      gamma <- k
      for (l in 5:8) {
        subsample <- l / 10
        for (m in 5:8) {
          colsample <- m / 10

          likelihood_duq <- c()
          likelihood_nm <- c()
          likelihood_nc <- c()
          likelihood_oreg <- c()
          likelihood_yale <- c()
          likelihood_char <- c()
          likelihood_verm <- c()
          likelihood_sam <- c()

          for (x in 1:4) {
            # Build and store model results
            home_df <- filter_home(mbb_pbp, "BYU")
            away_df <- filter_away(mbb_pbp, "Duquesne")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_duq <- c(likelihood_duq, likelihood)

            home_df <- filter_home(mbb_pbp, "Clemson")
            away_df <- filter_away(mbb_pbp, "New Mexico")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_nm <- c(likelihood_nm, likelihood)

            home_df <- filter_home(mbb_pbp, "Texas Tech")
            away_df <- filter_away(mbb_pbp, "NC State")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_nc <- c(likelihood_nc, likelihood)

            home_df <- filter_home(mbb_pbp, "South Carolina")
            away_df <- filter_away(mbb_pbp, "Oregon")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_oreg <- c(likelihood_oreg, likelihood)

            home_df <- filter_home(mbb_pbp, "Auburn")
            away_df <- filter_away(mbb_pbp, "Yale")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_yale <- c(likelihood_yale, likelihood)

            home_df <- filter_home(mbb_pbp, "Alabama")
            away_df <- filter_away(mbb_pbp, "Charleston")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_char <- c(likelihood_char, likelihood)

            home_df <- filter_home(mbb_pbp, "Duke")
            away_df <- filter_away(mbb_pbp, "Vermont")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_verm <- c(likelihood_verm, likelihood)

            home_df <- filter_home(mbb_pbp, "Kansas")
            away_df <- filter_away(mbb_pbp, "Samford")
            matchup <- set_up_matchup(home_df, away_df)
            likelihood <- build_model(matchup, 11, 11, max_depth, eta, gamma, subsample, colsample)
            likelihood_sam <- c(likelihood_sam, likelihood)
          }
          mean_duq <- mean(likelihood_duq)
          mean_nm <- mean(likelihood_nm)
          mean_nc <- mean(likelihood_nc)
          mean_oreg <- mean(likelihood_oreg)
          mean_yale <- mean(likelihood_yale)
          mean_char <- mean(likelihood_char)
          mean_verm <- mean(likelihood_verm)
          mean_sam <- mean(likelihood_sam)

          if (mean_nm > mean_duq) {
            if (mean_nc < mean_duq & mean_oreg < mean_duq) {
              if (mean_yale < mean_sam) {
                if (mean_sam < mean_verm & mean_sam < mean_char) {
                  print("FOURTH REQ MET")
                  print(paste(mean_nm, mean_duq, mean_nc, mean_oreg))
                  print(paste(max_depth, eta, gamma, subsample, colsample))
                }
              }
            }
          }
        }
      }
    }
  }
}
