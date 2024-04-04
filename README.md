# March Madness Predictor

The idea behind this repository is to matchup play types throughout NCAA basketball games with an emphasis on time. The response is weather a playtype matchup will result in a scoring play. I am using an XGBoost model to randomly simulate a matchup between two teams. The team with the higher probability of making scoring plays is who should be selectedt to win the game. I belive this will capture a team's stamina and how they will react to the usual play types of another team at different points in the game.

I have a loose weighting for the seeds of the teams that I plan to tune over the next year. Before that, evident through the "Model_Tuning.R" file, I am to find the best tuning parameters for the XGBoost model. These will be selected through an iterative process selecting the team that gives somewha solid rankings of upset likeliness for the first round of the March Madness Tournament.

I will be working on this on and off as March Madness 2025 comes closer.
