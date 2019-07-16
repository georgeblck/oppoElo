Retrospective strength of schedule in the NBA (shiny app)
================

During the regular season a common measure of a teams remaining strength
of schedule (SOS) is the combined winning percentage of those opponents.
[link](http://www.tankathon.com/remaining_schedule_strength). A high
winning percentage means the opponents you still have to face are
probably tough and vice-versa.

There are two ideas I want to apply in this work that are in a similar
vein:

1.  Substitute the winning percentage with a more adequate measure of
    performance: the Elo score.
2.  Instead of a *remaining* SOS, calculate a *retrospective* SOS.

The first addition is self explanatory and through 538s work very easy
to implement. The latter idea

## Data

  - Data since season 1977 (NBA/ABA Merger)

## Results

The variation in Opponents Elo is a lot smaller than the general Team
Elo (i.e. the strength of teams and not their opponents). That is to be
expected as the strength of an opponent can only vary so much through a
season.

![](README_files/figure-gfm/sd-1.png)<!-- -->![](README_files/figure-gfm/sd-2.png)<!-- -->

## To-Do

  - [x] Upload shiny to shinyapps.io
  - [x] Add more seasons (by solving conferences )
  - [x] Add carmElo for the recent seasons (via button)
  - [ ] Make the graphic bigger in browser
  - [ ] Look into the max/min delta a team can undergo in a season
  - [x] Visualize the variation in oppoElo and teamElo
  - [x] Make multiple seasons selectable
  - [ ] Replace team names if it’s only name change
  - [ ] How similar are the different Elo scores (logreg and visual)
