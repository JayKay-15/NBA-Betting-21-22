# NBA-Betting-21-22

Hey y'all thanks for checking out my work. I've been working on this NBA betting model for about a year. I ran a simplier version of this process last year and had some success before having to shut it down.

For the 2021-22 NBA season I've added more predcitive models, fixed minor errors in the data cleaning and stat calculation code, and converted my results tracking to R.

The basic process and overview of this model goes as follows:

NBA stats are scraped using the NBAstatR library, the stats then go through a cleaning process where advanced stats are calculated, stats are adjusted for opponent and finally weighted toward recent performance.

Once the stats are cleaned, six different models are run to make predicitions for the margin of victory, scores, and win probability. Once the predctions are made they are printed to an Excel spreadsheet where I have a game summary, predictions, and comparison of stats for each game for that day.

Finally, the last step is analyzing the models performance. To do this, I look at the models previous selections to determine 'keys' that I use to determine if the models prediction is worth betting on.

Again, thanks to anyone checking out this work, you can find my contact information on my github page if you have any questions or suggestions. Best of luck to anyone else taking on a similar challenge.

## Stat Calculations

-	FG % (FGM/FGA)
-	SR2 ((FGA-3PA)/FGA)
-	FG3 % (3PM/3PA)
-	SR3 (FGA3/FGA)
-	FT % (FTM/FTA)
-	FTR (FTM/FGA)
-	ORB % (ORB/(ORB + oDRB))
-	DRB % (DRB/(DRB + oORB))
-	TRB % (TRB/(TRB + oTRB))
-	AST % (AST/FGM)
-	TO % or (TOV/Poss)
-	STL % (STL/oPoss)
-	BLK % (BLK/(oFGA-o3PA))
-	PF % (PF/oPoss)
-	eFG % (FG + 0.5 * 3PM) / FGA))
-	TS % (PTS/ (2 * FGA + 0.44 * FTA) )
-	ORtg (PTS/Poss) * 100
-	DRtg (oPTS/oPoss) * 100
-	Poss (FGA - ORB + TOV + (0.44 * FTA) 
-	Pace (48 * ((Poss + oPoss) / (2 * (Team Min / 5)))
