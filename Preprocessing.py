# -*- coding: utf-8 -*-
"""
Created on Sat Jun  9 17:59:47 2018

@author: Alli Penner
"""

import pandas as pd

# load data
playsfull = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_Full.csv")

# MAXIMIZE SUCCESS RATE 
# series of conditional predictions
# game theory of playcalling strategy
# predict Success rate
# Cumulative Game Stats
# Down and Distance
# play type ***this will segue into playcaller application
#       comparive outcomes for diff play types given other data
# Prior plays percent each type and average results

# generate dataset of features to analyze

# start with empty frame to populate
playdata = []
label = []
colnames = ["down", "distance", 'rr', 'rm', 'rl', 'psr', 'psm', 'psl', 'pir', 'pim', 'pil', 'pdr', 'pdm', 'pdl']

for i in range(0,len(playsfull)):
    down = playsfull.iloc[i][5]
    dist = playsfull.iloc[i][13]
    gain = playsfull.iloc[i][21]
    
    # calc succ rate to predict
    if down == 1:
        if gain >= (dist*0.5):
            label.append(1)
        elif gain < (dist*0.5):
            label.append(0)
        else:
            label.append(float('NaN'))
    elif down == 2:
        if gain >= (dist*0.7):
            label.append(1)
        elif gain < (dist*0.7):
            label.append(0)
        else:
            label.append(float('NaN'))
    elif down == 3 or down == 4:
        if gain >= dist:
            label.append(1)
        elif gain < dist:
            label.append(0)
        else:
            label.append(float('NaN'))
    else:
        label.append(float('NaN'))
        
    # generate dataset
    play = []
    play.append(down)
    play.append(dist)
    # Playtype
    # run or pass
    # left, right, depth.
    passplay = playsfull[i][33]
    runplay = playsfull[i][44]
    airyards = playsfull[i][36]
    rundirect = playsfull[i][45]
    passdirect = playsfull[i][39]
    sackyards = playsfull[i][59]
    # leave sacks as NA for depth and direction to impute with PMM
    
    # one hot encode play type
    # rr, rm, rl, psr, psm, psl, pir, pim, pil, pdr, pdm, pdl
    
    if runplay == 1:
        if rundirect == 'right':
            play.extend[1,0,0,0,0,0,0,0,0,0,0,0]
        elif rundirect == 'middle':
            play.extend[0,1,0,0,0,0,0,0,0,0,0,0]
        elif rundirect == 'left':
            play.extend[0,0,1,0,0,0,0,0,0,0,0,0]
    elif passplay == 1:
        if airyards <= 10:
            if passdirect == 'right':
                play.extend[0,0,0,1,0,0,0,0,0,0,0,0]
            elif passdirect == 'middle':
                play.extend[0,0,0,0,1,0,0,0,0,0,0,0]
            elif passdirect == 'right':
                play.extend[0,0,0,0,0,1,0,0,0,0,0,0]
        elif airyards > 10 and airyards <= 20:
            if passdirect == 'right':
                play.extend[0,0,0,0,0,0,1,0,0,0,0,0]
            elif passdirect == 'middle':
                play.extend[0,0,0,0,0,0,0,1,0,0,0,0]
            elif passdirect == 'right':
                play.extend[0,0,0,0,0,0,0,0,1,0,0,0]
        elif airyards > 20:
            if passdirect == 'right':
                play.extend[0,0,0,0,0,0,0,0,0,1,0,0]
            elif passdirect == 'middle':
                play.extend[0,0,0,0,0,0,0,0,0,0,1,0]
            elif passdirect == 'right':
                play.extend[0,0,0,0,0,0,0,0,0,0,0,1]
    else:
        play.extend['nan','nan','nan','nan','nan','nan','nan','nan','nan','nan','nan','nan']
        
        
        playdata.append[play]
    
                
        
    

        
    
    
    
    
    