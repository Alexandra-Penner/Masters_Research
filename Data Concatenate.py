# -*- coding: utf-8 -*-
"""
Created on Mon Oct  1 11:39:44 2018

@author: Alli Penner
"""

import pandas as pd

# load then concatenate yearly play data into one frame
play09 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2009.csv")
play10 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2010.csv")
play11 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2011.csv")
play12 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2012.csv")
play13 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2013.csv")
play14 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2014.csv")
play15 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2015.csv")
play16 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2016.csv")
play17 = pd.read_csv("C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_2017.csv")

pl = [play09, play10, play11, play12, play13, play14, play15, play16, play17]

playsfull = pd.concat(pl)
#%%
playsfull.to_csv(path_or_buf= "C:/Users/Nick Penner/Documents/NFLProject/nflscrapR-data/data/season_play_by_play/pbp_Full.csv", 
                 na_rep = NA)


