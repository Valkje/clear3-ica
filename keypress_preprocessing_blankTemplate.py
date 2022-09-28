#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 18 00:18:33 2021

@author: mindy
"""
# import libraries
import pandas as pd
import glob
import re
import numpy as np
import sys

# file path for location of user data
pathIn = 'insert file path here' #file path to folder containing raw synapse keypress data for users 
# file path for location of output files
pathOut = 'insert file path here' #file path to folder where you want processed keypress data to go

#%% FUNCTIONS

# sort files by numerical order
numbers = re.compile(r'(\d+)')
def numericalSort(value):
    parts = numbers.split(value)
    parts[1::2] = map(int, parts[1::2])
    return(parts)

# convert unix timestamp to UTC
def convertTimestamp(dataframe):

    # convert to UTC and make timezone naive object
    sessionUTC = pd.to_datetime(dataframe['session_timestamp'], unit='ms', 
                                utc=True).dt.tz_localize(None) 
    keypressUTC = pd.to_datetime(dataframe['keypress_timestamp'], unit='s', 
                                 utc=True).dt.tz_localize(None) 
    # delete rows with no timezone
    df2 = dataframe.dropna(subset = ['timezone'], inplace=False)
    # calculate timezone
    uTZ = df2.timezone / 100 
    timeDelta = pd.to_timedelta(uTZ, unit = 'hours')
    # convert to local time
    sessLocalTime = sessionUTC + timeDelta
    kpLocalTime = keypressUTC + timeDelta
    dataframe['sessionTimestampLocal'] = pd.DataFrame(sessLocalTime)
    dataframe['keypressTimestampLocal'] = pd.DataFrame(kpLocalTime)
    return(dataframe)  

# check chronological order of dataframe
def checkChronOrder(dataframe):
    unordered = []
    row = 0
    Kts = dataframe.keypressTimestampLocal
    print(str('checking chron order for user: ') + dataframe.userID.unique())
    for i, j in zip(Kts, Kts.shift(1)):
        row = row
        if i < j:
            unordered.append((str(dataframe.userID.unique()), row, i, j))
        row = row + 1
    df = pd.DataFrame(unordered, columns = ['userID', 'row', 
                                                 'currentTimestamp', 'previousTimestamp'])
    if unordered != []:
        print(df)
    else:
        print('ordered')
    return(unordered)

# assign numbers to record IDs (sessions)
def assignSessionNumber(dataframe):
#    # dataframe needs to be sorted by keypress timestamp first
#    dataframe.sort_values(by=['keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
    userList = list(dataframe.recordId.unique())
    numberList = list(range(1, len(userList)+1))
    d = dict(zip(userList, numberList))
    dataframe['sessionNumber'] = dataframe['recordId'].map(d)
    return(dataframe)
    
# check order of sessions in dataframe
def checkSessionOrder(dataframe):
    unordered = []
    row = 0
    sessNo = dataframe.sessionNumber
    print(str('checking session order for user ') + dataframe.userID.unique())
    for i, j in zip(sessNo, sessNo.shift(1)):
        row = row
        if i < j:
            unordered.append((str(dataframe.userID.unique()), row, i, j))
        row = row + 1
    df = pd.DataFrame(unordered, columns = ['userID', 'row', 
                                                 'currentSession', 'previousSession'])
    if unordered != []:
        print(df)
    else:
        print('ordered')
    return(unordered) 
   
# simplify diagnosis groups
def makeDiagnosisGroups(dataframe):
    diagnosis = dataframe.diagnostic # can replace with 'what diagnosis is (ie bipolar disorder)'
    # watch out for when diagnostic == 'nan' or '' if replace all not HC with 'BD'
    dataframe['diagnosisGroup'] = np.where(dataframe.diagnostic == 
             'I have never been diagnosed with bipolar disorder', 'HC', diagnosis)
    return(dataframe)

# make previous key column
def previousKey(dataframe):
    l = []
    # group by session number
    dfSess = dataframe.groupby(['sessionNumber'])
    for sess, group in dfSess:
        # dataframe needs to be sorted by keypress timestamp
        group.sort_values(by=['keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
        prevKey = group.keypress_type.shift(1) # shifts down one
        for i in prevKey:
            l.append(i)
    dataframe['previousKeyType'] = l
    return(dataframe)

# calculate interkey delays (IKD)
def calculateIKD(dataframe):
    # dictionaries
    dictInner = {}    
    dictOuter = {}
#    # dataframe needs to be sorted by keypress timestamp first
#    dataframe.sort_values(by=['keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
    # group by session number
    dfBySession = dataframe.groupby(['sessionNumber'])
    # calculate IKD for each session
    for session, group in dfBySession:
        for t, tPrev in zip(group.keypressTimestampLocal, group.keypressTimestampLocal.shift(1)):
            # subtract keypress timestamp from previous keypress timestamp (using shifted column)
            diff = (t-tPrev)
            # link IKD to timestamp
            dictInner[t] = diff
        # link timestamp:IKD to session #
        dictOuter[session] = dictInner
    # map nested dictionary to dataframe
    ikd = dataframe.apply(lambda x: dictOuter[x['sessionNumber']][x['keypressTimestampLocal']], axis=1)
    # quit program if there are no IKDs
    if len(ikd) < 2:
        sys.exit('user ' + str(dataframe.userID.unique()) + ' has no data.')
    # make new column with ikd converted from time delta to number of seconds
    dataframe['IKD'] = ikd.dt.total_seconds()
#    dataframe2 = dataframe.loc[dataframe['IKD'] > 6, 'IKD'] = pd.NaT
    return(dataframe)

# make previous key column
def IKDcumsum(dataframe):
    l = []
    # group by session number
    dfSess = dataframe.groupby(['sessionNumber'])
    for sess, group in dfSess:
        # dataframe needs to be sorted by keypress timestamp
        group.sort_values(by=['keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
        cs = group['IKD'].cumsum()
        for i in cs:
            l.append(i)
    lRound = [round(i, 3) for i in l]
    dataframe['IKDcumsum'] = lRound
    return(dataframe)
    
# calculate days since start of study
def dayNumber(dataframe):
    # add column of dates to dataframe    
    dataframe['date'] = pd.to_datetime(dataframe['keypressTimestampLocal'], 
             format='%Y-%m-%d %H:%M:%S.%f').dt.date
    # delete rows with no timezone
    df2 = dataframe.dropna(subset = ['timezone'], inplace=False)
    # add column with days since app downloaded
    dayNo = (df2.date - df2.date.min() + pd.to_timedelta(1, unit = 'days')).dt.days
    # make dictionary
    d = dict(zip(df2.date, dayNo))
    # map day number to dataframe
    dataframe['dayNumber'] = dataframe['date'].map(d)
    return(dataframe)    

# calculate number of weeks since start of study
def weekNumber(dataframe):
    # calculate week number from day number
    wk = ((dataframe['dayNumber'] - 1) // 7) + 1
    dataframe['weekNumber'] = pd.DataFrame(wk)
    return(wk)

# calculate backspace rate
def bkspRate(dataframe):
    # needs to be grouped by session/day/week etc
#    autocorrect = (dataframe['keypress_type'].values=='autocorrection').sum()
    char = (dataframe['keypress_type'].values=='alphanum').sum()
    punct = (dataframe['keypress_type'].values=='punctuation').sum()
    other = (dataframe['keypress_type'].values=='other').sum()
    pound = (dataframe['keypress_type'].values=='#').sum()
    at = (dataframe['keypress_type'].values=='@').sum()
    suggest = (dataframe['keypress_type'].values=='suggestion').sum()
    bksp = (dataframe['keypress_type'].values=='backspace').sum()
    total = char + punct + other + pound + at + suggest + bksp #+ autocorrect
    if total != 0:
        rate = bksp / total
    else:
        rate = 0
    return(rate)

# calculate autocorrect rate
def autocorrectRate(dataframe):
    # needs to be grouped by session/day/week etc
    autocorrect = (dataframe['keypress_type'].values=='autocorrection').sum()
    char = (dataframe['keypress_type'].values=='alphanum').sum()
    punct = (dataframe['keypress_type'].values=='punctuation').sum()
    other = (dataframe['keypress_type'].values=='other').sum()
    pound = (dataframe['keypress_type'].values=='#').sum()
    at = (dataframe['keypress_type'].values=='@').sum()
    suggest = (dataframe['keypress_type'].values=='suggestion').sum()
    bksp = (dataframe['keypress_type'].values=='backspace').sum()
    total = autocorrect + char + punct + other + pound + at + suggest + bksp
    if total != 0:
        rate = autocorrect / total
    else: 
        rate = 0
    return(rate)

#%% calculate median IKD, median char IKD, backspace & autocorrect rates, and 95th percentile per session
def metricsPerSession(dataframe):
    # make dictionaries
    bkspDict = {}
    ACdict = {}
    p95Dict = {}
    ikdDict = {}
    ikdCharDict = {}

    # group by session number
    dfBySession = dataframe.groupby(['sessionNumber'])

    for session, group in dfBySession:
        # locate all IKDs within one session
        IKDgroup = group.loc[group.sessionNumber == session]
        # locate all alphanum-alphanum transitions in that session
        IKDcharGroup = IKDgroup.loc[((IKDgroup['keypress_type'] == 'alphanum') & 
                                    (IKDgroup['previousKeyType'] == 'alphanum'))]
        
        # calculate backspace and autocorrect rates
        bksp_rate = bkspRate(IKDgroup)
        autocorr_rate = autocorrectRate(IKDgroup)
        bkspDict[session] = bksp_rate
        ACdict[session] = autocorr_rate
        
        # remove NaN from IKD column for following metrics
        IKDgroup2 = IKDgroup.dropna(subset = ['IKD'], inplace=False)
        IKDcharGroup2 = IKDcharGroup.dropna(subset = ['IKD'], inplace=False)
        
        #filter group to only include IKDs < session cutoff
        session_cutoff = 6
        IKDgroup3 = IKDgroup2[IKDgroup2['IKD'] < session_cutoff]
        IKDcharGroup3 = IKDcharGroup2[IKDcharGroup2['IKD'] < session_cutoff]
        
        # calculate median IKD and 95th percentile for all transitions
        if len(IKDgroup3) >=20:
            medIKD=np.median(IKDgroup3.IKD)
            p95 = np.percentile(IKDgroup3.IKD, 95)

        else:
            medIKD = ''
            p95 = ''
        ikdDict[session] = medIKD
        p95Dict[session] = p95
            
        # calculate median for alphanum-alphanum transitions
        if len(IKDcharGroup3) >=20:
            medCharIKD = np.median(IKDcharGroup3.IKD)
        else:
            medCharIKD = ''
        ikdCharDict[session] = medCharIKD
    
    # map calculations to dataframe
    dataframe['backspaceRate'] = dataframe['sessionNumber'].map(bkspDict)
    dataframe['autocorrectRate'] = dataframe['sessionNumber'].map(ACdict)
    dataframe['percent95IKD'] =dataframe['sessionNumber'].map(p95Dict)
    dataframe['medianIKD'] = dataframe['sessionNumber'].map(ikdDict)
    dataframe['medianCharIKD'] = dataframe['sessionNumber'].map(ikdCharDict)
    
    return(dataframe)

#%% sort all csv files in pathIn 
all_files = sorted(glob.glob(pathIn + "*.csv"), key = numericalSort)

# iterate through all csv files in pathIn
for filename in all_files:
    # read csv into dataframe
    df = pd.read_csv(filename, index_col = False)
    
    # skip blank files
    if len(df) < 2:
        continue
    
    # get ID number from file name (arbitrarily assigned)
    uIdx = re.sub(r'\D+','', filename[-12:])
    df.insert(0, 'userID', uIdx)
    
    # remove scientific notation for keypress timestamp
    df['keypress_timestamp'] = df.keypress_timestamp.apply(lambda x: format(x, 'f'))
    # convert session and keypress timestamps to local time
    convertTimestamp(df)
    
#    # round local time to ms
#    df['keypressTimestampLocal'] = df['keypressTimestampLocal'].dt.round('3ms')

    # sort to chronological order based on keypress timestamp
    df.sort_values(by=['keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
    
#####drop duplicate timestamps from the simultaneous typing across multiple apps
#    df.drop_duplicates(subset = ['keypress_timestamp'], keep='first', ignore_index = True, inplace = True)
    df.drop_duplicates(subset = ['keypressTimestampLocal'], keep='first', ignore_index = True, inplace = True)

    # ensure keypress timestamp in local time is in datetime format for future functions
    df['keypressTimestampLocal'] = pd.to_datetime(df.keypressTimestampLocal)
#    # check chronological order (prints userID, row #, value, & previous value if unordered)
#    checkChronOrder(df)

    # assign session numbers
    assignSessionNumber(df)
#    # check session number order (prints userID, row #, value, & previous value if unordered)
#    checkSessionOrder(df)
    
    # other data processing
    makeDiagnosisGroups(df)
    dayNumber(df)
    weekNumber(df)

    # sort based on session
    df.sort_values(by=['sessionNumber','keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
    previousKey(df)

#    df.sort_values(by=['keypress_timestamp'], ascending=True, inplace=True, ignore_index=True)
    calculateIKD(df)
    
#%%    
    # remove IKDs > 6 sec (session cutoff)
    df.loc[df['IKD'] > 6, 'IKD'] = pd.NaT
    
    # subtract 6 sec to remove session cutoff from session duration
    df['sessionDuration_minus6sec'] = df['session_duration'] - 6

    # some data calculations: backspace & autocorrect rates, median IKD, median character IKD, 95th percentile
    metricsPerSession(df)
    
    # phone info
    df[['phoneType', 'iOSversion']] = df['phoneInfo'].str.split(';', n=1, expand=True)
    df[['appVersion', 'phoneBuild']] = df['appVersion'].str.split(';', expand=True)
    
    # reorder dataframe columns
    df = df[['healthCode', 'recordId', 'userID', 'age', 'gender', 'diagnosisGroup', 'phoneType',
             'iOSversion', 'appVersion', 'phoneBuild', 'sessionTimestampLocal', 
             'sessionDuration_minus6sec', 'date', 'dayNumber', 'weekNumber','sessionNumber', 
             'keypressTimestampLocal', #'KPtimeFix', 
             'keypress_type', 'previousKeyType', 
             'IKD', #'IKDcumsum', 
             'backspaceRate', 'autocorrectRate', 'medianIKD', 'medianCharIKD',
             'percent95IKD', 'duration', 'distanceFromCenter', 'distanceFromPrevious', 'session_duration',
             'session_timestamp', 'keypress_timestamp', 'timezone', 'diagnostic']]
        
    # make new csv file for each user
    df.to_csv(pathOut + 'User_' + str(uIdx) + '_keypressDataMetrics.csv', index = False)  
    
    print('Finished user ' + str(uIdx)) 
    
print('Finish')


