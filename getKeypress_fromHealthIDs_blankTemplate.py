## GET KEYPRESS INFO, DURATION, DIAGNOSIS AND HEALTHCODE ID FROM SYNAPSE  
## INPUT: HEALTHCODE IDS in csv or excel
## Output: one file with keypress data for each user
## MAKE SURE TO ADD YOUR USERNAME AND PASSWORD FOR SYNAPSE
import synapseclient, os, json
import pandas as pd
import datetime

#%% Add in your email and password:
username_synapse = 'Loran' 
password_synapse = r'password'

#%% Csv file with data (read all IDs) and provide 1 output per Table syn code: 
pathIds = '~/Documents/CLEAR3_Biaffect_Leow' #file path to folder containing healthCodes (eg: /Users/mindy/Desktop/BiAffect/Users)
outPath = 'insert folder file path here' #file path to where keypress files should go (eg: /Users/mindy/Desktop/BiAffect/Output)

### File of health code IDs
hc = pd.read_csv(pathIds + 'insert healthCodes csv filename here', index_col = False) 
#HealthID = list([hc])
HealthID = hc['healthCode'].to_list()

## Output file name (one file per user): 
nameOutFile = 'userKP_raw_U'

#%% Loop over each user and obtain their keypress data
for uIdx in range(0,len(HealthID)): 
    syn = synapseclient.login(email=username_synapse, password=password_synapse, rememberMe=True)
    testSubj = HealthID[uIdx]; #this is what needs to be fixed
    print(testSubj)
    
    # map userIDs to healthCodes
    userDict = dict(zip(hc.healthCode, hc.userID))
    
    #Each user is one output
    data = [];
    data.append(','.join(['recordId','healthCode','diagnostic', 'age',
                          'gender', 'phoneInfo', 'appVersion', 'session_duration',
                          'session_timestamp','timezone',
                          'keypress_timestamp','keypress_type','duration',
                          'distanceFromCenter','distanceFromPrevious'])+'\n')
    
    ## Get diagnosis: 
    diagnosisTable = "syn17015193"; #DiagnosisV3
    query_string = 'select * from {} where healthCode = {}'.format(diagnosisTable, str("'") + testSubj + str("'"))
    diag = syn.tableQuery(query_string);  df_diag = diag.asDataFrame();

    df_diag = df_diag.sort_values("healthCode"); df_diag = df_diag.drop_duplicates(["healthCode"]) 
    if(df_diag.size == 0):
        diag = float('NaN')
        print('No diagnosis');
    else:
        diag = df_diag["Bipolar diagnosis"][0]

    ## Get age, gender: 
    ageGenderTable = "syn12279831"; #birth-gender-v4
    query_string = 'select * from {} where healthCode = {}'.format(ageGenderTable, str("'") + testSubj + str("'"))
    ageGender = syn.tableQuery(query_string);  dfAgeGender = ageGender.asDataFrame();

    dfAgeGender = dfAgeGender.sort_values("healthCode"); dfAgeGender = dfAgeGender.drop_duplicates(["healthCode"]) 
    currentYear = datetime.datetime.now().year
    # age
    if(dfAgeGender["birth"].size == 0):
        age = float('NaN')
        print('No age');
    else:
        birthYear = dfAgeGender["birth"][0]
        age = currentYear - birthYear
    # gender
    if(dfAgeGender["gender"].size == 0):
        gender = float('NaN')
        print('No gender');
    else:
        gender = dfAgeGender["gender"][0]
    
    ## Get keyboard Info: 
    sessionTable = "syn7841520"; #Keyboard V2
    query_string = 'select * from {} where healthCode = {}'.format(sessionTable, str("'") + testSubj + str("'"))
    results = syn.tableQuery(query_string)
    keypressFiles = syn.downloadTableColumns(results,['Session.json.keylogs'])
    
    ## Extract info from Keyboard CVS
    resultsDF = results.asDataFrame()
    
    ## Collect here the columns from the CSV table you want in the output file:
    recordIDsNum = resultsDF['recordId'];
    timeStamps = resultsDF['Session.json.timestamp'];
    sessionDuration = resultsDF['Session.json.duration'];
    sessionTimestamp = resultsDF['createdOn']  
    timeZones = resultsDF['createdOnTimeZone']
    phoneInfo = resultsDF['phoneInfo'].str.replace(',', '-', n=1)
    appVer = resultsDF['appVersion'].str.replace(',', ';')
    
    r = -1; 
    ## Loop over the JSON files: 
    for file_id, path in keypressFiles.items():
        r = 1 + r;
        ## Open JSON file:
        with open(path) as f:
            cur_data = json.loads(f.read())
            for rec in cur_data:
                try:
                    cur_duration = str(rec['duration']) #Different than session Dur - this is time keypress held
                except KeyError:
                    cur_duration = ''
                    
                ## check for missing vars: 
                if ('distanceFromPrevious' in rec) == False: distPrev = float('NaN')
                else: distPrev = rec['distanceFromPrevious'];
                if ('distanceFromCenter' in rec) == False: distCenter = float('NaN')
                else: distCenter = rec['distanceFromCenter'];
                if ('radius' in rec) == False: radius = float('NaN')
                else: radius = rec['radius'];
                    
                ## Append rows to the output data file:
                data.append(','.join([str(recordIDsNum[r]),
                                      testSubj,str(diag), 
                                      str(age), str(gender),
                                      str(phoneInfo[r]),
                                      str(appVer[r]),
                                      str(sessionDuration[r]),
                                      str(sessionTimestamp[r]),
                                      str(timeZones[r]),
                                      str(rec['timestamp']),
                                      str(rec['value']),
                                      cur_duration,
                                      str(distCenter),
                                      str(distPrev)])+'\n')
    userID = userDict[testSubj]
    
    syn.logout()
    output_file = open(os.path.join(outPath,nameOutFile+str(userID)+'.csv'),'w')
    output_file.writelines(data)
    output_file.close()
