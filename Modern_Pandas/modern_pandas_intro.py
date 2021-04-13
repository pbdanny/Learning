import os
import zipfile

import requests
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

import requests

# -------------------
# Modern Pasdas Part I
# -------------------

# ------------------------------
# Get data from website with
# requests
# ------------------------------
# Parameter for Data loading from website with http request
headers = {
    'Referer': 'https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time',
    'Origin': 'https://www.transtats.bts.gov',
    'Content-Type': 'application/x-www-form-urlencoded',
}

params = (
    ('Table_ID', '236'),
    ('Has_Group', '3'),
    ('Is_Zipped', '0'),
)

# read parameter for request
with open('modern-1-url.txt', encoding='utf-8') as f:
    data = f.read().strip()

# create data directory to save loaded data file
os.makedirs('data', exist_ok=True)
dest = "data/flights.csv.zip"

# read data and save as zipfile
if not os.path.exists(dest):
    r = requests.post('https://www.transtats.bts.gov/DownLoad_Table.asp',
                      headers=headers, params=params, data=data, stream=True)

    with open("data/flights.csv.zip", 'wb') as f:
        for chunk in r.iter_content(chunk_size=102400): 
            if chunk:
                f.write(chunk)

# ----------------------------------
# extract data and read with pandas
# ----------------------------------
zf = zipfile.ZipFile("data/flights.csv.zip")
# extract zip file, return file path
fp = zf.extract(zf.filelist[0].filename, path='data/')
df = pd.read_csv(fp, parse_dates=["FL_DATE"],
                 usecols=np.arange(0, 32)).rename(columns=str.lower)

df.info()

# -----------------
# slicing/indexing
# -----------------
# for row slicing always use .loc / .iloc 
# Avoid chain indexing "df[][]"" at all cost

# Label indexing is our best friends
df.loc[10:15, ['fl_date', 'tail_num']]

# Label indexing could handle boolean indexing, column range indexing
# also could do column skipping (like list skipping)
df.loc[df['fl_date'] >= '2017-01-15']
df.loc[df['fl_date'] >= '2017-01-15', :]
df.loc[df['fl_date'] >= '2017-01-15', 'fl_date':'tail_num']
df.loc[:, 'fl_date'::2]

# .iloc, integer indexing
# is EXCLUSIVE, could not easily select column by name
df.iloc[10:15] # will get only 5 rows since it is EXCLUSIVE

# Slicing columns need help from .get_loc
df.iloc[10:15, df.columns.get_loc('fl_date'):df.columns.get_loc('tail_num')]

# .iloc for boolean indexing, must pass row with numpy type
df.iloc[(df['fl_date'] >= '2017-01-15').values, df.columns.get_loc('fl_date'):df.columns.get_loc('tail_num')]

# groupby.first = 1st resulf of groupby -> create as Dataframe
first = df.groupby('airline_id')[['fl_date', 'unique_carrier']].first()
first.head()
first.iloc[10:15,:]

# row & column name indexing
first = df.groupby('unique_carrier').first()
first.loc[['AA','AS', 'DL'], ['fl_date', 'tail_num']]

# boolean slicing without loc/iloc, unstable result
f = pd.DataFrame({'a':list(range(1,6)), 'b':list(range(10, 60, 10))})
# unstable with no loc/iloc
f[f.a <= 3].b = f[f.a <= 3].b/10
# stable when use loc for boolean & column name indexing
f.loc[f['a'] <= 3, 'b'] = f.loc[f['a'] <= 3, 'b']/10

# multidimension index
hdf = df.set_index(['unique_carrier', 'origin', 'dest', 'tail_num', 
                    'fl_date']).sort_index()
hdf[hdf.columns[:4]].head()

# ---------------------
# Modern Pandas Part II
# ---------------------

def extract_city_name(df):
    '''
    Chicago, IL -> Chicago for origin_city_name and dest_city_name
    '''
    cols = ['origin_city_name', 'dest_city_name']
    city = df[cols].apply(lambda x: x.str.extract("(.*), \w{2}", expand=False))
    df = df.copy()
    df[['origin_city_name', 'dest_city_name']] = city
    return df

def time_to_datetime(df, columns):
    '''
    Combine all time items into datetimes.

    2014-01-01,0914 -> 2014-01-01 09:14:00
    '''
    df = df.copy()
    def converter(col):
        timepart = (col.astype(str)
                       .str.replace('\.0$', '')  # NaNs force float dtype
                       .str.pad(4, fillchar='0'))
        
        return pd.to_datetime(df['fl_date'] + ' ' +
                               timepart.str.slice(0, 2) + ':' +
                               timepart.str.slice(2, 4),
                               errors='coerce')
    
    df[columns] = df[columns].apply(converter)
    return df

# -----------------------
# Modern Pandas Part II
# -----------------------
# method chaining
df = (pd
      .read_csv('./data/813827284_T_ONTIME.csv')
      .drop('Unnamed: 32', axis=1)
      .rename(columns=str.lower)
      .pipe(time_to_datetime, ['dep_time', 'arr_time', 'crs_arr_time', 'crs_dep_time'])
      .assign(fl_date=lambda x: pd.to_datetime(x['fl_date']),
              dest=lambda x: pd.Categorical(x['dest']),
              origin=lambda x: pd.Categorical(x['origin']),
              tail_num=lambda x: pd.Categorical(x['tail_num']),
              unique_carrier=lambda x: pd.Categorical(x['unique_carrier']),
              cancellation_code=lambda x: pd.Categorical(x['cancellation_code']))
     )

(df.dropna(subset=['dep_time', 'unique_carrier'])
   .loc[df['unique_carrier']
       .isin(df['unique_carrier'].value_counts().index[:5])]
   .set_index('dep_time')
   # TimeGrouper to resample & groupby at once
   .groupby(['unique_carrier', pd.Grouper(freq='H')])
   .fl_num.count()
   .unstack(0)
   .fillna(0)
   .rolling(24)
   .sum()
   .rename_axis("Flights per Day", axis=1)
   .plot()
)
sns.despine()

flights = (df[['fl_date', 'tail_num', 'dep_time', 'dep_delay']]
           .dropna()
           .sort_values('dep_time')
           .loc[lambda x: x.dep_delay < 500]
           .assign(turn = lambda x:
                x.groupby(['fl_date', 'tail_num'])
                 .dep_time
                 .transform('rank').astype(int)))

fig, ax = plt.subplots(figsize=(15, 5))
sns.boxplot(x='turn', y='dep_delay', data=flights, ax=ax)
ax.set_ylim(-50, 50)
sns.despine()

plt.figure(figsize=(15, 5))
(df[['fl_date', 'tail_num', 'dep_time', 'dep_delay']]
    .dropna()
    .assign(hour=lambda x: x.dep_time.dt.hour)
    .query('5 < dep_delay < 600')
    .pipe((sns.boxplot, 'data'), 'hour', 'dep_delay'))


# -----------------------
# Modern pandas part III
# -----------------------
