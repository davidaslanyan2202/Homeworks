#!/usr/bin/env python
# coding: utf-8

# In[8]:


import pandas as pd

file_path = "/Users/davidaslanyan/Documents/AUA I 2425Spring I Courses/Data Visualization/Homeworks_DV/crime_data.csv"
df = pd.read_csv(file_path)
print(df.head())
missing_values = df.isnull().sum()
missing_values = missing_values[missing_values > 0]
print("Missing values per column:\n", missing_values)
df_cleaned = df.dropna(thresh=len(df) * 0.5, axis=1).copy()
df_cleaned.columns = df_cleaned.columns.str.strip().str.lower()
df_cleaned.loc[:, 'date occ'] = pd.to_datetime(df_cleaned['date occ'], errors='coerce')
df_cleaned.loc[:, 'year'] = df_cleaned['date occ'].dt.year
df_cleaned.loc[:, 'month'] = df_cleaned['date occ'].dt.month
df_cleaned.loc[:, 'day'] = df_cleaned['date occ'].dt.day
df_cleaned.loc[:, 'hour'] = df_cleaned['time occ'] // 100
df_2023 = df_cleaned[df_cleaned['year'] == 2023]
df_burglary = df_2023[df_2023['crm cd desc'] == 'BURGLARY']
crime_summary = df_cleaned.groupby('area name').agg(
    total_crimes=('crm cd desc', 'count'),
    average_victim_age=('vict age', 'mean')
).sort_values(by='total_crimes', ascending=False)
print(crime_summary.head())
top_crimes = df_cleaned['crm cd desc'].value_counts().head(3)
print("Top 3 Crimes:\n", top_crimes)
crime_by_hour = df_cleaned.groupby('hour').size()
print(crime_by_hour)
victim_summary = df_cleaned.groupby('vict sex').agg(
    total_crimes=('crm cd desc', 'count'),
    average_victim_age=('vict age', 'mean')
)
print(victim_summary)
def severity_score(row):
    if 'weapon used cd' in df_cleaned.columns and pd.notnull(row.get('weapon used cd')):
        return 5
    elif row['crm cd desc'] == 'BURGLARY':
        return 3
    else:
        return 1
df_cleaned['severity score'] = df_cleaned.apply(severity_score, axis=1)
severity_by_area = df_cleaned.groupby('area name')['severity score'].sum()
print(severity_by_area)


# In[ ]:




