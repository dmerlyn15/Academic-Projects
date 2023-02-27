#The EPA website provides a vast amount of data on public water systems (PWS) across the United States. 
#On the topic of violations, my goal was to find the worst offenders across the board. 
#The scope of this question was complex, and I would be attempting to approach the problem from several angles before deciding on a single solution. 
#I would be leveraging two main datasets available through the EPA’s Data Downloads page. 
#These would be the SWDA Violations and SWDA Serious Violators consisting of 1,048,576 and 54805 instances respectively. 
#The variables that I would be focusing on are  State name, city served, Fiscal Year, PWSID, PWS size, Rule Name, and Serious Violator.
#Considering that I would be looking for the worst offenders, my approach would be to tackle the most serious violations first to acquire insight into the data. 
#I would aim to arrive upon the top 10 states and the top 10 cities within each of these states which are repeat serious violators. 




#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
import missingno as msno
import seaborn as sns


# In[3]:


df_new =pd.read_csv("C:/Users/dsouz/Desktop/ALY6080/SDWA_VIOLATIONS - NEW.csv")


# In[4]:


df2 = pd.read_csv("C:/Users/dsouz/Desktop/ALY6080/SDWA_SERIOUS_VIOLATORS.csv")


# In[7]:


df_merged =pd.merge(df_new[['PWSID','PWS_NAME','STATE', 'STATE_NAME', 'PWS_TYPE_CODE', 'PWS_TYPE_SHORT', 'SOURCE_WATER', 'PWS_SIZE', 'POPULATION_SERVED_COUNT', 'FISCAL_YEAR', 'VIOLATION_NAME', 'VIOLATION_ID', 'RULE_NAME','BEGIN_YEAR', 'END_YEAR', 'RTC_YEAR', 'ACUTE_HEALTH_BASED', 'HEALTH_BASED', 'MONITORING_REPORTING', 'PUBLIC_NOTIF_OTHER']], df2[['PWSID','CITY_SERVED', 'FISCAL_YEAR','SERIOUS_VIOLATOR']],  
               how='left', left_on=['PWSID', 'FISCAL_YEAR'], 
               right_on = ['PWSID', 'FISCAL_YEAR'])


# In[8]:


df_merged.head()


# In[9]:


df_merged.CITY_SERVED.unique()


# In[10]:


df_merged.STATE.unique()


# In[11]:


df_new.info()


# In[12]:


df2.info()


# In[15]:


df_merged['PWSID'] = df_merged['PWSID'].astype(str)
df_merged['CITY_SERVED'] = df_merged['CITY_SERVED'].astype(str)
df_merged['STATE'] = df_merged['STATE'].astype(str)
df_merged['STATE_NAME'] = df_merged['STATE_NAME'].astype(str)
df_merged['PWS_SIZE'] = df_merged['PWS_SIZE'].astype(str)
df_merged['VIOLATION_NAME'] = df_merged['VIOLATION_NAME'].astype(str)
df_merged['VIOLATION_ID'] = df_merged['VIOLATION_ID'].astype(str)
df_merged['RULE_NAME'] = df_merged['RULE_NAME'].astype(str)
df_merged['ACUTE_HEALTH_BASED'] = df_merged['ACUTE_HEALTH_BASED'].map(dict(Y=1, N=0))
df_merged['HEALTH_BASED'] = df_merged['HEALTH_BASED'].map(dict(Y=1, N=0))
df_merged['MONITORING_REPORTING'] = df_merged['MONITORING_REPORTING'].map(dict(Y=1, N=0))
df_merged['PUBLIC_NOTIF_OTHER'] = df_merged['PUBLIC_NOTIF_OTHER'].map(dict(Y=1, N=0))
df_merged['SERIOUS_VIOLATOR'] = df_merged['SERIOUS_VIOLATOR'].map(dict(Y=1, N=0))


# In[16]:


df_merged.head()


# In[60]:


top_violators = df_merged.groupby('STATE_NAME') ['SERIOUS_VIOLATOR'].count().reset_index().sort_values(by= 'SERIOUS_VIOLATOR', ascending = False).head(10)

top_violators


# In[17]:


city_df_merged = df_merged[df_merged['CITY_SERVED'] != 'nan']
city_df_merged[city_df_merged['STATE'] == 'TX']


# In[20]:


table_vs_merged = pd.pivot_table(city_df_merged[(city_df_merged['PWS_SIZE'] == 'Very Small') &
                               (city_df_merged['STATE'] != 'AS') & 
                               (city_df_merged['STATE'] != '108') & 
                               (city_df_merged['STATE'] != 'NN') &
                               (city_df_merged['STATE'] != 'MP') &
                               (city_df_merged['STATE'] != '124') &
                               (city_df_merged['STATE'] != '608') &
                               (city_df_merged['STATE'] != '180') & 
                               (city_df_merged['STATE'] != '117')],
                       values='SERIOUS_VIOLATOR', 
                       index=['CITY_SERVED',
                              'STATE_NAME', 
                              'STATE', 
                              'PWS_NAME',
                              'PWS_SIZE',
                              'VIOLATION_NAME',
                             'RULE_NAME'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

city_table_vs_merged = table_vs_merged.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(10)
city_table_vs_merged


# In[25]:


table_vs_merged = pd.pivot_table(city_df_merged[(city_df_merged['PWS_SIZE'] == 'Very Small')],
                       values='SERIOUS_VIOLATOR', 
                       index=['CITY_SERVED',
                              'STATE_NAME', 
                              'STATE', 
                              'PWS_NAME',
                              'PWS_SIZE',
                              'VIOLATION_NAME',
                             'RULE_NAME'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

city_table_vs_merged = table_vs_merged.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(10)
city_table_vs_merged


# In[26]:


table_s_merged = pd.pivot_table(city_df_merged[(city_df_merged['PWS_SIZE'] == 'Small')], 
                       values='SERIOUS_VIOLATOR', 
                       index=['CITY_SERVED',
                              'STATE_NAME', 
                              'STATE', 
                              'PWS_NAME',
                              'PWS_SIZE',
                              'VIOLATION_NAME',
                             'RULE_NAME'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

city_table_s_merged = table_s_merged.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(10)
city_table_s_merged


# In[27]:


table_m_merged = pd.pivot_table(city_df_merged[(city_df_merged['PWS_SIZE'] == 'Medium')], 
                       values='SERIOUS_VIOLATOR', 
                       index=['CITY_SERVED',
                              'STATE_NAME', 
                              'STATE', 
                              'PWS_NAME',
                              'PWS_SIZE',
                              'VIOLATION_NAME',
                             'RULE_NAME'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

city_table_m_merged = table_m_merged.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(10)
city_table_m_merged


# In[31]:


table_l_merged = pd.pivot_table(city_df_merged[(city_df_merged['PWS_SIZE'] == 'Large')], 
                       values='SERIOUS_VIOLATOR', 
                       index=['CITY_SERVED',
                              'STATE_NAME', 
                              'STATE', 
                              'PWS_NAME',
                              'PWS_SIZE',
                              'VIOLATION_NAME',
                             'RULE_NAME'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

city_table_l_merged =table_l_merged.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(10)
city_table_l_merged


# In[32]:


table_vl_merged = pd.pivot_table(city_df_merged[(city_df_merged['PWS_SIZE'] == 'Very Large')], 
                       values='SERIOUS_VIOLATOR', 
                       index=['CITY_SERVED',
                              'STATE_NAME', 
                              'STATE', 
                              'PWS_NAME',
                              'PWS_SIZE',
                              'VIOLATION_NAME',
                             'RULE_NAME'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

city_table_vl_merged = table_vl_merged.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(10)
city_table_vl_merged


# In[33]:


table1 = pd.pivot_table(df_merged[df_merged['STATE'] != 'AS'],
                       values='SERIOUS_VIOLATOR', 
                       index=['STATE_NAME', 
                              'STATE',
                              'VIOLATION_NAME',
                             'RULE_NAME',
                             'PWS_SIZE',
                             'ACUTE_HEALTH_BASED',
                             'HEALTH_BASED'],
                             #'FISCAL_YEAR'],
                       aggfunc=np.sum).reset_index()

main_table = table1.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(50)
main_table


# In[34]:


all_data = city_table_vs_merged.append(city_table_s_merged).append(city_table_m_merged).append(city_table_l_merged).append(city_table_vl_merged)

all_data.PWS_SIZE.unique()


# In[35]:


top_10_data =all_data.sort_values(by = 'SERIOUS_VIOLATOR', ascending = False).head(20)
top_10_data


# In[36]:


#top 20 repeat serious violators for very small PWSs
city_table_vs_merged.groupby('STATE_NAME')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[41]:


city_table_vs_merged.groupby('CITY_SERVED')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[37]:


#top 20 repeat serious violators for small PWSs
city_table_s_merged.groupby('STATE_NAME')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[42]:



city_table_s_merged.groupby('CITY_SERVED')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[38]:


#top 20 repeat serious violators for medium PWSs
city_table_m_merged.groupby('STATE_NAME')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[43]:


city_table_m_merged.groupby('CITY_SERVED')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[39]:


#top 20 repeat serious violators for large PWSs
city_table_l_merged.groupby('STATE_NAME')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[44]:


city_table_l_merged.groupby('CITY_SERVED')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[40]:


#top 20 repeat serious violators for very large PWSs
city_table_vl_merged.groupby('STATE_NAME')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[45]:


city_table_vl_merged.groupby('CITY_SERVED')['RULE_NAME']    .value_counts()    .unstack(level=1)    .plot.bar(stacked=True)


# In[61]:


pip install openpyxl


# In[62]:


df_merged.to_excel (r'C:\Users\dsouz\Desktop\export_dataframe.xlsx', index = False, header=True)

#"C:/Users/dsouz/Desktop/ALY6080/


# In[ ]:




