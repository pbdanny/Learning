import pandas as pd

df = pd.read_csv('ab_data.csv')

# data check
df['user_id'].agg({pd.Series.count, pd.Series.nunique})

# create flagging group
flg = ['correct', 'correct', 'remove', 'remove']
gr = ['control', 'treatment', 'control', 'treatment']
lnd = ['old_page', 'new_page', 'new_page', 'old_page']

flag = pd.DataFrame({'flag':flg, 'group':gr, 'landing_page':lnd})

# create flagging group # method list of row
data = [['correct', 'control', 'old_page'], 
        ['correct', 'treatment', 'new_page'],
        ['remove', 'control', 'new_page'],
        ['remove', 'treatment', 'old_page']]
flag = pd.DataFrame(data = data, 
                    columns=['flag', 'group', 'landing_page'], 
                    index=['T', 'T', 'F', 'F'])


# drop control -> new_page / treatment -> old_page
df = df[
    (df['group'] == 'control') & (df['landing_page'] == 'old_page') | 
    (df['group'] == 'treatment') & (df['landing_page'] == 'new_page')]

df['user_id'].agg({pd.Series.count, pd.Series.nunique})

# Baseline from control group
control_df = df[df['group'] == 'control']
treat_df = df[df['group'] ==  'treatment']
baseline = control_df['converted'].sum() / control_df.shape[0]

import statsmodels.api as sm
target_kpi_increase = 0.01
sig_lv = 0.05
sensitiviy_lv = 0.8

effect_size = sm.stats.proportion_effectsize(baseline, baseline+target_kpi_increase)
sample_size = sm.stats.NormalIndPower().solve_power(effect_size, power = sensitiviy_lv, alpha = sig_lv, ratio=1)


# A/B testing
# https://cosmiccoding.com.au/tutorials/ab_tests

num_a, num_b = 550, 450
click_a, click_b = 48, 56
rate_a, rate_b = click_a / num_a, click_b / num_b


import matplotlib.pyplot as plt
from scipy.stats import binom
import numpy as np

# Determine the probability of having x number of click throughs
clicks = np.arange(20, 80)
prob_a = binom(num_a, rate_a).pmf(clicks)
prob_b = binom(num_b, rate_b).pmf(clicks)

# Make the bar plots.
plt.bar(clicks, prob_a, label="A", alpha=0.7)
plt.bar(clicks, prob_b, label="B", alpha=0.7)
plt.legend()
plt.xlabel("Num converted"); plt.ylabel("Probability");
