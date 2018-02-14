#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 11 23:52:43 2017

@author: Danny
"""

import csv
import os

os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')
CITIES = 'cities.csv'


def array_to_list(arr):
    a_1 = arr.replace('{', '')  # remove { from begining
    a_2 = a_1.replace('}', '')  # remove } from end
    li = a_2.split('|')  # split by '|'
    
    return li

def fix_name(name):
    li_out = list()
    if name == 'NULL':
        return li_out
    elif name[0] != '{':
        li_out.append(name)
        return li_out
    else:
        return array_to_list(name)


def process_file(filename):
    data = []
    with open(filename, "r") as f:
        reader = csv.DictReader(f)
        #skipping the extra metadata
        for i in range(3):
            next(reader, None)
        # processing file
        for line in reader:
            # calling your function to fix the area value
            if "name" in line:
                line["name"] = fix_name(line["name"])
            data.append(line)
    return data