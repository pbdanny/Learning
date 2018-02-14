#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 10 22:45:45 2017

@author: Danny
"""
import os
import csv

os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')

CITIES = 'cities.csv'

def string_to_float(s):
    try:
        return float(s)
    except ValueError:
        return None
    
def find_long_float(s):
    s_1 = s.replace('{', '')  # remove { from begining
    s_2 = s_1.replace('}', '')  # remove } from end
    li = s_2.split('|')  # split by '|'
    
    max_li = len(li[0])  # max # char to first of list 
    max_li_str = li[0]  # value of max # char
    for i in li:
        if len(i) > max_li:
            max_li_str = i
    return string_to_float(max_li_str)


def fix_area(area):
    try:
        return float(area)
    except ValueError:
        if '{' in area:
            return find_long_float(area)
        else:
            return None


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
            if "areaLand" in line:
                line["areaLand"] = fix_area(line["areaLand"])
            data.append(line)
    print('Length of data', len(data))
    return data    