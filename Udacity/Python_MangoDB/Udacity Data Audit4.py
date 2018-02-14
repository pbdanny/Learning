#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 23:17:10 2017

@author: Danny
"""
import os
import csv

os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')
CITIES = 'cities.csv'


def check_loc(point, lat, longi):
    p_lat_long = point.split(" ")
    if p_lat_long[0] == lat and p_lat_long[1] == longi:
        return True
    else:
        return False


def process_file(filename):
    data = []
    with open(filename, "r") as f:
        reader = csv.DictReader(f)
        #skipping the extra matadata
        for i in range(3):
            next(reader, None)
        # processing file
        for line in reader:
            # calling your function to check the location
            result = check_loc(line["point"], line["wgs84_pos#lat"], line["wgs84_pos#long"])
            if not result:
                print("{}: {} != {} {}".format(line["name"], line["point"], line["wgs84_pos#lat"], line["wgs84_pos#long"]))
            data.append(line)

    return data