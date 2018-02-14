#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 23 02:58:02 2017

@author: Danny

Open streen map Quiz tag type
"""
import os
import xml.etree.ElementTree as ET
import re

""" 
fixed parameters + varibles

"""

filename = 'example.osm'

lower = re.compile(r'^([a-z]|_)*$')
lower_colon = re.compile(r'^([a-z]|_)*:([a-z]|_)*$')
problemchars = re.compile(r'[=\+/&<>;\'"\?%#$@\,\. \t\r\n]')


def process_map(filename):
    
    # Start prasing tree
    tree = ET.parse(filename)
    
    # Create dict for keep matching regular expression in each key
    keys = {"lower": 0, "lower_colon": 0, "problemchars": 0, "other": 0}
    
    for node in tree.iter(tag = 'tag'):
        v = node.attrib['k']
        if re.search(problemchars, v):
            keys['problemchars'] += 1
        elif re.search(lower_colon, v):
            keys['lower_colon'] += 1
        elif re.search(lower, v):
            keys['lower'] += 1
        else:
            keys['other'] += 1
        
    return keys

"""
Main testing part
"""
os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')
keys = process_map('example.osm')
assert keys == {'lower': 5, 'lower_colon': 0, 'other': 1, 'problemchars': 1}