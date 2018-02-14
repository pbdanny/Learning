#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 24 00:09:30 2017

@author: Danny

Open streen map Quiz tag type

"""
import os
import xml.etree.ElementTree as ET
# import re

filename = 'example.osm'

def get_user(filename):

    # Start prasing tree
    tree = ET.parse(filename)
    
    uid = set()
    
    for node in tree.iter():
        try:
            uid.add(node.attrib['uid'])
        except Exception:
            pass
        
    return uid


"""
Main testing part
"""
os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')

users = get_user('example.osm')
users