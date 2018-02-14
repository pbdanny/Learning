#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 20 22:30:11 2017

@author: Danny

Open streen map Quiz
"""
import os
import xml.etree.ElementTree as ET

filename = 'example.osm'

def count_tags(filename):
        
    tree = ET.parse(filename)
    
    # root = tree.getroot()
    t = list()
    
    # Python 3.6 up use xml-object.iter() to traverse all items under
    for node in tree.iter():
        t.append(node.tag)
    
    # return unique + count in each tag names as dict 
    return dict((x,t.count(x)) for x in set(t))
        

os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')
tags = count_tags('example.osm')
