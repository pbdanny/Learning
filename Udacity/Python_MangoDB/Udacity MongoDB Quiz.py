#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 14 23:17:43 2017

@author: Danny

Quiz : Working with MongoDB

In this problem set you work with another type of infobox data, audit it,
clean it, come up with a data model, insert it into MongoDB and then run some
queries against your database. The set contains data about Arachnid class
animals.

Your task in this exercise is to parse the file, process only the fields that
are listed in the FIELDS dictionary as keys, and return a list of dictionaries
of cleaned values. 

The following things should be done:
- keys of the dictionary changed according to the mapping in FIELDS dictionary
- trim out redundant description in parenthesis from the 'rdf-schema#label'
  field, like "(spider)"
- if 'name' is "NULL" or contains non-alphanumeric characters, set it to the
  same value as 'label'.
- if a value of a field is "NULL", convert it to None
- if there is a value in 'synonym', it should be converted to an array (list)
  by stripping the "{}" characters and splitting the string on "|". Rest of the
  cleanup is up to you, e.g. removing "*" prefixes etc. If there is a singular
  synonym, the value should still be formatted in a list.
- strip leading and ending whitespace from all fields, if there is any
- the output structure should be as follows:

[ { 'label': 'Argiope',
    'uri': 'http://dbpedia.org/resource/Argiope_(spider)',
    'description': 'The genus Argiope includes rather large and spectacular spiders that often ...',
    'name': 'Argiope',
    'synonym': ["One", "Two"],
    'classification': {
                      'family': 'Orb-weaver spider',
                      'class': 'Arachnid',
                      'phylum': 'Arthropod',
                      'order': 'Spider',
                      'kingdom': 'Animal',
                      'genus': None
                      }
  },
  { 'label': ... , }, ...
]

  * Note that the value associated with the classification key is a dictionary
    with taxonomic labels.
"""
import csv
import pprint
import re
import os

os.chdir("/Users/Danny/Documents/Python Project/UdactiyMangoDB")


DATAFILE = 'arachnid.csv'
FIELDS ={'rdf-schema#label': 'label',
         'URI': 'uri',
         'rdf-schema#comment': 'description',
         'synonym': 'synonym',
         'name': 'name',
         'family_label': 'family',
         'class_label': 'class',
         'phylum_label': 'phylum',
         'order_label': 'order',
         'kingdom_label': 'kingdom',
         'genus_label': 'genus'}

def strip_spider(s):
    """ Remove (spider) from label
    """
    return re.sub(r'\s*\(spider\)\s*', '', s)

def strip_mites(s):
    """ Remove (mites) from label
    """
    return re.sub(r'\s*\(mites\)\s*', '', s)

def null_to_none(s):
    """ Change null to none
    """
    if s == 'NULL':
        return None
    return s

def parse_array(v):
    """ Check if string is array which start with { and end with }
    """
    if (v[0] == "{") and (v[-1] == "}"):
        v = v.lstrip("{")  # Left strip {
        v = v.rstrip("}")  # Right strip }
        v_array = v.split("|")  # Split each separator '|' to a list
        # List comprehension strip all whitespace and '*'
        v_array = [i.strip().strip('*') for i in v_array]
        return v_array
    return v  # If string is not array return as-is

def enlist_synonym(v):
    """ Prase 'synonym' value if not Null return list
    """
    if v == 'NULL':  # return 'NULL' when NULL
        return v
    elif v[0] != '{':  # If not 'NULL' but also not array
        return [v]
    else:
        return parse_array(v)
        

def process_file(filename, fields):

    process_fields = fields.keys()  # target process fields
    data = []  # target output list
    
    with open(filename, "r") as f:
        reader = csv.DictReader(f)       
        
        for i in range(3):  # Skipped 3 row of unused data
            next(reader, None)
        
        for line in reader:  
            
            each_data = {}  # Create each_data as dict
            classif = {}  # Classification also as dict
            
            for key, val in line.items():  # Traverse key and value in each row
                
                if key not in process_fields:  # Data key not in process key then skipped to next key
                    continue
                
                if key == 'rdf-schema#label':  # Edit label by trimmed all string in the parenthesis
                    val = strip_spider(val)
                    val = strip_mites(val)
                    
                if key == 'name':  # Correct 'name'
                    if val == 'NULL' or not val.isalpha():
                        val = line['rdf-schema#label']
                if key == 'synonym':  # correct 'synonym' value
                    val = enlist_synonym(val)
                
                #val = val.strip()  # Remove whitespace
                val = parse_array(val)  # prase {} to array
                val = null_to_none(val)  # Change NULL to None
                
                # If data in classification then create sub dict 
                if key in ['family_label', 'class_label', 'phylum_label', 'order_label', 'kingdom_label', 'genus_label']:
                    classif[fields[key]] = val
                # If data not in classif then create each_data from fields    
                else:  
                    each_data[fields[key]] = val
                
                # add classification to each_data from created sub dict
            if classif:
                each_data['classification'] = classif
                
            # Append cleased data back to list of data
            data.append(each_data)
    return data

""" Process data cleaning & insert to MongoDB - db 'examples' 
- collecstion 'arachnid' 
"""

data = process_file(DATAFILE, FIELDS)

# shell command start MongoDB demon
# !mongod --dbpath /Users/Danny/data/db

from pymongo import MongoClient
client = MongoClient('mongodb://localhost:27017')
db = client.examples
db.arachnid.insert_many([i for i in data])

