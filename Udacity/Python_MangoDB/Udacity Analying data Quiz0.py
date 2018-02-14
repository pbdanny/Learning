#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 19 00:06:42 2017

@author: Danny

Udacity Analysing data - Quiz
"""
import pprint

from pymongo import MongoClient
client = MongoClient('mongodb://localhost:27017')
db = client.twitter

# Quiz 1 : Most Common city name 
def make_pipeline1():
    pipeline = [
            {'$match': {'name': {'$exists': True, "$ne": None}}},
            {'$group': {'_id': '$name',
                        'count': {'$sum': 1}}},
            {'$sort': {'count': -1}},
            {'$limit': 1}
    ]
    return pipeline

# Quiz 2 : Which Region in India has the largest number of cities 
# with longitude between 75 and 80?
def make_pipeline2():
    pipeline = [
            {'$match': {'country' : 'India'}},
            {'$match': {'lon': {'$gte': 75, '$lte': 80}}},
            {'$unwind': '$isPartOf'},
            {'$group': {'_id': '$isPartOf',
                        'count': {'$sum': 1}}},
            {'$sort': {'count': -1}},
            {'$limit': 1}
    ]
    return pipeline

# Quiz 3: the average regional city population for all countries
def make_pipeline3():
    pipeline = [
            {'$match': {'country' : 'India'}},
            {'$match': {'lon': {'$gte': 75, '$lte': 80}}},
            {'$unwind': '$isPartOf'},
            {'$group': {'_id': '$isPartOf',
                        'count': {'$sum': 1}}},
            {'$sort': {'count': -1}},
            {'$limit': 1}
    ]
    return pipeline


def tweet_sources(db, pipeline):
    return [doc for doc in db.tweets.aggregate(pipeline)]

pipeline = make_pipeline1()
result = tweet_sources(db, pipeline)
pprint.pprint(result[0])

