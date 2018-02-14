#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 19 23:49:14 2017

@author: Danny
"""

#import pyodbc
#
#[x for x in pyodbc.drivers() if x.startwith('Microsoft Access Driver')]

import jaydebeapi

ucanaccess_jars = [
        "/Users/Danny/Documents/UCanAccess-4.0.2-bin/ucanaccess-4.0.2.jar",
        "/Users/Danny/Documents/UCanAccess-4.0.2-bin/lib/commons-lang-2.6.jar",
        "/Users/Danny/Documents/UCanAccess-4.0.2-bin/lib/commons-logging-1.1.1.jar",
        "/Users/Danny/Documents/UCanAccess-4.0.2-bin/lib/hsqldb.jar",
        "/Users/Danny/Documents/UCanAccess-4.0.2-bin/lib/hsqldb.jar"
        ]

classpath = ":".join(ucanaccess_jars)

cnxn = jaydebeapi.connect(
    "net.ucanaccess.jdbc.UcanaccessDriver",
    "jdbc:ucanaccess:///Users/Danny/Share Win7/OSSDatabase-RL.accdb;newDatabaseVersion=V2010",
    ["", ""],
    classpath
    )