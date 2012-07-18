#!/usr/bin/env python2

# Simple script for sending out messages via smtp.  
# This is an alternative to using sendmail

import smtplib
import os
import sys
import string

msg = [ line for line in sys.stdin ]
msgtext = string.join([line[:-1] for line in msg],sep="\n")

def get_headers(msg):
    i = 0
    headers = {}
    while i < len(msg):
        line = msg[i]
        line = line[:-1]
        if line == "": break
        (field,data) = line.split(":",1)
        field = field.lower().strip()
        data = data.strip()
        if field == "from":
            headers["from"] = data
        elif field == "to":
            headers["to"] = [ addr.strip() for addr in data.split(",") ]
        i = i + 1
    return headers

headers = get_headers(msg)
smtp = smtplib.SMTP("smtp.earthlink.net")
smtp.sendmail(headers["from"],headers["to"],msgtext)
