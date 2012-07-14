#!/usr/bin/env python

import sys
import os
import urllib2
import urllib
import subprocess
import re

def get_online_score(mapfile, route):
    req = urllib2.Request(url='http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi',
                          data=urllib.urlencode({'mapfile': mapfile, 'route': route}))
    rep = urllib2.urlopen(req).read()
    board = re.search(r'<pre>(.*)</pre>', rep, re.M | re.S).groups()[0]
    score = re.search(r'Score: (-?[0-9]*)', rep).groups()[0]
    return (board, score)

def download_high_scores():
    req = urllib2.Request(url='http://undecidable.org.uk/~edwin/cgi-bin/weblifter.cgi?standings=1')
    rep = urllib2.urlopen(req).read()
    scores = re.findall(r'<h4>(.+?)</h4><p>([^<]+)', rep)
    nscores = {}
    for (name, values) in scores:
        nscores[name] = values
    return nscores

def test_bot(mapfile):
    os.chdir('src')
    out, err = subprocess.Popen(['runhaskell', 'Main.hs'], stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    if err:
        print >> sys.stderr, err
        return
    print out
    return
    mapid = os.path.basename(mapfile)
    onl_board, onl_score = get_online_score(mapid, route)
    high_scores = download_high_scores()
    print route

if __name__ == '__main__':
    test_bot(sys.argv[1])

