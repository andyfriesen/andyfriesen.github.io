#!/usr/bin/python

from glob import glob
import os
import datetime

def stripPostName(fileName):
    prefix = '_posts/yyyy-mm-dd-'
    suffix = '.md'
    return fileName[len(prefix):-len(suffix)]

def stripLhsName(fileName):
    prefix = '_lhs/'
    suffix = '.lhs'
    return fileName[len(prefix):-len(suffix)]

posts = map(stripPostName, glob('_posts/*.md'))
lhs = map(stripLhsName, glob('_lhs/*.lhs'))

for l in lhs:
    if l not in posts:
        now = datetime.date.today()
        today = '%04i-%02i-%02i' % (now.year, now.month, now.day)
        cmd = 'runhaskell _lhs/lhs-to-jekyll-markdown.lhs < _lhs/{0}.lhs > _posts/{1}-{0}.md'.format(l, today)
        print 'running', cmd
        os.system(cmd)
