#!/usr/bin/env python
#
# Get_dates.py: To get the list of date  times from a Start Date To endDate with a specific interval 
# in days. 
###################################################################
import datetime
import sys

#print ("Len of arguments = ", len(sys.argv)) 
if len(sys.argv) not in [5]:
   #        usage()
        raise SystemExit, "ERROR, invalid number of command line arguments!"
sd = sys.argv[1]
ed = sys.argv[2]
Interval = sys.argv[3]
fname = sys.argv[4]
dt = int(Interval)
year1 = int(sd[0:4])
month1 = int(sd[4:6])
day1 = int(sd[6:8])
sdate = datetime.date(year1,month1,day1)
year2 = int(ed[0:4])
month2 = int(ed[4:6])
day2 = int(ed[6:8])
edate = datetime.date(year2,month2,day2)
 
cdate = sdate
#outfile = 'Datetime.dat'
outfile = fname
output=open(outfile,"wb")
while cdate <= edate:
	print cdate
        cdate2 = cdate.strftime("%Y")+cdate.strftime("%m")+cdate.strftime("%d")+'\n'
	output.write(cdate2)
	incr=datetime.timedelta(days=dt)
	cdate = cdate + incr

output.close()
#------end

