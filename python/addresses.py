
class db:
    def __init__(self):
	self.data = []

    def select(self, field, value):
	out = []
	for rec in self.data:
	    if rec[field] == value:
		out.append(rec)
	outdb = db()
	outdb.data = out
	return outdb

    def keys (self):
	return self.data[0].keys()

    def values(self, field, sorted=False):
	out = []
	for rec in self.data:
	    if not rec[field] in out:
		out.append(rec[field])

        if sorted: out.sort()
	return out

    def read(self, fn):
	self.data = []
	inf = open(fn)
	for line in iter(inf):
	    rec = line.split(';')
	    d = {}
	    d['first'] = rec[1]
	    d['last'] = rec[0]
	    d['category'] = rec[2]
	    d['title'] = rec[3]
	    d['company'] = rec[4]
	    d['work'] = rec[5]
	    d['home'] = rec[6]
	    d['cell'] = rec[7]
	    d['fax'] = rec[8]
	    d['other'] = rec[9]
	    d['email'] = rec[10]
	    d['address'] = rec[11]
	    d['city'] = rec[12]
	    d['state'] = rec[13]
	    d['zip'] = rec[14]
	    d['country'] = rec[15]
	    d['custom1'] = rec[16]
	    d['custom2'] = rec[17]
	    d['custom3'] = rec[18]
	    d['custom4'] = rec[19]
	    d['note'] = rec[20]
	    self.data.append(d)
	return self
    
