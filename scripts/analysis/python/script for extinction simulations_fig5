from numpy import where,zeros
from random import randrange,random
import numpy.ma as ma
import csv
from random import sample
###################################
def ext_w(mat, ext_type = 'top'):
	wmat = ma.masked_invalid(mat*(mat.T/mat.sum(1)).T)
	w0 = wmat.sum()
	row_n,col_n = mat.shape
	res = [[0,1]]
	if ext_type == 'top':
		tot_n = col_n
	elif ext_type == 'bottom':
		tot_n = row_n
	else:
		tot_n = col_n+row_n
	for sc in range(tot_n):
		if mat.sum()>0:
			if ext_type == 'top':
				cons_rand = sample(list(where(wmat.sum(0)>0)[0]),1)
				mat[:,cons_rand] = 0.0
			elif ext_type == 'bottom':
				res_rand = sample(list(where(wmat.sum(1)>0)[0]),1)
				mat[res_rand,:] = 0.0
			else:
				if sample([0,1],1)[0] == 0:
					cons_rand = sample(list(where(wmat.sum(0)>0)[0]),1)
					mat[:,cons_rand] = 0.0
				else:
					res_rand = sample(list(where(wmat.sum(1)>0)[0]),1)
					mat[res_rand,:] = 0.0
			wmat = ma.masked_invalid(mat*(mat.T/mat.sum(1)).T).filled(0)
		res.append([sc+1,wmat.sum()/w0])
	return	res



def null_m(net):
	cons_sp = sorted(list(set([i[1] for i in net])))
	r = range(len(cons_sp))
	adj = [[[i[0],i[2]] for i in net if i[1]==sp] for sp in cons_sp]
	for rep in range(10000):
		c1,c2 = sample(r,2)
		s1 = size_dict[cons_sp[c1]]
		s2 = size_dict[cons_sp[c2]]
		ratio = s1/s2
		if 0.8<ratio<1.2:
			c1_r = randrange(len(adj[c1]))
			c2_r = randrange(len(adj[c2]))
			c1_p = adj[c1][c1_r]
			c2_p = adj[c2][c2_r]
			if (c1_p[0]==c2_p[0]) or ((c1_p[0] not in [i[0] for i in adj[c2]]) and (c2_p[0] not in [j[0] for j in adj[c1]])):
				tot_w = c1_p[1]+c2_p[1]
				p = random()
				c1_p[1] = tot_w*p
				c2_p[1] = tot_w*(1-p)
				adj[c1][c1_r],adj[c2][c2_r] = c2_p, c1_p
	null_net = []
	for i in range(len(cons_sp)):
		for j in adj[i]:
			null_net.append([j[0],cons_sp[i],j[1]])
	return null_net


def mat_from_net(net):
	res = sorted(list(set([i[0] for i in net])))
	cons = sorted(list(set([i[1] for i in net])))
	mat = zeros([len(res),len(cons)])
	for i in net:
		rid = res.index(i[0])
		cid = cons.index(i[1])
		mat[rid][cid] = i[2]
	return mat



all_data = [i for i in csv.reader(open('no_dup.csv','r'))]

size_dict = dict([[i[3],float(i[4])] for i in all_data[1:]])
sites = sorted(list(set([i[0] for i in all_data[1:]])))

out = open('results.csv','w')
out.write('site,type,null,step,val\n')

for site in sites:
	net = []
	added = []
	for i in all_data:
		if i[0]==site and [i[5],i[3]] not in added:
			net.append([i[5],i[3],float(i[7])])
			added.append([i[5],i[3]])
	for rep in range(1000):
		nm = 0
		for net_ in [net.copy(),null_m(net.copy())]:
			for ext_type in ['top','bottom','both']:
				mat = mat_from_net(net_)
				res_rand = ext_w(mat.copy(),ext_type = ext_type)
				for j in res_rand:
					out.write(','.join(map(str,[site,ext_type,nm,j[0],j[1]]))+'\n')
			nm+=1
		print (rep)


out.close()


