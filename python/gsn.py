import gsnext

"""Given a list of lists, generate all permutations
ie, [[a], [b,c]] generates [[a,b], [a,c]]"""
def permutations(z):
    if len(z) == 1:
        return map(lambda x: [x], z[0])    
    subperms = permutations(z[:-1])
    out = []
    for e in z[-1]:
        for l in subperms:
            t=list(l)
            t.append(e)
            out.append(t)
    return out
