N = 200

def tak (x, y, z):
    if (y >= x):
        return z
    return tak(
        tak (x - 1, y, z),
        tak (y - 1, z, x),
        tak (z - 1, x, y))

def run_tak (k):
    if (k == 0):
        return True
    tak(18, 12, 6)
    run_tak(k - 1)

print(tak(18, 12, 6) == 7)
run_tak(N)
