N = 32000

def derivaux(a):
    return list("/", deriv(a), a)
    
def deriv(a):
    if a.isalpha():
        if (a == "x"):
            return 1
        else: 
            return 0
    elif isinstance(a, list):
        if (a[0] == "+"):
            return list("+") + map(deriv, a[1:])
        elif (a[0] == "-"):
            return list("-") + map(deriv, a[1:])
        elif (a[0] == "*"):
            return list("*") + map(deriv, a[1:])
        elif (a[0] == "/"):
            list_1 = list("/", deriv(a[1]), a[2])
            inner_inner_list = list("*", a[2], a[2], deriv(a[2]))
            list_2 = list("/", a[2], inner_inner_list)
            return list("-", list_1, list_2)
        else:
            return ("error")

def benchmark(start=0, end):
    while (start < end):
        deriv("(+ (* 3 x x) (* a x x) (* b x) 5)")
        start += 1

benchmark(0, N)