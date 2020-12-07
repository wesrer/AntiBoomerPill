def listn(n)
    if n == 0
        return []
    end
    (listn(n - 1)).append(n)
end

def cdr x
  x.drop(1)
end

def mas (x, y, z)
    if (not (shorter y, x))
        z
    else
        val_1 = mas (cdr x), y, z
        val_2 = mas (cdr y), z, x
        val_3 = mas (cdr z), x, y
        mas val_1, val_2, val_3
    end
end

def shorter (x,y)
    if x.empty?
        not (y.empty?)
    elsif y.empty?
        false
    else
        one = cdr x
        two = cdr y
        shorter one, two
    end
end

    l18 = listn 18
    l12 = listn 12
    l6 = listn 6
    
hello = mas l18.reverse(), l12.reverse(), l6.reverse()
    
puts hello
