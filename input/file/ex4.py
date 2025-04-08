import math

def delta(a, b, c):
    return b**2 - 4*a*c

def bhaskara(a, b, c):
    d = delta(a, b, c)
    x1 = (-b + math.sqrt(d)) / 2*a
    x2 = (-b - math.sqrt(d)) / 2*a
    return x1, x2

a = float(input("Digite o valor de a: "))
b = float(input("Digite o valor de b: "))
c = float(input("Digite o valor de c: "))

x1, x2 = bhaskara(a, b, c)

print(f"{a}x^2 + {b}x + {c}")
print(f"Raiz 1: {x1}")
print(f"Raiz 2: {x2}")