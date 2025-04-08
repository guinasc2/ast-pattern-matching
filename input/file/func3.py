import random
import math
from collections import deque

def calcular_fatorial(n):
    if n == 0 or n == 1:
        return 1
    else:
        return n * calcular_fatorial(n - 1)

def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def busca_binaria(lista, alvo, baixo=0, alto=None):
    if alto is None:
        alto = len(lista) - 1
    
    if baixo > alto:
        return -1
    
    meio = (baixo + alto) // 2
    if lista[meio] == alvo:
        return meio
    elif lista[meio] > alvo:
        return busca_binaria(lista, alvo, baixo, meio - 1)
    else:
        return busca_binaria(lista, alvo, meio + 1, alto)

def quicksort(lista):
    if len(lista) <= 1:
        return lista
    pivo = lista[0]
    menores = [x for x in lista[1:] if x <= pivo]
    maiores = [x for x in lista[1:] if x > pivo]
    return quicksort(menores) + [pivo] + quicksort(maiores)

def calcular_raiz_quadrada(x):
    return math.sqrt(x)

def gerar_lista_aleatoria(tamanho, limite):
    return [random.randint(1, limite) for _ in range(tamanho)]

def somar_lista(lista):
    if not lista:
        return 0
    else:
        return lista[0] + somar_lista(lista[1:])

def largura_primeira(grafo, inicio):
    visitados = set()
    fila = deque([inicio])
    ordem_visita = []

    while fila:
        vertice = fila.popleft()
        if vertice not in visitados:
            visitados.add(vertice)
            ordem_visita.append(vertice)
            fila.extend(grafo[vertice])
    
    return ordem_visita

def profundidade_primeira(grafo, vertice, visitados=None):
    if visitados is None:
        visitados = set()

    visitados.add(vertice)
    ordem_visita = [vertice]

    for vizinho in grafo[vertice]:
        if vizinho not in visitados:
            ordem_visita += profundidade_primeira(grafo, vizinho, visitados)
    
    return ordem_visita

def funcao_principal():
    n = random.randint(1, 10)
    lista_aleatoria = gerar_lista_aleatoria(10, 100)
    lista_ordenada = quicksort(lista_aleatoria)
    
    print(f"Fatorial de {n}: {calcular_fatorial(n)}")
    print(f"Fibonacci de {n}: {fibonacci(n)}")
    print(f"Soma da lista {lista_aleatoria}: {somar_lista(lista_aleatoria)}")
    print(f"Raiz quadrada de {n}: {calcular_raiz_quadrada(n)}")
    print(f"Lista aleatória ordenada: {lista_ordenada}")
    
    alvo = random.choice(lista_ordenada)
    resultado_busca = busca_binaria(lista_ordenada, alvo)
    print(f"Resultado da busca binária pelo valor {alvo}: {resultado_busca}")
    
    grafo = {
        'A': ['B', 'C'],
        'B': ['A', 'D', 'E'],
        'C': ['A', 'F'],
        'D': ['B'],
        'E': ['B', 'F'],
        'F': ['C', 'E']
    }
    
    print(f"Ordem de visita em largura a partir de 'A': {largura_primeira(grafo, 'A')}")
    print(f"Ordem de visita em profundidade a partir de 'A': {profundidade_primeira(grafo, 'A')}")
