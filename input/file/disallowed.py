def inputVetor():
    entrada = input("Informe as metas dos estados: ")
    return list(map(int, entrada.split(',')))

def inputMatriz():
    entrada = input("Informe o plantio de arvores: ")
    linhas = entrada.split(';')
    matriz = [list(map(int, linha.split(','))) for linha in linhas]
    return matriz

def main():
    print("Ministerio do Meio Ambiente")
    metas = inputVetor()
    plantio = inputMatriz()

    num_estados = len(metas)
    totais_plantio = [sum(linha[i] for linha in plantio) for i in range(num_estados)]

    for i in range(num_estados):
        if totais_plantio[i] < metas[i]:
            print(f"Estado {i+1}, meta = {metas[i]}, plantio = {totais_plantio[i]}")

if __name__ == "__main__":
    main()