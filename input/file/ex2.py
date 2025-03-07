S = float(input('Digite a distância percorrida em metros: '))
T = float(input('Digite o tempo gasto em segundos: '))

if S <= 0 or T <= 0:
  print(f'ERRO: Entrada inválida.')
else:
  v = S / T
  print(f'A velocidade média é {v:.2f}')