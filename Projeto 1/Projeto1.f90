! Projeto 1 - Introdução à Programação
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br



! Exercício 1: Bhaskara
!	TAREFAS:
!		Escreva: ax^2 + bx + c = 0 ; a,b,c reais
!		Encontre quais são as raízes reais e quantas são
! 		Rode o Código para 5 exemplos e imprima os resultados



! Exercício 2: Fatoriais e aproximação de Stirling
!	TAREFAS:
!		(a) Escreva um programa que imprima em um arquivo todos os fatoriais para n=1 até n=20
!		(b) Escreva um programa que imprima em um arquivo o logaritmo de todos os fatoriais entre n=2 e n=30
!		(c) Compare com a aproximação de Stirling
!				Stirling: ln (n!) = S = n*ln(n) - n + (1/2)*ln(2*pi*n)
!		(d) Imprima uma tabela com quatro colunas: n, ln(n!), S, (ln(n!)-S)/(ln(n!)

! Exercício 3: Função Gamma
!	TAREFAS:
!		(a) Mostre que para x = n natural a relação de recorrência leva à n!= gamma(n+1)
!		(b) Implemente a fórmula de Lanezos para a função gamma e calcule n! para 5 inteiros entre n=5 e n=20 
!	 	(c) Compare com o resultado do exercício anteterior: Tabela com: n, n!, gamma
!		(d) Verifique a precisão da aproximação e verifique se depende do n escolhido
!		(e) Calcule Gamma(3/2) e Gamma(5/2)


! Exercício 4: Série de Taylor
!	TAREFAS:
!		(a) Calcule o valor de sin(x), x dentro do intervalor [0,2pi] com precisão de 10^-6
!		(b) Compare a expressão analítica e seu valor exato
!		(c) Utilize ao menos 5 valores de x distribuídos ao longo do intervalo indicado e, identifique a ordem da expansão para que a precisão seja alcançada em cada um dos casos
!		(d) Expresse a resposta em forma de uma tabela

! Exercício 5: Valores médios e desvio padrão
!	TAREFAS:
!	Utilize o arquivo indicado para realizar os cálculos
!		(a) Média aritmética: <x> = sum_(i=1)^(N) x_i / N
!		(b) Desvio padrão: sigma = sqrt( (sum_(i=1)^(N) (x_i - <x>)^2)/(N-1) )
!		(c) Mostre que o desvio padrão por ser tomado como: sigma = sqrt (<x^2> - <x>^2) se N>>1
!		(d) Discuta qual é mais fácil de ser implementada numéricamente


! Exercício 6: Números aleatórios e amostragem
!	TAREFAS:
!	Utilize a função call Random_Number(r) para gerar um número entre 0<=r<1
!		(a) Gere uma sequência de números aleatórios r_N de tamanho N_seq e faça média e desvio padrão para diversos tamanhos de N_seq. Para determinar N_seq veja o exercício 1
!	Utilize uma tabela para N<= 10^8 para mostrar os resultados. Compare com valores esperados de <r>=0,5 e sigma=0,288675
!		(b) Estime pi através de um círculo unitário x_i^2 + y_i^2 <= 1 ; onde x_i e y_i são números aleatórios. Se os pontos respeitarem a condição faça N_(dentro) = N_(dentro)+1
!	pi será estimado através de: pi = 4* (N_(dentro) / N_(total) )
!	O valor de pi deve ser feito com 4 casas decimais. Verifique o valor de N_total para essa precisão e interprete geométricamente
!		(c) Adapte o algoritmo anterior para calcular o volume de uma esfera de raio unitário para dimensões 3,4 e 5
!	Compare com o valor exato: V_d = pi^(d/2) * R^d / Gamma(d/2 +1) 

! Exercício 7: Método da potência para o cálculo do autovalor/autovetor dominante
!	TAREFAS:
!		(a) Explique o algoritmo por meio de um fluxograma
!		(b) Implemente o algoritmo, respeitando uma certa tolerância epsilon para a convergência do autovalor dominante, com a condição de um número máximo de iterações k_max (maior prioridade que a tolerância)
!		(c) Discuta seu palpite para o valor inicial do vetor x\vec
!		(d) Aplique o algoritmo para as matrizes Hermitianas A_1, A_2 e A_3 fornecidas 
!		(e) Verifique as respostas e discuta elas, veja se a precisão para os autovalores é a mesma que para os autovetores para um dado valor de iteração k

