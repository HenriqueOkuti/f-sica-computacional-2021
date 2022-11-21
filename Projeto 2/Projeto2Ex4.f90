program mmquadrado

! Projeto 2 - Cálculo numérico
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 somax, somay, somaxx, somaxy, delta, total
	real*8 somay1, somay2, somay3, somay4
	real*8 y1, y2, y3, y4, x1
	real*8 somaxy1, somaxy2, somaxy3, somaxy4
	integer	i,n, io
	real*8 angular, linear
	real*8 x(7), y(7)

	x = (/3,4,5,6,7,8,9/)
	y = (/9.2,10.5,14.8,15.6,19.6,20.2,23.2/)
	
	somax = 0.d0
	somay = 0.d0
	somaxx = 0.d0
	somaxy = 0.d0

	i = 1
	n = 7
	total = 7
	
	do while (i.LE.n)
		somax = somax + x(i)
		somay = somay + y(i)
		somaxx = somaxx + (x(i)*x(i))
		somaxy = somaxy + (x(i)*y(i))
		i = i+1
	enddo

	delta = (total*somaxx) - (somax*somax)
	angular = ((total*somaxy)-(somax*somay))/delta
	linear = ((somaxx*somay)-(somax*somaxy))/delta
	
	write(*,*)"Para os dados fornecidos no projeto:"
	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear
	
	! fort.41 -> logh,logdif_ff,logdif_ft,logdif_f3s,logdif_ff3s
	! fort.42 -> logh,logerro_trapezio
	! fort.43 -> logh,logerro_simpson
	! Fazer regressão para: logh x logY para cada um dos Y acima

	! Vamos calcular agora para o exercício 1:

	n = 0
		! Usar os comandos abaixo para ler o número de linhas
	open(51, file = 'fort.41')	! Abrimos o arquivo com os dados
	do	
		read(51,*,iostat=io)	! Lemos o arquivo com os dados
		if (io/=0) exit			! Condicional para término da leitura
		n = n + 1		! Contador da quantidade de números no arquivo
	end do					
	close(51)					
	
	i = 1
	somax = 0.d0
	somaxx = 0.d0
	somay1 = 0.d0
	somay2 = 0.d0
	somay3 = 0.d0
	somay4 = 0.d0
	somaxy1 = 0.d0
	somaxy2 = 0.d0
	somaxy3 = 0.d0
	somaxy4 = 0.d0
	
	write(*,*)""
	write(*,*)"Os coeficientes abaixo sao do exercicio 1"
	write(*,*)""
	
	open(51, file = 'fort.41')
	
	do while (i.LE.n)
		read(51,*)x1,y1,y2,y3,y4
		somax = somax + x1
		somay1 = somay1 + y1
		somay2 = somay2 + y2
		somay3 = somay3 + y3
		somay4 = somay4 + y4		
		somaxx = somaxx + (x1*x1)
		somaxy1 = somaxy1 + (x1*y1)
		somaxy2 = somaxy2 + (x1*y2)
		somaxy3 = somaxy3 + (x1*y3)
		somaxy4 = somaxy4 + (x1*y4)
		i = i+1
	enddo
	
	total = n
	delta = (total*somaxx) - (somax*somax)
	
	write(*,*)"Colunas 1 e 2 (x , f'f):"
	
	angular = ((total*somaxy1)-(somax*somay1))/delta
	linear = ((somaxx*somay1)-(somax*somaxy1))/delta

	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear
	
	write(*,*)"Colunas 1 e 3 (x , f't):"
	
	angular = ((total*somaxy2)-(somax*somay2))/delta
	linear = ((somaxx*somay2)-(somax*somaxy2))/delta

	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear
	
	write(*,*)"Colunas 1 e 4 (x , f'3s):"
	
	angular = ((total*somaxy3)-(somax*somay3))/delta
	linear = ((somaxx*somay3)-(somax*somaxy3))/delta

	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear

	write(*,*)"Colunas 1 e 5 (x , f''3s):"
	
	angular = ((total*somaxy4)-(somax*somay4))/delta
	linear = ((somaxx*somay4)-(somax*somaxy4))/delta

	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear

	! Vamos calcular agora para o exercício 2 (trapézio):

	write(*,*)""
	write(*,*)"Os coeficientes abaixo sao do exercicio 2 (trapezio)"
	write(*,*)""

	n = 0
	! Usar os comandos abaixo para ler o número de linhas
	open(52, file = 'fort.42')	! Abrimos o arquivo com os dados
	do	
		read(52,*,iostat=io)	! Lemos o arquivo com os dados
		if (io/=0) exit			! Condicional para término da leitura
		n = n + 1		! Contador da quantidade de números no arquivo
	end do					
	close(52)					

	open(52, file = 'fort.42')
	i = 1
	somax = 0.d0
	somaxx = 0.d0
	somay1 = 0.d0
	somaxy1 = 0.d0
		
	do while (i.LE.n)
		read(52,*)x1,y1
		somax = somax + x1
		somay1 = somay1 + y1
		somaxx = somaxx + (x1*x1)
		somaxy1 = somaxy1 + (x1*y1)
		i = i+1
	enddo

	total = n
	delta = (total*somaxx) - (somax*somax)
	
	write(*,*)"Colunas 1 e 2:"
	
	angular = ((total*somaxy1)-(somax*somay1))/delta
	linear = ((somaxx*somay1)-(somax*somaxy1))/delta

	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear

	! Vamos calcular agora para o exercício 2 (Simpson):

	write(*,*)""
	write(*,*)"Os coeficientes abaixo sao do exercicio 2 (Simpson)"
	write(*,*)""

	n = 0
	! Usar os comandos abaixo para ler o número de linhas
	open(53, file = 'fort.43')	! Abrimos o arquivo com os dados
	do	
		read(53,*,iostat=io)	! Lemos o arquivo com os dados
		if (io/=0) exit			! Condicional para término da leitura
		n = n + 1		! Contador da quantidade de números no arquivo
	end do					
	close(53)					

	open(53, file = 'fort.43')
	i = 1
	somax = 0.d0
	somaxx = 0.d0
	somay1 = 0.d0
	somaxy1 = 0.d0
		
	do while (i.LE.n)
		read(53,*)x1,y1
		somax = somax + x1
		somay1 = somay1 + y1
		somaxx = somaxx + (x1*x1)
		somaxy1 = somaxy1 + (x1*y1)
		i = i+1
	enddo

	total = n
	delta = (total*somaxx) - (somax*somax)
	
	write(*,*)"Colunas 1 e 2:"
	
	angular = ((total*somaxy1)-(somax*somay1))/delta
	linear = ((somaxx*somay1)-(somax*somaxy1))/delta

	write(*,*)"Coeficiente angular: ",angular
	write(*,*)"Coeficiente linear: ",linear


end program mmquadrado