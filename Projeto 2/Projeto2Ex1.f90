program derivadanumerica

! Projeto 2 - Cálculo numérico
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 funcao1, ff, ft, f3s, ff3s, ffrente, ftras
	real*8 ffuncao1, fffuncao1
	real*8 dif_ff, dif_ft, dif_f3s, dif_ff3s
	real*8 logdif_ff, logdif_ft, logdif_f3s, logdif_ff3s, logh
	real*8 h(16)
	integer i,n
	
	h(1) = 5E-1
	h(2) = 1E-1
	h(3) = 5E-2
	h(4) = 1E-2
	h(5) = 5E-3
	h(6) = 1E-3
	h(7) = 5E-4
	h(8) = 1E-4
	h(9) = 5E-5
	h(10) = 1E-5
	h(11) = 5E-6
	h(12) = 1E-6
	h(13) = 5E-7
	h(14) = 1E-7
	h(15) = 5E-8
	h(16) = 1E-8
	
	!Vamos calcular valores relevantes para termos de referência
	
	funcao1 = exp(1.d0/2.d0) * sin(1.d0/3.d0)											! f(1)
	ffuncao1 = (1.d0/6.d0)*exp(1.d0/2.d0)*((3*sin(1.d0/3.d0) + 2*cos(1.d0/3.d0))) 		! f'(1)
	fffuncao1 = (1.d0/36.d0)*exp(1.d0/2.d0)*((5*sin(1.d0/3.d0) + 12*cos(1.d0/3.d0)))	! f''(1)
	i = 1
	n = 16
	write(*,*)"f(1) = ",funcao1
	write(*,*)"f'(1) = ",ffuncao1
	write(*,*)"f''(1) = ",fffuncao1

	! Exercício a)
	
	!Vamos gerar um arquivo com os dados relevantes
	write(10,*)"	h(i)	","	ff	","	ft	","	f3s	","	ff3s	"
	!Vamos fazer o laço para calcular as derivadas em relação à cada h(i)
	do while(i.LE.n)
		ffrente = (exp((1.d0 + h(i))/2.d0) * sin((h(i)+1.d0)/3.d0))
		ftras 	= (exp((1.d0 - h(i))/2.d0) * sin((1.d0 - h(i))/3.d0))
		ff		= ( ffrente - funcao1 ) / h(i)
		ft		= ( funcao1 - ftras )/h(i)
		f3s		= ( ffrente - ftras )/(2.d0*h(i))
		ff3s 	= ( ffrente - (funcao1+funcao1) + ftras ) /(h(i)**2.d0)
		write(10,*)h(i),ff,ft,f3s,ff3s
		i = i+1
		
	end do	

	!Agora vamos gerar um arquivo com as diferenças entre os valores calculados para as derivadas
	!em relação ao valor exato da função no ponto

	write(11,*)"	h(i)	","	ff - f'	","	ft - f'	","	f3s - f'	","	ff3s - f''	"
	
	!Agora vamos gerar um arquivo com o logaritmo das diferenças entre os valores calculados para
	!as derivadas em relação ao valor exato da função no ponto
	
	write(12,*)"	h(i)	","log(h(i))","	log(ff - f')	","	log(ft - f')	","	log(f3s - f')	","	log(ff3s - f'')	"
	
	!Vamos fazer o laço para calcular as derivadas em relação à cada h(i)
	!Aproveitamos também para calcular as diferenças entre as derivadas e a função no ponto
	!Assim como também geramos os logaritmos das diferenças entre as funções no ponto 
	
	i = 1
	n = 16	
	do while(i.LE.n)
		ffrente = (exp((1.d0 + h(i))/2.d0) * sin((h(i)+1.d0)/3.d0))
		ftras = (exp((1.d0 - h(i))/2.d0) * sin((1.d0 - h(i))/3.d0))
		
		ff = ( ffrente - funcao1 ) / h(i)
		dif_ff = ff - ffuncao1
		logdif_ff = dlog10(abs(dif_ff))
		
		ft = ( funcao1 - ftras )/h(i)
		dif_ft = ft - ffuncao1
		logdif_ft = dlog10(abs(dif_ft))
		
		f3s = ( ffrente - ftras )/(2.d0*h(i))
		dif_f3s = f3s - ffuncao1	
		logdif_f3s = dlog10(abs(dif_f3s))		
		
		ff3s = ( ffrente - (funcao1+funcao1) + ftras ) /(h(i)**2)
		dif_ff3s = ff3s - fffuncao1		
		logdif_ff3s = dlog10(abs(dif_ff3s))		
		
		logh = dlog10(h(i))
		
		write(11,*)h(i),dif_ff,dif_ft,dif_f3s,dif_ff3s
		write(12,*)h(i),logh,logdif_ff,logdif_ft,logdif_f3s,logdif_ff3s
		write(41,*)logh,logdif_ff,logdif_ft,logdif_f3s,logdif_ff3s	! Este arquivo servirá para o exercício 4 também
		
		i = i+1
		
	end do		
	
	!Para ter de fácil referência:
	!fort.10 : Derivadas no ponto
	!fort.11 : Diferenças das derivadas no ponto
	!fort.12 : Logaritmo do valor absoluto das diferenças no ponto
	!fort.14 : Logaritmo de h e logaritmo do valor absoluto das diferenças no ponto 

end program derivadanumerica