program integracaonumerica

! Integral Monte Carlo 
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

	real*8 integral,integral_mc,funcao_montecarlo
	real*8 x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,soma
	integer i,total
	
! Começamos definindo o valor exato da integral

	integral = 155.d0/6.d0
	write(*,*)"A integral deve valer: ",integral

! Crio arquivos para armazenar os dados
	open(10,file='integral.dat') 
	open(11,file='erro_integral.dat') 
	write(10,*)"A integral deve valer: ",integral
	write(10,*)"			N	","	integral monte carlo	","	razao	"

	integral_mc = 0.d0
	total = 2
	do while (total.LE.(2**24))
		funcao_montecarlo = 0.d0
		i = 0
		do while (i.NE.total)
			call random_number(x1)
			call random_number(x2)
			call random_number(x3)
			call random_number(x4)
			call random_number(x5)
			call random_number(x6)
			call random_number(x7)
			call random_number(x8)
			call random_number(x9)
			call random_number(x10)
			soma = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
			funcao_montecarlo = ((soma)**2.d0) + funcao_montecarlo
			i = i + 1
		enddo
		integral_mc = funcao_montecarlo / total
		write(10,*)total,integral_mc,(abs(1.d0-(integral_mc/integral)))
		write(11,*)(abs(1.d0-(integral_mc/integral))),(1/sqrt(total*1.d0))
		total = total*2
		enddo
end program integracaonumerica