program orbitas_circular_eliptica

! Projeto 4 - Leis de Kepler e o problema de três corpos
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Declaração de variáveis

	real*8	G_Ms, pi
	real*8  x_velho, x_novo, k_1_x, k_2_x, k_3_x, k_4_x, soma_k_x
	real*8  v_x_velho, v_x_novo, k_1_vx, k_2_vx, k_3_vx, k_4_vx, soma_k_vx
	real*8  y_velho, y_novo, k_1_y, k_2_y, k_3_y, k_4_y, soma_k_y
	real*8  v_y_velho, v_y_novo, k_1_vy, k_2_vy, k_3_vy, k_4_vy, soma_k_vy
	real*8  r_velho, r_novo
	real*8  tempo_inicial, delta_tempo, tempo
	real*8	periodo, orbitas
	real*8  raiz_epsilon
	real*8  x_ref, y_ref
	real*8, dimension(8) :: vetor_eixo
	real*8, dimension(8) :: vetor_epsilon
	integer*8 pontos, i, flag

! Definições de constantes utilizadas

	pi = 4.d0*atan(1.d0)
	G_Ms = 4.d0*(pi**(2.d0))
	tempo_inicial = 0.d0
	vetor_eixo(1) = 0.39d0			! Mercúrio
	vetor_eixo(2) = 0.72d0			! Vênus
	vetor_eixo(3) = 1.0d0			! Terra
	vetor_eixo(4) = 1.52d0			! Marte
	vetor_eixo(5) = 5.20d0			! Júpiter
	vetor_eixo(6) = 9.24d0			! Saturno
	vetor_eixo(7) = 19.19d0			! Urano
	vetor_eixo(8) = 30.06d0			! Netuno
	vetor_epsilon(1) = 0.206d0		! Mercúrio
	vetor_epsilon(2) = 0.007d0		! Vênus
	vetor_epsilon(3) = 0.017d0		! Terra
	vetor_epsilon(4) = 0.093d0		! Marte
	vetor_epsilon(5) = 0.049d0		! Júpiter
	vetor_epsilon(6) = 0.057d0		! Saturno
	vetor_epsilon(7) = 0.046d0		! Urano
	vetor_epsilon(8) = 0.009d0		! Netuno
	
	!=========================
	!	QUESTÃO A e B		==
	!=========================
	
	open(10,file = 'questao_a_posicoes.dat')
	open(11,file = 'questao_a_velocidades.dat')
	open(12,file = 'questao_a_vetores.dat')
	
	do i = 1, 8, 1
	
	tempo = tempo_inicial
	delta_tempo = 0.1d0		! Caso eu acabe mudando o parâmetro: usei 0.002d0
	v_x_velho = 0.d0
	x_velho = vetor_eixo(i)
	y_velho = 0.d0
	r_velho = vetor_eixo(i)
	v_y_velho = dsqrt(G_Ms/r_velho)
	orbitas = 0.d0
	periodo = 0.d0
	flag = 0
	
	do pontos = 0 , 1000 , 1	! Caso eu acabe mudando o parâmetro: usei de 0 até 100000, variando de 1 em 1

		! RK4 - CALCULO COEFICIENTES P/ X:
		
		k_1_vx = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*x_velho	
		k_1_x = delta_tempo*v_x_velho
		
		k_2_vx = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*(x_velho + k_1_x/2.d0)
		k_2_x = delta_tempo*(v_x_velho + k_1_vx/2.d0)
	
		k_3_vx = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*(x_velho + k_2_x/2.d0)
		k_3_x = delta_tempo*(v_x_velho + k_2_vx/2.d0)
		
		k_4_vx = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*(x_velho + k_3_x)
		k_4_x = delta_tempo*(v_x_velho + k_3_vx)
		
		!APLICAÇÃO DO RK4 EM X:
		
		soma_k_x = (1.d0/6.d0)*(k_1_x + 2.d0*k_2_x + 2.d0*k_3_x + k_4_x)
		x_novo = x_velho + soma_k_x
		
		
		! RK4 - CALCULO COEFICIENTES P/ Y:
		
		k_1_vy = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*y_velho
		k_1_y = delta_tempo*v_y_velho
		
		k_2_vy = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*(y_velho + k_1_y/2.d0)
		k_2_y = delta_tempo*(v_y_velho + k_1_vy/2.d0)
		
		k_3_vy = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*(y_velho + k_2_y/2.d0)
		k_3_y = delta_tempo*(v_y_velho + k_2_vy/2.d0)
		
		k_4_vy = -delta_tempo*(G_Ms/(r_velho**(3.d0)))*(y_velho + k_3_y)
		k_4_y = delta_tempo*(v_y_velho + k_3_vy)

		!APLICAÇÃO DO RK4 EM Y:
		
		soma_k_y = (1.d0/6.d0)*(k_1_y + 2.d0*k_2_y + 2.d0*k_3_y + k_4_y)
		y_novo = y_velho + soma_k_y	
		
		!APLICAÇÃO DO RK4 EM V_X:
		
		soma_k_vx = (1.d0/6.d0)*(k_1_vx + 2.d0*k_2_vx + 2.d0*k_3_vx + k_4_vx)
		v_x_novo = v_x_velho + soma_k_vx	

		!APLICAÇÃO DO RK4 EM V_Y:
		
		soma_k_vy = (1.d0/6.d0)*(k_1_vy + 2.d0*k_2_vy + 2.d0*k_3_vy + k_4_vy)		
		v_y_novo = v_y_velho + soma_k_vy		

		
		r_novo = dsqrt(x_novo**(2.d0) + y_novo**(2.d0))
		
		if (flag.NE.1) then
		
			if ((x_novo.GE.(0.d0)).AND.(x_velho.LE.(0.d0))) then
	
				orbitas = 3.d0/4.d0
				periodo = tempo
				flag = 1
		
			end if
		
		end if
		
		write(10,*)x_velho, y_velho, tempo
		write(11,*)v_x_velho, v_y_velho, tempo
		write(12,*)(r_velho), (v_x_velho**(2.d0) + v_y_velho**(2.d0)), tempo
		
		x_velho = x_novo
		y_velho = y_novo
		v_x_velho = v_x_novo
		v_y_velho = v_y_novo
		r_velho = r_novo
		
		tempo = tempo + delta_tempo

	end do

	periodo = periodo/orbitas

	write(*,*)"semi-eixo maior: ",vetor_eixo(i)
	write(*,*)"Periodo: ",periodo
	write(*,*)"r_novo:",r_novo
	write(*,*)"Razao da lei de Kepler: T^2 / a^3",(periodo**(2.d0)/(vetor_eixo(i)**(3.d0)))
	write(*,*)""
	write(*,*)"Mudanca de planeta"
	write(*,*)""

	write(10,*)""
	write(10,*)""
	write(10,*)"	MUDANCA DE PLANETA	"
	write(10,*)""
	write(10,*)""
	
	write(11,*)""
	write(11,*)""
	write(11,*)"	MUDANCA DE PLANETA	"
	write(11,*)""
	write(11,*)""
	
	write(12,*)""
	write(12,*)""
	write(12,*)"	MUDANCA DE PLANETA	"
	write(12,*)""
	write(12,*)""

	end do
	
	!=========================
	!	QUESTÃO C			==
	!=========================

	write(*,*)""
	write(*,*)"Mudanca de exercicio, estamos agora verificando orbitas elipticas de Mercurio"
	write(*,*)""

	open(13,file = 'questao_elipse_posicoes.dat')
	open(14,file = 'questao_elipse_velocidades.dat')
	open(15,file = 'questao_elipse_vetores.dat')
	
	do i = 1, 8, 1	
	
	tempo = tempo_inicial
	delta_tempo = 0.005d0		! Caso eu acabe mudando o parâmetro: usei 0.005d0
	raiz_epsilon = dsqrt((1.d0 - vetor_epsilon(i))/(1.d0 + vetor_epsilon(i)))
	
	v_x_velho = 0.d0
	y_velho = 0.d0
	
	x_velho = (1.d0 + vetor_epsilon(i))*vetor_eixo(i)
	r_velho = dsqrt(x_velho**(2.d0))
	v_y_velho = dsqrt(G_Ms/vetor_eixo(i)) * raiz_epsilon

	orbitas = 0.d0
	periodo = 0.d0
	r_referencia = x_velho
	flag = 0
	
	x_ref = x_velho
	y_ref = y_velho
	
	do pontos = 0 , 25000 , 1	! Caso eu acabe mudando o parâmetro: usei de 0 até 25000, variando de 1 em 1
	
		! RK4 - CALCULO COEFICIENTES P/ X:
		
		k_1_vx = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*x_velho	
		k_1_x = delta_tempo*v_x_velho
		
		k_2_vx = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*(x_velho + k_1_x/2.d0)
		k_2_x = delta_tempo*(v_x_velho + k_1_vx/2.d0)
	
		k_3_vx = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*(x_velho + k_2_x/2.d0)
		k_3_x = delta_tempo*(v_x_velho + k_2_vx/2.d0)
		
		k_4_vx = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*(x_velho + k_3_x)
		k_4_x = delta_tempo*(v_x_velho + k_3_vx)
		
		!APLICAÇÃO DO RK4 EM X:
		
		soma_k_x = (1.d0/6.d0)*(k_1_x + 2.d0*k_2_x + 2.d0*k_3_x + k_4_x)
		x_novo = x_velho + soma_k_x
		
		! RK4 - CALCULO COEFICIENTES P/ Y:
		
		k_1_vy = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*y_velho
		k_1_y = delta_tempo*v_y_velho
		
		k_2_vy = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*(y_velho + k_1_y/2.d0)
		k_2_y = delta_tempo*(v_y_velho + k_1_vy/2.d0)
		
		k_3_vy = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*(y_velho + k_2_y/2.d0)
		k_3_y = delta_tempo*(v_y_velho + k_2_vy/2.d0)
		
		k_4_vy = (-1.d0)*delta_tempo*(G_Ms/(vetor_eixo(i)**(3.d0)))*(y_velho + k_3_y)
		k_4_y = delta_tempo*(v_y_velho + k_3_vy)

		!APLICAÇÃO DO RK4 EM Y:
		
		soma_k_y = (1.d0/6.d0)*(k_1_y + 2.d0*k_2_y + 2.d0*k_3_y + k_4_y)
		y_novo = y_velho + soma_k_y
		
		!APLICAÇÃO DO RK4 EM V_X:
		
		soma_k_vx = (1.d0/6.d0)*(k_1_vx + 2.d0*k_2_vx + 2.d0*k_3_vx + k_4_vx)
		v_x_novo = v_x_velho + soma_k_vx

		!APLICAÇÃO DO RK4 EM V_Y:
		
		soma_k_vy = (1.d0/6.d0)*(k_1_vy + 2.d0*k_2_vy + 2.d0*k_3_vy + k_4_vy)		
		v_y_novo = v_y_velho + soma_k_vy
		
		r_novo = dsqrt(x_novo**(2.d0) + y_novo**(2.d0))

		if (flag.NE.1) then
		
			if ((x_novo.GE.(0.d0)).AND.(x_velho.LE.(0.d0))) then
				orbitas = 3.d0/4.d0
				periodo = tempo
				flag = flag + 1
		
			end if
		
		end if
		
		write(13,*)x_velho, y_velho, tempo
		write(14,*)v_x_velho, v_y_velho, tempo
		write(15,*)(r_velho), (v_x_velho**(2.d0) + v_y_velho**(2.d0)), r_velho*(v_x_velho**(2.d0) + v_y_velho**(2.d0)), tempo
		
		x_velho = x_novo
		y_velho = y_novo
		v_x_velho = v_x_novo
		v_y_velho = v_y_novo
		r_velho = r_novo
		
		tempo = tempo + delta_tempo

	end do

	periodo = periodo / orbitas
	
	write(*,*)"Periodo:	",periodo
	write(*,*)"Excentricidade:	",vetor_epsilon(i)
	write(*,*)"semi-eixo maior: ",vetor_eixo(i)
	write(*,*)"Razao da lei de Kepler: T^2 / a^3",(periodo**(2.d0)/(vetor_eixo(i)**(3.d0)))
	write(*,*)""
	write(*,*)"Mudanca de planeta"
	write(*,*)""

	write(13,*)""
	write(13,*)""
	write(13,*)"	MUDANCA DE PLANETA	"
	write(13,*)""
	write(13,*)""

	write(14,*)""
	write(14,*)""
	write(14,*)"	MUDANCA DE PLANETA	"
	write(14,*)""
	write(14,*)""

	write(15,*)""
	write(15,*)""
	write(15,*)"	MUDANCA DE PLANETA	"
	write(15,*)""
	write(15,*)""

	end do

end program orbitas_circular_eliptica