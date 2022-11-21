program pendulo_simples

! Projeto 3 - Movimento realista
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Exercício 3: Pêndulo simples


	real*8 frequencia_nova, frequencia_velha
	real*8 theta_nova, theta_velha, theta_max, theta_inicial, delta_theta, theta, theta_final
	real*8 deltaT, tempo_inicial, tempo_final, tempo
	real*8 aux, f_a, f_b, i_s, aux2, aux3, funcao, raiz_tudo
	real*8 energia, periodo, oscilacao, tempo_osci
	real*8 k1_theta, k2_theta, k3_theta, k4_theta, somak_theta
	real*8 k1_omega, k2_omega, k3_omega, k4_omega, somak_omega
	real*8 omega_velha, omega_inicial, omega_nova
	real*8 massa, length, gravity, periodo_teorico, pi, g_por_l, l_por_g, m_g_l, m_l_2, h
	real*8 xi, funcao_montecarlo, mediafuncao, somaquadrado, desvio_padrao, nr, n, i, mediasomaquadrado
	
	
	pi = 4*atan(1.d0)
	massa = 1.d0
	length = 1.d0
	gravity = 10.d0	
	periodo_teorico = 2*pi*sqrt(length/gravity)
	deltaT = 0.01
	theta_inicial = pi/6.d0
	tempo_inicial = 0.d0
	tempo_final = 20.d0
	theta_max = pi/2.d0
	delta_theta = pi/6.d0
	g_por_l = gravity / length
	l_por_g = 1.d0/g_por_l
	m_l_2 = massa*length*length
	m_g_l = massa*gravity*length
	
	
	!===================================================================
	!======		EXERCICIO A		========================================
	!===================================================================
	
	! Iremos implementar o método de Euler para resolver o pêndulo simples
	
	theta = theta_inicial
	
	do while (theta.LE.theta_max)
	theta_velha = theta
	tempo = tempo_inicial
	energia = 0.d0
	frequencia_velha = 0.d0
	
	write(*,*)"Estamos em = ",(180*theta_velha/pi),"graus"
	write(30,*)"Para theta inicial = ",(180*theta_velha/pi)
	write(30,*)""
	write(30,*)""
	
	write(40,*)"Para theta inicial = ",(180*theta_velha/pi)
	write(40,*)""
	write(40,*)""
	
	
		do while (tempo.LE.tempo_final)
	
			frequencia_nova = frequencia_velha - g_por_l*sin(theta_velha)*deltaT
			theta_nova = theta_velha + frequencia_velha*deltaT
			energia = (0.5*m_l_2)*(frequencia_velha**(2.d0)) + ((m_g_l)*(1.d0-cos(theta_velha)))
			
			write(30,*)frequencia_velha, theta_velha, tempo
			write(40,*)tempo, energia
			
			frequencia_velha = frequencia_nova
			theta_velha = theta_nova
			tempo = tempo + deltaT
		enddo
	write(30,*)""
	write(30,*)""
	write(30,*)"		MUDANCA DE ANGULO INICIAL		"
	write(30,*)""
	write(30,*)""
	
	write(40,*)""
	write(40,*)""
	write(40,*)"		MUDANCA DE ANGULO INICIAL		"
	write(40,*)""
	write(40,*)""
	
	theta = theta + delta_theta
	enddo
	
	
	!=======================================================================
	!======		EXERCICIO A*		========================================
	!=======================================================================
	
	! Nesta seção estamos aplicando uma correção no exercício acima
	! A correção se baseia no método de Euler-Cromer
	
	theta = theta_inicial
	
	do while (theta.LE.theta_max)
	theta_velha = theta
	tempo = tempo_inicial
	energia = 0.d0
	frequencia_velha = 0.d0
	
	write(*,*)"Estamos em = ",(180*theta_velha/pi),"graus"
	write(31,*)"Para theta inicial = ",(180*theta_velha/pi)
	write(31,*)""
	write(31,*)""
	
	write(41,*)"Para theta inicial = ",(180*theta_velha/pi)
	write(41,*)""
	write(41,*)""
	
		do while (tempo.LE.tempo_final)
	
			frequencia_nova = frequencia_velha - g_por_l*sin(theta_velha)*deltaT
			theta_nova = theta_velha + frequencia_nova*deltaT
			energia = (0.5*m_l_2)*(frequencia_velha**(2.d0)) + ((m_g_l)*(1.d0-cos(theta_velha)))
			
			write(31,*)frequencia_velha, theta_velha, tempo
			write(41,*)tempo, energia
			
			frequencia_velha = frequencia_nova
			theta_velha = theta_nova
			tempo = tempo + deltaT
		enddo
		
	write(31,*)""
	write(31,*)""
	write(31,*)"		MUDANCA DE ANGULO INICIAL		"
	write(31,*)""
	write(31,*)""
	
	write(41,*)""
	write(41,*)""
	write(41,*)"		MUDANCA DE ANGULO INICIAL		"
	write(41,*)""
	write(41,*)""
	
	
	theta = theta + delta_theta
	enddo
	
	
	!===================================================================
	!======		EXERCICIO B/C		========================================
	!===================================================================
	
	! Agora iremos implementar o método de RK4
	! Simultaneamente iremos calcular o período do pêndulo simples

	h = deltaT
	theta = 0.d0
	theta_max = pi/2.d0
	
	! Iniciamos o Loop para variarmos theta
	do while(theta.LE.theta_max)
		theta_velha = theta
		omega_velha = 0.d0
			
		write(*,*)"Estamos em = ",(180*theta_velha/pi),"graus"
		write(51,*)"Para theta inicial = ",(180*theta_velha/pi)
		write(51,*)""
		write(51,*)""
		
		write(61,*)"Para theta inicial = ",(180*theta_velha/pi)
		write(61,*)""
		write(61,*)""
		
		tempo = tempo_inicial
		
		! Iniciamos o Loop para variarmos o tempo
		do while(tempo.LE.tempo_final)

			! APLICAÇÃO RK4 theta:
			k1_theta = h*omega_velha
			k2_theta = h*(omega_velha + k1_omega/2.d0)
			k3_theta = h*(omega_velha + k2_omega/2.d0)
			k4_theta = h*(omega_velha + k3_omega)
			somak_theta = k1_theta + (2.d0*k2_theta) + (2.d0*k3_theta) + k4_theta
			theta_nova = theta_velha + (1.d0/6.d0)*somak_theta

			! APLICAÇÃO RK4 omega: 
			k1_omega = (-1.d0)*h*g_por_l*sin(theta_velha)
			k2_omega = (-1.d0)*h*g_por_l*sin(theta_velha + k1_theta/2.d0)
			k3_omega = (-1.d0)*h*g_por_l*sin(theta_velha + k2_theta/2.d0)
			k4_omega = (-1.d0)*h*g_por_l*sin(theta_velha + k3_theta)
			somak_omega = k1_omega + 2.d0*k2_omega + 2.d0*k3_omega + k4_omega
			omega_nova = omega_velha + (1.d0/6.d0)*somak_omega 
			
			energia = (0.5*m_l_2)*(omega_velha**(2.d0)) + ((m_g_l)*(1.d0-cos(theta_velha)))
			
			write(51,*)tempo, theta_velha, omega_velha, (theta_velha*180.d0/pi)
			write(61,*)tempo, energia
			
			omega_velha = omega_nova
			theta_velha = theta_nova
			
			tempo = tempo + deltaT	
		enddo
		
		write(51,*)""
		write(51,*)""
		write(51,*)"		MUDANCA DE ANGULO INICIAL		"
		write(51,*)""
		write(51,*)""
	
		write(61,*)""
		write(61,*)""
		write(61,*)"		MUDANCA DE ANGULO INICIAL		"
		write(61,*)""
		write(61,*)""
		
		theta = theta + delta_theta
				
	enddo
	

	!===================================================================
	!======		EXERCICIO C		========================================
	!===================================================================	

	
	! CÁLCULO DO PERÍODO: variar theta_inicial = 0º até theta_inicial = 90º de 1 grau em 1 grau
	theta_inicial = 0.d0
	theta_final = theta_max
	delta_theta = pi/180.d0
	h = deltaT
		
	! Iniciamos o Loop para variarmos o ângulo inicial
	do while (theta_inicial.LE.theta_final)
		aux = 0.d0
		tempo_osci  = 0.d0
		theta_velha = theta_inicial
		omega_velha = 0.d0
		tempo = 0.d0
		oscilacao = 0.d0
		! Iniciamos o Loop para variarmos o tempo
		do while(tempo.LE.tempo_final)
		
			! APLICAÇÃO RK4 theta:
			k1_theta = h * omega_velha
			k2_theta = h * (omega_velha + k1_omega/2.d0)
			k3_theta = h * (omega_velha + k2_omega/2.d0)
			k4_theta = h * (omega_velha + k3_omega)
			somak_theta = k1_theta + 2.d0*k2_theta + 2.d0*k3_theta + k4_theta
			theta_nova = theta_velha + (1.d0/6.d0)*somak_theta
			
			! APLICAÇÃO RK4 omega: 
			k1_omega = (-1.d0)*h*g_por_l*sin(theta_velha)
			k2_omega = (-1.d0)*h*g_por_l*sin(theta_velha + k1_theta/2.d0)
			k3_omega = (-1.d0)*h*g_por_l*sin(theta_velha + k2_theta/2.d0)
			k4_omega = (-1.d0)*h*g_por_l*sin(theta_velha + k3_theta)
			somak_omega = k1_omega + 2.d0*k2_omega + 2.d0*k3_omega + k4_omega
			omega_nova = omega_velha + (1.d0/6.d0)*somak_omega 
			
			aux = aux + 1.d0
			
			if ((theta_velha.LT.theta_inicial).AND.(theta_nova.GT.theta_inicial)) then	! Condicional para ocorrer oscilacao
	 			oscilacao = oscilacao + 1.d0
				tempo_osci = deltaT*aux
			end if		
			omega_velha = omega_nova
			theta_velha = theta_nova
			tempo = tempo + deltaT	
		enddo

		periodo = tempo / oscilacao		! Período = tempo dividido pelas oscilações
		
		!periodo = tempo_osci / oscilacao
		write(71,*)theta_inicial*180/pi,tempo_osci, oscilacao, periodo
		theta_inicial = theta_inicial + delta_theta
	enddo


	!===================================================================
	!======		EXERCICIO C*		====================================
	!===================================================================
	
	! Neste último item iremos calcular a integral eliptica para o cálculo do período
	
	! Realizar loop para theta_inicial = 0º até theta_inicial = 90º
	
	aux = 4.d0 * sqrt(l_por_g)
	h = (pi/2.d0)*(1.d0/8192.d0)		! Incrementos a serem feitos entre trapezios
	aux3 = (pi/2.d0) * 1.d0/h			! Numero total de trapezios a serem calculados, considerando a integral de x = 0 até x = pi/2
	theta_inicial = 0.d0
	theta_final = pi/2.d0
	
	write(75,*)"	theta_inicial	", "	periodo	"
	do while(theta_inicial.LE.theta_final)
		aux2 = 1.d0
		theta = theta_inicial
		i_s = 0.d0
	! Por definição: T = 4*SQRT(L/G)* INT_0^{PI/2} 1/sqrt(1 - sin^2 (theta_inicial/2) * sin^2 (u)) du
	
		!========================================
		! Método de Simpson		=================
		!========================================
		
		! Começamos calculando o valor nas extremidades
		f_a = 1.d0/(dsqrt(1.d0 - ((sin(theta/2.d0)**(2.d0))*(sin(0.d0)**(2.d0)))))
		f_b = 1.d0/(dsqrt(1.d0 - ((sin(theta/2.d0)**(2.d0))*(sin(pi/2.d0)**(2.d0)))))
		
		
		! Então fazemos um laço para os termos cruzados:
		do while (aux2.LT.aux3)
	
			if(dmod(aux2,2.d0).EQ.0.d0) then						! Condicional para o caso par
				raiz_tudo = sqrt(1.d0 - ((sin(theta/2.d0)*sin(theta/2.d0))*(sin(h*aux2)*sin(h*aux2))))
				funcao = 2.d0/raiz_tudo								! Calculamos a função no trapézio indicado
				i_s = i_s + funcao				! A integral é a soma de todos os trapézios
				aux2 = aux2 + 1					! Devemos incrementar o contador para passar para o trapézio seguinte
			else													! Condicional para o caso ímpar
				raiz_tudo = sqrt(1.d0 - ((sin(theta/2.d0)*sin(theta/2.d0))*(sin(h*aux2)*sin(h*aux2))))
				funcao = 4.d0/raiz_tudo								! Calculamos a função no trapézio indicado
				i_s = i_s + funcao				! A integral é a soma de todos os trapézios
				aux2 = aux2 + 1					! Devemos incrementar o contador para passar para o trapézio seguinte			
			endif
			

		enddo
		
		! Então finalmente terminamos a integral:
		i_s = (h/3.d0)*(i_s + f_a + f_b)
		! Substituimos o resultado da integral na expressão do período
		periodo = aux*i_s
		! Salvamos os dados no arquivo fort.75
		write(75,*)(theta_inicial*180/pi), periodo
		
		theta_inicial = theta_inicial + delta_theta
	enddo
	
	!========================================
	! Método do Trapezio		=============
	!========================================
	
	aux = 4.d0 * sqrt(l_por_g)
	h = (pi/2.d0)*(1.d0/8192.d0)		! Incrementos a serem feitos entre trapezios
	aux3 = (pi/2.d0) * 1.d0/h			! Numero total de trapezios a serem calculados, considerando a integral de x = 0 até x = pi/2
	theta_inicial = 0.d0
	theta_final = pi/2.d0

	write(76,*)"	theta_inicial	", "	periodo	"
	
	! Realizamos um laço indo de theta_inicial = 0 até theta_inicial = pi/2
	do while(theta_inicial.LE.theta_final)
		aux2 = 1.d0
		theta = theta_inicial
		i_s = 0.d0

		! Começamos calculando o valor nas extremidades
		f_a = 1.d0/(dsqrt(1.d0 - ((sin(theta/2.d0)**(2.d0))*(sin(0.d0)**(2.d0)))))
		f_b = 1.d0/(dsqrt(1.d0 - ((sin(theta/2.d0)**(2.d0))*(sin(pi/2.d0)**(2.d0)))))
		
		
		! Então fazemos um laço para os termos cruzados:
		do while (aux2.LT.aux3)
				
				! A variavel raiz_tudo calcula a raiz da expressão presente na integral eliptica
				raiz_tudo = dsqrt(1.d0 - ((sin(theta/2.d0)**(2.d0))*(sin(h*aux2)**(2.d0))))
				! A variavel funcao termina de calcular, tendo em vista que ainda falta inverter
				funcao = 1.d0/raiz_tudo								! Calculamos a função no trapézio indicado
				! Este arquivo fort.99 é gerado para confirmar o intervalo de integração, é algo completamente opcional mas precisei utilizar para confirmar os parâmetros
				write(99,*)h, aux2, aux3, h*aux2, funcao
				i_s = i_s + funcao				! A integral é a soma de todos os trapézios
				aux2 = aux2 + 1					! Devemos incrementar o contador para passar para o trapézio seguinte
			
		enddo
		
		! Então finalmente terminamos a integral:
		i_s = (h)*(i_s + 0.5*f_a + 0.5*f_b)
		! Novamente substituimos os dados e salvamos no arquivo adequado, agora em fort.76
		periodo = aux*i_s
		write(76,*)(theta_inicial*180/pi), periodo
		
		! Prosseguimos incrementando theta_inicial até chegarmos na condição de saída
		theta_inicial = theta_inicial + delta_theta
		enddo

end program pendulo_simples