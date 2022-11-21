program lancamento_de_projeteis

! Projeto 3 - Movimento realista
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Declaração de variáveis

		real*8 posicao_x_velha, posicao_x_nova, velocidade_x_velha, velocidade_x_nova
		real*8 coeficiente_x, coeficiente_y, raiz_quadrados
		real*8 posicao_y_velha, posicao_y_nova, velocidade_y_velha, velocidade_y_nova
		real*8 theta_inicial, theta_final, delta_theta, theta_temp
		real*8 deltaT, tempo_temp
		real*8 grav, gamma2, v_inicial, coef_b, temperatura, coef_alpha, coef_gamma
		integer*8 i, n

	theta_inicial = 0.d0				! rad
	theta_final = (4*atan(1.d0))/2.d0	! rad 
	delta_theta = theta_final / 6.d0	! angulos calculados
	grav = 9.8d0						! m/s^2
	v_inicial = 700.d0					! m/s
	gamma2 = 0.00004				! m^-1
	deltaT = 0.01					! s
	coef_b = 6.5e-3					! K/m
	temperatura = 300.d0			! K
	coef_alpha = 2.5					! adimensional

	write(*,*)"Exercicio a)"
	write(*,*)""

	! ============================	
	! Questão A ==================
	! ============================

	theta_temp = 0.d0

	do while (theta_temp.LE.theta_final)
		
		posicao_x_velha = 0.d0
		posicao_y_velha = 0.d0
		velocidade_x_velha = v_inicial*cos(theta_temp)
		velocidade_y_velha = v_inicial*sin(theta_temp)
		tempo_temp = 0.d0
		
		write(20,*)""
		write(20,*)"angulo: ",theta_temp
		write(20,*)"em graus: ", theta_temp*90.d0/theta_final
		write(20,*)""
		
		write(20,*)"	posicao_x_velha	","	posicao_y_velha	","	velocidade_x_velha	","	velocidade_y_velha	","	tempo_temp"
		do while (posicao_y_velha.GE.(0.d0))
			
			posicao_x_nova = posicao_x_velha + velocidade_x_velha*deltaT
			posicao_y_nova = posicao_y_velha + velocidade_y_velha*deltaT
			
			velocidade_x_nova = velocidade_x_velha
			velocidade_y_nova = velocidade_y_velha - grav*deltaT
			
			tempo_temp = tempo_temp + deltaT
			
			write(20,*)posicao_x_velha,posicao_y_velha,velocidade_x_velha,velocidade_y_velha,tempo_temp
			
			velocidade_x_velha = velocidade_x_nova
			velocidade_y_velha = velocidade_y_nova
			posicao_x_velha = posicao_x_nova
			posicao_y_velha = posicao_y_nova
		
		enddo
		
		write(20,*)""
		write(20,*)"		MUDANCA DE ANGULO		"
		write(20,*)""

		write(*,*)"Angulo graus: ", theta_temp*90.d0/theta_final
		write(*,*)"Tempo: ",tempo_temp
		
		theta_temp = theta_temp + delta_theta

	enddo


	! ============================	
	! Questão B ==================
	! ============================

	! Primeiro vamos fazer o mesmo procedimento que a questão anterior, com a correção necessária	
	write(*,*)""
	write(*,*)"Exercicio b) - trajetoria"
	write(*,*)""

	theta_temp = 0.d0

	do while (theta_temp.LE.theta_final)
		
		posicao_x_velha = 0.d0
		posicao_y_velha = 0.d0
		velocidade_x_velha = v_inicial*cos(theta_temp)
		velocidade_y_velha = v_inicial*sin(theta_temp)
		tempo_temp = 0.d0
		
		write(21,*)""
		write(21,*)"angulo: ",theta_temp
		write(21,*)"em graus: ", theta_temp*90.d0/theta_final
		write(21,*)""
		
		write(21,*)"	posicao_x_velha	","	posicao_y_velha	","	velocidade_x_velha	","	velocidade_y_velha	","	tempo_temp"
		do while (posicao_y_velha.GE.(0.d0))
			
			posicao_x_nova = posicao_x_velha + velocidade_x_velha*deltaT
			posicao_y_nova = posicao_y_velha + velocidade_y_velha*deltaT
			
			raiz_quadrados = sqrt( (velocidade_x_velha**2.d0) + (velocidade_y_velha**2.d0) )
			
			coeficiente_x = raiz_quadrados*velocidade_x_velha*deltaT
			coeficiente_y = raiz_quadrados*velocidade_y_velha*deltaT
			
			velocidade_x_nova = velocidade_x_velha - gamma2*coeficiente_x
			velocidade_y_nova = velocidade_y_velha - grav*deltaT - gamma2*coeficiente_y
			
			tempo_temp = tempo_temp + deltaT
			
			write(21,*)posicao_x_velha,posicao_y_velha,velocidade_x_velha,velocidade_y_velha,tempo_temp
			
			velocidade_x_velha = velocidade_x_nova
			velocidade_y_velha = velocidade_y_nova
			posicao_x_velha = posicao_x_nova
			posicao_y_velha = posicao_y_nova
		
		enddo
		
		write(21,*)""
		write(21,*)"		MUDANCA DE ANGULO		"
		write(21,*)""

		write(*,*)"Angulo graus: ", theta_temp*90.d0/theta_final
		write(*,*)"Tempo: ",tempo_temp
		
		theta_temp = theta_temp + delta_theta

	enddo
	
	! ====================================================================================
	! Agora vamos desenvolver a trajetória máxima em função do ângulo para obter theta_max
	! ====================================================================================
	
	write(*,*)""
	write(*,*)"Exercicio b) - Alcance x Angulo"
	write(*,*)""

	delta_theta = (8*atan(1.d0))/360
	theta_temp = 0.d0

	write(22,*)"	posicao_x_velha	","	theta_temp	","	ang graus	","	tempo_temp"


	do while (theta_temp.LE.theta_final)
		
		posicao_x_velha = 0.d0
		posicao_y_velha = 0.d0
		velocidade_x_velha = v_inicial*cos(theta_temp)
		velocidade_y_velha = v_inicial*sin(theta_temp)
		tempo_temp = 0.d0
		
		do while (posicao_y_velha.GE.(0.d0))
			
			posicao_x_nova = posicao_x_velha + velocidade_x_velha*deltaT
			posicao_y_nova = posicao_y_velha + velocidade_y_velha*deltaT
			
			raiz_quadrados = sqrt( (velocidade_x_velha**2.d0) + (velocidade_y_velha**2.d0) )
			
			coeficiente_x = raiz_quadrados*velocidade_x_velha*deltaT
			coeficiente_y = raiz_quadrados*velocidade_y_velha*deltaT
			
			velocidade_x_nova = velocidade_x_velha - gamma2*coeficiente_x
			velocidade_y_nova = velocidade_y_velha - grav*deltaT - gamma2*coeficiente_y
			
			tempo_temp = tempo_temp + deltaT
			velocidade_x_velha = velocidade_x_nova
			velocidade_y_velha = velocidade_y_nova
			posicao_x_velha = posicao_x_nova
			posicao_y_velha = posicao_y_nova
		
		enddo
		write(22,*)posicao_x_velha,theta_temp, theta_temp*90.d0/theta_final, tempo_temp		
		

		write(*,*)"Angulo graus: ", theta_temp*90.d0/theta_final
		write(*,*)"Tempo: ",tempo_temp
		
		theta_temp = theta_temp + delta_theta

	enddo

	! ============================	
	! Questão C ==================
	! ============================
	
	! Primeiro vamos fazer o mesmo procedimento que a questão anterior, com a correção necessária	
	
	write(*,*)""
	write(*,*)"Exercicio C) - trajetoria"
	write(*,*)""

	delta_theta = theta_final / 6.d0
	theta_temp = 0.d0

	do while (theta_temp.LE.theta_final)
		
		posicao_x_velha = 0.d0
		posicao_y_velha = 0.d0
		velocidade_x_velha = v_inicial*cos(theta_temp)
		velocidade_y_velha = v_inicial*sin(theta_temp)
		tempo_temp = 0.d0
		
		write(23,*)""
		write(23,*)"angulo: ",theta_temp
		write(23,*)"em graus: ", theta_temp*90.d0/theta_final
		write(23,*)""
		
		write(23,*)"	posicao_x_velha	","	posicao_y_velha	","	velocidade_x_velha	","	velocidade_y_velha	","	tempo_temp"
		do while (posicao_y_velha.GE.(0.d0))
			
			posicao_x_nova = posicao_x_velha + velocidade_x_velha*deltaT
			posicao_y_nova = posicao_y_velha + velocidade_y_velha*deltaT
			
			raiz_quadrados = sqrt( (velocidade_x_velha**2.d0) + (velocidade_y_velha**2.d0) )
			
			coeficiente_x = raiz_quadrados*velocidade_x_velha*deltaT
			coeficiente_y = raiz_quadrados*velocidade_y_velha*deltaT
			
			coef_gamma = gamma2*((1.d0 - (coef_b*posicao_y_velha/temperatura))**(coef_alpha))
			
			velocidade_x_nova = velocidade_x_velha - coef_gamma*coeficiente_x
			velocidade_y_nova = velocidade_y_velha - grav*deltaT - coef_gamma*coeficiente_y
			
			tempo_temp = tempo_temp + deltaT
			
			write(23,*)posicao_x_velha,posicao_y_velha,velocidade_x_velha,velocidade_y_velha,tempo_temp
			
			velocidade_x_velha = velocidade_x_nova
			velocidade_y_velha = velocidade_y_nova
			posicao_x_velha = posicao_x_nova
			posicao_y_velha = posicao_y_nova
		
		enddo
		
		write(23,*)""
		write(23,*)"		MUDANCA DE ANGULO		"
		write(23,*)""

		write(*,*)"Angulo graus: ", theta_temp*90.d0/theta_final
		write(*,*)"Tempo: ",tempo_temp
		
		theta_temp = theta_temp + delta_theta

	enddo
	
	! =========================================================================================
	! Agora vamos desenvolver a trajetória máxima em função do ângulo para obter theta_max ====
	! =========================================================================================
	
	write(*,*)""
	write(*,*)"Exercicio C) - Alcance x Angulo"
	write(*,*)""

	delta_theta = (8*atan(1.d0))/360
	theta_temp = 0.d0

	write(24,*)"	posicao_x_velha	","	theta_temp	","	ang graus	","	tempo_temp"


	do while (theta_temp.LE.theta_final)
		
		posicao_x_velha = 0.d0
		posicao_y_velha = 0.d0
		velocidade_x_velha = v_inicial*cos(theta_temp)
		velocidade_y_velha = v_inicial*sin(theta_temp)
		tempo_temp = 0.d0
		
		do while (posicao_y_velha.GE.(0.d0))
			
			posicao_x_nova = posicao_x_velha + velocidade_x_velha*deltaT
			posicao_y_nova = posicao_y_velha + velocidade_y_velha*deltaT
			
			raiz_quadrados = sqrt( (velocidade_x_velha**2.d0) + (velocidade_y_velha**2.d0) )
			
			coeficiente_x = raiz_quadrados*velocidade_x_velha*deltaT
			coeficiente_y = raiz_quadrados*velocidade_y_velha*deltaT
			
			coef_gamma = gamma2*((1.d0 - (coef_b*posicao_y_velha/temperatura))**(coef_alpha))
			
			velocidade_x_nova = velocidade_x_velha - coef_gamma*coeficiente_x
			velocidade_y_nova = velocidade_y_velha - grav*deltaT - coef_gamma*coeficiente_y
			
			tempo_temp = tempo_temp + deltaT
			velocidade_x_velha = velocidade_x_nova
			velocidade_y_velha = velocidade_y_nova
			posicao_x_velha = posicao_x_nova
			posicao_y_velha = posicao_y_nova
		
		enddo
		write(24,*)posicao_x_velha,theta_temp, theta_temp*90.d0/theta_final, tempo_temp		
		

		write(*,*)"Angulo graus: ", theta_temp*90.d0/theta_final
		write(*,*)"Tempo: ",tempo_temp
		
		theta_temp = theta_temp + delta_theta

	enddo


end program lancamento_de_projeteis