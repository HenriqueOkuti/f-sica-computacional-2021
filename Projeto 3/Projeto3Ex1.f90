program efeito_resistivo_do_ar

! Projeto 3 - Movimento realista
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Declaração de variáveis

	real*8 velocidade_velha, velocidade_nova, velocidade_inicial, potencia, deltaT, massa
	real*8 rho, area_fixa, area, tempo_terminal, velocidade_terminal_analitica
	real*8 coeficiente1, coeficiente2, tempo_max, tempo_atual
	real*8 integral_velocidade, velocidade, lixo, velocidade_terminal, erro, precisao
	integer contador, i, iteracoes, flag

	massa = 70.d0
	potencia = 400.d0
	rho = 1.3
	area_fixa = 0.333
	precisao = 1E-2
	velocidade_terminal_analitica = 12.2716
	
	! ============================	
	! Questão A ==================
	! ============================

	contador = 0
	deltaT = 0.1
	tempo_max = 300.d0
	tempo_atual = 0.d0
	velocidade_inicial = 4.d0
	velocidade_velha = velocidade_inicial
	flag = 0
	velocidade_terminal = 0.d0
	iteracoes = 0
	
	open(10,file = "dados_velocidade.dat")
	do while (tempo_atual.LE.(tempo_max+deltaT))
		coeficiente1 = (potencia / (massa * velocidade_velha))*deltaT
		velocidade_nova = velocidade_velha + coeficiente1
		write(10,*)tempo_atual, velocidade_velha
		erro = abs((velocidade_nova - velocidade_terminal_analitica))
		if (flag.NE.1) then
			if (erro.LE.precisao) then
				velocidade_terminal = velocidade_nova
				tempo_terminal = tempo_atual
				iteracoes = contador
				flag = 1
			endif
		endif
		velocidade_velha = velocidade_nova
		tempo_atual = tempo_atual + deltaT
		contador = contador+1
	enddo
	close(10)
	
	open(11,file = 'dados_velocidade.dat')
	
	read(11,*)lixo,velocidade
	integral_velocidade = 0.5 * deltaT * velocidade

	i = 0
	do while(i.LT.(contador-2))
		read(11,*)lixo,velocidade
		integral_velocidade = deltaT * velocidade + integral_velocidade
		i = i+1
	enddo
	
	read(11,*)lixo,velocidade
	integral_velocidade = 0.5 * deltaT * velocidade + integral_velocidade
	write(*,*)"O resultado da integral sem resistencia do ar:"
	write(*,*)integral_velocidade
	
	write(*,*)""
	write(*,*)"Fim da questao A"
	write(*,*)""
	! ============================	
	! Questão B ==================
	! ============================

	contador = 0
	deltaT = 0.1
	tempo_max = 300.d0
	tempo_atual = 0.d0
	velocidade_inicial = 4.d0
	velocidade_velha = velocidade_inicial
	flag = 0

	open(12,file = "dados_velocidade2.dat")
	do while (tempo_atual.LE.tempo_max)
		coeficiente1 = (potencia / (massa * velocidade_velha))*deltaT
		coeficiente2 = ( (rho * area_fixa * velocidade_velha**2.d0)/(2.d0*massa) * deltaT )
		velocidade_nova = velocidade_velha + coeficiente1 - coeficiente2
		write(12,*)tempo_atual, velocidade_velha
		erro = abs((velocidade_nova - velocidade_terminal_analitica))
		if (flag.NE.1) then
			if (erro.LE.precisao) then
				velocidade_terminal = velocidade_nova
				tempo_terminal = tempo_atual
				iteracoes = contador
				flag = 1
			endif
		endif
		velocidade_velha = velocidade_nova
		tempo_atual = tempo_atual + deltaT
		contador = contador+1
	enddo
	close(12)

	open(13,file = 'dados_velocidade2.dat')
	read(13,*)lixo,velocidade
	integral_velocidade = 0.5 * deltaT * velocidade
	i = 0
	
	do while(i.LT.(contador-2))
		read(13,*)lixo,velocidade
		integral_velocidade = deltaT * velocidade + integral_velocidade
		i = i+1
	enddo
	
	read(13,*)lixo,velocidade
	integral_velocidade = 0.5 * deltaT * velocidade + integral_velocidade
	write(*,*)"O resultado da integral com resistencia do ar:"
	write(*,*)integral_velocidade
	
	if (velocidade_terminal.GT.4.d0) then
	write(*,*)"Velocidade terminal:"
	write(*,*)velocidade_terminal
	write(*,*)"Tempo necessario:"
	write(*,*)tempo_terminal
	endif


	! ============================	
	! Questão C ==================
	! ============================

	contador = 0
	deltaT = 0.1
	tempo_max = 300.d0
	tempo_atual = 0.d0
	velocidade_inicial = 4.d0
	area = 1.d0-(7*0.1)
	
	velocidade_velha = velocidade_inicial

		open(14,file = 'dados_velocidade_area3.dat')
		do while (tempo_atual.LE.tempo_max)
			coeficiente1 = (potencia / (massa * velocidade_velha))*deltaT
			coeficiente2 = ( (rho * area * velocidade_velha**2.d0)/(2.d0*massa) * deltaT )
			velocidade_nova = velocidade_velha + coeficiente1 - coeficiente2
			write(14,*)tempo_atual, velocidade_velha
			velocidade_velha = velocidade_nova
			tempo_atual = tempo_atual + deltaT
			contador = contador+1
		enddo
		close(14)

end program efeito_resistivo_do_ar