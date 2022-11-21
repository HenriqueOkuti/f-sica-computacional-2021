program oscilador_harmonico_amortecido

! Projeto 3 - Movimento realista
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Exercício 4: Oscilador Harmônico amortecido
!	## m*x'' + lambda*x' + kx = F(t) ; m = k = 1 ; omega_0 = sqrt(k/m) = 1 s^-1 ; T = 2*pi s
!	TAREFAS:
!		a) Resolva pelo método de RK4 para F(t) = 0 nas situações
!			++ gamma < omega_0 (amortecimento fraco)
!			++ gamma > omega_0 (amortecimento forte)
!			++ gamma = omega_0 (amortecimento crítico)
!			++ Note que: gamma = lambda/2m
!			++ Use ao menos 3 condições iniciais diferentes e tempo t>5T ; use h = T/50
!			++ Grafique: x(t) por t ; x'(t) por t ; E(t) por t ; m*x'(t) por x(t)
!			++ Use que E(t) = m*x'(t)^2 /2 + k*x(t)^2 /2 == Energia mecânica do sistema na ausência de dissipação
!		b) Repita o procedimento para F(t) = cos(omega_1 *t) no caso de amortecimento fraco
!			++ Grafique x(t) por t ; mesma condição inicial ; varie omega_1 e gamma 
!			++ Faça um gráfico da amplitude do movimento para tempos longos (t>> 1/gamma) como função de omega_1 para diferentes valores de gamma





end program oscilador_harmonico_amortecido