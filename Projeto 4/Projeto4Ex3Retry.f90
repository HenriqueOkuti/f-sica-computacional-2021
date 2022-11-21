program coreografia_celeste

! Projeto 4 - Leis de Kepler e o problema de três corpos
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Declaração de variáveis


	real*8 pi, G_M
	integer*8 pontos, i

	pi = 4.d0*atan(1.d0)
	G_M = (-1.d0)*4.d0*(pi**(2.d0))

	
	!=========================
	!	QUESTÃO A 		======
	!=========================
	
	! partícula[4] -> {x , y , vx , vy}
	p1 = (1.d0, 0.d0, 0.d0, 0.759836d0)
	p2 = (-0.5d0, 0.866025d0, -0.658037d0, -0.379918d0)
	p3 = (-0.5d0, -0.866025d0, 0.658037d0, -0.379918d0)
	
	rpAB[1] = dsqrt((p1[1] - p2[1])**(2.d0) + (p1[1] - p2[2])**(2.d0))	! Separação 1-2
	rpAB[2] = dsqrt((p1[1] - p2[3])**(2.d0) + (p1[1] - p2[3])**(2.d0))	! Separação 1-3
	rpAB[3] = dsqrt((p1[2] - p2[3])**(2.d0) + (p1[2] - p2[3])**(2.d0))	! Separação 2-3
	
	! Laço que calcula o cubo das separações
	do i = 1 , 3 , 1
	
		r3pAB[i] = rpAB[i]**(2.d0)
	
	end do
	
	deltaT = 0.002d0
	tempo = 0.d0
	
	! Laço que faz a progressão temporal
	do i = 1 , 5000 , 1
	
	! Calculo dos coeficientes do RK4

		k1vx[1] = deltaT*G_M( ((p1[1]-p2[1])/rpAB[1]) + ((p1[1]-p3[1])/rpAB[2]) )
		k1vy[1] = deltaT*G_M( ((p1[2]-p2[2])/rpAB[1]) + ((p1[2]-p3[2])/rpAB[2]) )
		k1vx[2] = deltaT*G_M( ((p2[1]-p1[1])/rpAB[1]) + ((p2[1]-p3[1])/rpAB[3]) )
		k1vy[2] = deltaT*G_M( ((p2[2]-p1[2])/rpAB[1]) + ((p2[2]-p3[2])/rpAB[3]) )
		k1vx[3] = deltaT*G_M( ((p3[1]-p1[1])/rpAB[2]) + ((p3[1]-p2[1])/rpAB[3]) )
		k1vy[3] = deltaT*G_M( ((p3[2]-p1[2])/rpAB[2]) + ((p3[2]-p2[2])/rpAB[3]) )
	
		k1x[1] = deltaT*p1[3]
		k1y[1] = deltaT*p1[4]
		k1x[2] = deltaT*p2[3]
		k1y[2] = deltaT*p2[4]
		k1x[3] = deltaT*p3[3]
		k1y[3] = deltaT*p3[4]
	
		k2vx[1] = deltaT*G_M( ((p1[1]-p2[1] + k1x[1])/rpAB[1]) + ((p1[1]-p3[1] + k1y[1])/rpAB[2]) )
		k2vy[1] = deltaT*G_M( ((p1[2]-p2[2] + k1x[1])/rpAB[1]) + ((p1[2]-p3[2] + k1y[1])/rpAB[2]) )
		k2vx[2] = deltaT*G_M( ((p2[1]-p1[1] + k1x[2])/rpAB[1]) + ((p2[1]-p3[1] + k1y[2])/rpAB[3]) )
		k2vy[2] = deltaT*G_M( ((p2[2]-p1[2] + k1x[2])/rpAB[1]) + ((p2[2]-p3[2] + k1y[2])/rpAB[3]) )
		k2vx[3] = deltaT*G_M( ((p3[1]-p1[1] + k1x[3])/rpAB[2]) + ((p3[1]-p2[1] + k1y[3])/rpAB[3]) )
		k2vy[3] = deltaT*G_M( ((p3[2]-p1[2] + k1x[3])/rpAB[2]) + ((p3[2]-p2[2] + k1y[3])/rpAB[3]) )
	
		k2x[1] = deltaT*p1[3]
		k2y[1] = deltaT*p1[4]
		k2x[2] = deltaT*p2[3]
		k2y[2] = deltaT*p2[4]
		k2x[3] = deltaT*p3[3]
		k2y[3] = deltaT*p3[4]
	

	tempo = tempo + deltaT

	end do

end program coreografia_celeste