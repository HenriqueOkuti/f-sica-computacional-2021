program coreografia_celeste

! Projeto 4 - Leis de Kepler e o problema de três corpos
! Nome: Henrique Krastins Okuti
! Contato: henrique.okuti@usp.br

	implicit none

! Declaração de variáveis

	real*8 x1_old, x1_new, y1_old, y1_new, vx1_old, vx1_new, vy1_old, vy1_new, rx1
	real*8 k1x1, k2x1, k3x1, k4x1, somakx1, k1vx1, k2vx1, k3vx1, k4vx1, somakvx1
	real*8 k1y1, k2y1, k3y1, k4y1, somaky1, k1vy1, k2vy1, k3vy1, k4vy1, somakvy1
	real*8 x2_old, x2_new, y2_old, y2_new, vx2_old, vx2_new, vy2_old, vy2_new, rx2
	real*8 k1x2, k2x2, k3x2, k4x2, somakx2, k1vx2, k2vx2, k3vx2, k4vx2, somakvx2
	real*8 k1y2, k2y2, k3y2, k4y2, somaky2, k1vy2, k2vy2, k3vy2, k4vy2, somakvy2
	real*8 x3_old, x3_new, y3_old, y3_new, vx3_old, vx3_new, vy3_old, vy3_new, rx3
	real*8 k1x3, k2x3, k3x3, k4x3, somakx3, k1vx3, k2vx3, k3vx3, k4vx3, somakvx3
	real*8 k1y3, k2y3, k3y3, k4y3, somaky3, k1vy3, k2vy3, k3vy3, k4vy3, somakvy3
	real*8 x4_old, x4_new, y4_old, y4_new, vx4_old, vx4_new, vy4_old, vy4_new, rx4
	real*8 k1x4, k2x4, k3x4, k4x4, somakx4, k1vx4, k2vx4, k3vx4, k4vx4, somakvx4
	real*8 k1y4, k2y4, k3y4, k4y4, somaky4, k1vy4, k2vy4, k3vy4, k4vy4, somakvy4
	real*8 rp12, rp13, rp14, rp23, rp24, rp34, rp12aux, rp13aux, rp23aux, r3p12, r3p13, r3p14, r3p23, r3p24, r3p34
	real*8 tempo, deltaT
	real*8 pi, G_M, delta, G_M1, G_M2, G_M3, delta1
	integer*8 pontos
	real*8 k1vx12, k1vx13, k1vx21, k1vx23, k1vx31, k1vx32
	real*8 k2vx12, k2vx13, k2vx21, k2vx23, k2vx31, k2vx32
	real*8 k3vx12, k3vx13, k3vx21, k3vx23, k3vx31, k3vx32
	real*8 k4vx12, k4vx13, k4vx21, k4vx23, k4vx31, k4vx32
	real*8 k1vy12, k1vy13, k1vy21, k1vy23, k1vy31, k1vy32
	real*8 k2vy12, k2vy13, k2vy21, k2vy23, k2vy31, k2vy32
	real*8 k3vy12, k3vy13, k3vy21, k3vy23, k3vy31, k3vy32
	real*8 k4vy12, k4vy13, k4vy21, k4vy23, k4vy31, k4vy32
	real*8 k1x12, k1x13, k1x21, k1x23, k1x31, k1x32
	real*8 k2x12, k2x13, k2x21, k2x23, k2x31, k2x32
	real*8 k3x12, k3x13, k3x21, k3x23, k3x31, k3x32
	real*8 k4x12, k4x13, k4x21, k4x23, k4x31, k4x32
	real*8 k1y12, k1y13, k1y21, k1y23, k1y31, k1y32
	real*8 k2y12, k2y13, k2y21, k2y23, k2y31, k2y32
	real*8 k3y12, k3y13, k3y21, k3y23, k3y31, k3y32
	real*8 k4y12, k4y13, k4y21, k4y23, k4y31, k4y32	
	real*8 k1x12_2, k1x13_2, k1x14_2, k1x23_2, k1x24_2, k1x34_2, k1y12_2, k1y13_2, k1y14_2, k1y23_2, k1y24_2, k1y34_2
	real*8 k2x12_2, k2x13_2, k2x14_2, k2x23_2, k2x24_2, k2x34_2, k2y12_2, k2y13_2, k2y14_2, k2y23_2, k2y24_2, k2y34_2	
	real*8 k3x12_2, k3x13_2, k3x14_2, k3x23_2, k3x24_2, k3x34_2, k3y12_2, k3y13_2, k3y14_2, k3y23_2, k3y24_2, k3y34_2
	real*8 k4x12_2, k4x13_2, k4x14_2, k4x23_2, k4x24_2, k4x34_2, k4y12_2, k4y13_2, k4y14_2, k4y23_2, k4y24_2, k4y34_2
	real*8 aux1, aux2, aux3

	! Constantes úteis
	pi = 4.d0*atan(1.d0)
	
	!=========================
	!	QUESTÃO A 		======
	!=========================
	
	x1_old = 1.0d0
	y1_old = 0.d0
	vx1_old = 0.d0
	vy1_old = 0.759836d0

	x2_old = -0.5d0
	y2_old = 0.866025d0
	vx2_old = -0.658037d0
	vy2_old = -0.379918d0
	
	x3_old = -0.5d0
	y3_old = -0.866025d0
	vx3_old = 0.658037d0
	vy3_old = -0.379918d0
	
	rp12 = dsqrt( (x1_old - x2_old)**(2.d0) + (y1_old - y2_old)**(2.d0) )
	rp13 = dsqrt( (x1_old - x3_old)**(2.d0) + (y1_old - y3_old)**(2.d0) )
	rp23 = dsqrt( (x2_old - x3_old)**(2.d0) + (y2_old - y3_old)**(2.d0) )	

	
	r3p12 = rp12**(3.d0)
	r3p13 = rp13**(3.d0)
	r3p23 = rp23**(3.d0)
	
	rp12aux = r3p12
	rp13aux = r3p13
	rp23aux = r3p23
	
	delta = 0.0d0	! delta = 0.d0 , 0.01d0 , 0.05d0 ou 0.1d0
	delta1 = 1.d0 + delta	! Não utilizado neste exercício
	
	!G_M = (-1.d0)*4.d0*(pi**(2.d0))	! G_M modificado 4.d0*pi^2 -> 1.d0
	G_M = -1.d0
	
	write(*,*)rp12 , r3p12
	write(*,*)rp13 , r3p13
	write(*,*)rp23 , r3p23

	deltaT = 0.02d0	! Caso eu mude, utilizei: 0.0002d0
	tempo = 0.d0

	do pontos = 1 , 5 , 1	! Caso eu mude, utilizei: pontos = 1, 50000, 1 <- Deve fornecer 10 anos para deltaT = 0.0002d0
	
		write(34,*)x1_old, y1_old, tempo, x2_old, y2_old, tempo, x3_old, y3_old, tempo
		write(35,*)vx1_old, vy1_old, tempo, vx2_old, vy2_old, tempo, vx3_old, vy3_old, tempo
		write(36,*)tempo, rp12, rp13, rp23

		!================================================================================================
		!================================================================================================
		! RK4 - CALCULO COEFICIENTES P/ {VX1, VX2 , VX3 , X1 , X2 , X3, VY1 , VY2 , VY3 , Y1 , Y2 , Y3 }:
		
		k1vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old)/r3p12) + ((x1_old - x3_old)/r3p13) )
		k1vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old)/r3p12) + ((x2_old - x3_old)/r3p23) )
		k1vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old)/r3p13) + ((x3_old - x2_old)/r3p23) )
		
		k1vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old)/r3p12) + ((y1_old - y3_old)/r3p13) )
		k1vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old)/r3p12) + ((y2_old - y3_old)/r3p23) )
		k1vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old)/r3p13) + ((y3_old - y2_old)/r3p23) )

		k1x1 = deltaT*vx1_old
		k1x2 = deltaT*vx2_old
		k1x3 = deltaT*vx3_old
		
		k1y1 = deltaT*vy1_old
		k1y2 = deltaT*vy2_old
		k1y3 = deltaT*vy3_old

		k1x12_2 = (k1x1+k1x2)/2.d0
		k1x13_2 = (k1x1+k1x3)/2.d0
		k1x23_2 = (k1x2+k1x3)/2.d0
		
		k1y12_2 = (k1y1+k1y2)/2.d0
		k1y13_2 = (k1y1+k1y3)/2.d0
		k1y23_2 = (k1y2+k1y3)/2.d0
		
		rp12 = dsqrt( (x1_old - x2_old + k1x12_2)**(2.d0) + (y1_old - y2_old + k1x12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k1x13_2)**(2.d0) + (y1_old - y3_old + k1x13_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k1x23_2)**(2.d0) + (y2_old - y3_old + k1x23_2)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		k2vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k1x12_2)/r3p12) + ((x1_old - x3_old + k1x13_2)/r3p13) )
		k2vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k1x12_2)/r3p12) + ((x2_old - x3_old + k1x23_2)/r3p23) )
		k2vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k1x13_2)/r3p13) + ((x3_old - x2_old + k1x23_2)/r3p23) )
		
		k2vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k1y12_2)/r3p12) + ((y1_old - y3_old + k1y13_2)/r3p13) )
		k2vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k1y12_2)/r3p12) + ((y2_old - y3_old + k1y23_2)/r3p23) )
		k2vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k1y13_2)/r3p13) + ((y3_old - y2_old + k1y23_2)/r3p23) )

		k2x1 = deltaT*(vx1_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0)
		k2x2 = deltaT*(vx2_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0)
		k2x3 = deltaT*(vx3_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0)
		
		k2y1 = deltaT*(vy1_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0)
		k2y2 = deltaT*(vy2_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0)
		k2y3 = deltaT*(vy3_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0)
		
		k2x12_2 = (k2x1+k2x2)/2.d0
		k2x13_2 = (k2x1+k2x3)/2.d0
		k2x23_2 = (k2x2+k2x3)/2.d0
		
		k2y12_2 = (k2y1+k2y2)/2.d0
		k2y13_2 = (k2y1+k2y3)/2.d0
		k2y23_2 = (k2y2+k2y3)/2.d0
		
		rp12 = dsqrt( (x1_old - x2_old + k2x12_2)**(2.d0) + (y1_old - y2_old + k2y12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k2x13_2)**(2.d0) + (y1_old - y3_old + k2y13_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k2x23_2)**(2.d0) + (y2_old - y3_old + k2y23_2)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		k3vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k2x12_2)/r3p12) + ((x1_old - x3_old + k2x13_2)/r3p13) )
		k3vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k2x12_2)/r3p12) + ((x2_old - x3_old + k2x23_2)/r3p23) )
		k3vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k2x13_2)/r3p13) + ((x3_old - x2_old + k2x23_2)/r3p23) )
		
		k3vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k2y12_2)/r3p12) + ((y1_old - y3_old + k2y13_2)/r3p13) )
		k3vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k2y12_2)/r3p12) + ((y2_old - y3_old + k2y23_2)/r3p23) )
		k3vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k2y13_2)/r3p13) + ((y3_old - y2_old + k2y23_2)/r3p23) )

		k3x1 = deltaT*(vx1_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0)
		k3x2 = deltaT*(vx2_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0)
		k3x3 = deltaT*(vx3_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0)
		
		k3y1 = deltaT*(vy1_old + k2vy1/2.d0 + k2vy2/2.d0+ k2vy3/2.d0)
		k3y2 = deltaT*(vy2_old + k2vy1/2.d0 + k2vy2/2.d0+ k2vy3/2.d0)
		k3y3 = deltaT*(vy3_old + k2vy1/2.d0 + k2vy2/2.d0+ k2vy3/2.d0)	
		
		k3x12 = (k3x1+k3x2)
		k3x13 = (k3x1+k3x3)
		k3x23 = (k3x2+k3x3)
		
		k3y12 = (k3y1+k3y2)
		k3y13 = (k3y1+k3y3)
		k3y23 = (k3y2+k3y3)
		
		rp12 = dsqrt( (x1_old - x2_old + k3x12)**(2.d0) + (y1_old - y2_old + k3y12)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k3x13)**(2.d0) + (y1_old - y3_old + k3y13)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k3x23)**(2.d0) + (y2_old - y3_old + k3y23)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		k4vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k3x12)/r3p12) + ((x1_old - x3_old + k3x13)/r3p13) )
		k4vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k3x12)/r3p12) + ((x2_old - x3_old + k3x23)/r3p23) )
		k4vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k3x13)/r3p13) + ((x3_old - x2_old + k3x23)/r3p23) )
		
		k4vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k3y12)/r3p12) + ((y1_old - y3_old + k3y13)/r3p13) )
		k4vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k3y12)/r3p12) + ((y2_old - y3_old + k3y23)/r3p23) )
		k4vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k3y13)/r3p13) + ((y3_old - y2_old + k3y23)/r3p23) )		
		
		k4x1 = deltaT*(vx1_old + k3vx1 + k3vx2 + k3vx3)
		k4x2 = deltaT*(vx2_old + k3vx1 + k3vx2 + k3vx3)
		k4x3 = deltaT*(vx3_old + k3vx1 + k3vx2 + k3vx3)
		
		k4y1 = deltaT*(vy1_old + k3vy1 + k3vy2 + k3vy3)
		k4y2 = deltaT*(vy2_old + k3vy1 + k3vy2 + k3vy3)
		k4y3 = deltaT*(vy3_old + k3vy1 + k3vy2 + k3vy3)
		
		
		!================================================================================================
		!================================================================================================
		!APLICAÇÃO DO RK4 EM {X1 , X2 , X3 , VX1, VX2, VX3}:
		
		somakx1 = (1.d0/6.d0)*(k1x1 + (2.d0*k2x1) + (2.d0*k3x1) + k4x1)
		somakx2 = (1.d0/6.d0)*(k1x2 + (2.d0*k2x2) + (2.d0*k3x2) + k4x2)
		somakx3 = (1.d0/6.d0)*(k1x3 + (2.d0*k2x3) + (2.d0*k3x3) + k4x3)
		
		somakvx1 = (1.d0/6.d0)*(k1vx1 + (2.d0*k2vx1) + (2.d0*k3vx1) + k4vx1)
		somakvx2 = (1.d0/6.d0)*(k1vx2 + (2.d0*k2vx2) + (2.d0*k3vx2) + k4vx2)
		somakvx3 = (1.d0/6.d0)*(k1vx3 + (2.d0*k2vx3) + (2.d0*k3vx3) + k4vx3)
		
		x1_new = x1_old + somakx1
		x2_new = x2_old + somakx2
		x3_new = x3_old + somakx3

		vx1_new = vx1_old + somakvx1
		vx2_new = vx2_old + somakvx2
		vx3_new = vx3_old + somakvx3

		
		!================================================================================================
		!================================================================================================
		!APLICAÇÃO DO RK4 EM { Y1 , Y2 , Y3 , VY1 , VY2 , VY3}:
		
		somaky1 = (1.d0/6.d0)*(k1y1 + (2.d0*k2y1) + (2.d0*k3y1) + k4y1)
		somaky2 = (1.d0/6.d0)*(k1y2 + (2.d0*k2y2) + (2.d0*k3y2) + k4y2)
		somaky3 = (1.d0/6.d0)*(k1y3 + (2.d0*k2y3) + (2.d0*k3y3) + k4y3)
		
		somakvy1 = (1.d0/6.d0)*(k1vy1 + (2.d0*k2vy1) + (2.d0*k3vy1) + k4vy1)
		somakvy2 = (1.d0/6.d0)*(k1vy2 + (2.d0*k2vy2) + (2.d0*k3vy2) + k4vy2)
		somakvy3 = (1.d0/6.d0)*(k1vy3 + (2.d0*k2vy3) + (2.d0*k3vy3) + k4vy3)

		y1_new = y1_old + somaky1
		y2_new = y2_old + somaky2
		y3_new = y3_old + somaky3

		vy1_new = vy1_old + somakvy1
		vy2_new = vy2_old + somakvy2
		vy3_new = vy3_old + somakvy3

		!write(*,*)somakvx2 , somakvy2
	
		!================================================================================================
		!================================================================================================
		
		rp12 = dsqrt((x1_new-x2_new)**(2.d0) + (y1_new-y2_new)**(2.d0))
		rp13 = dsqrt((x1_new-x3_new)**(2.d0) + (y1_new-y3_new)**(2.d0))
		rp23 = dsqrt((x2_new-x3_new)**(2.d0) + (y2_new-y3_new)**(2.d0))	
		
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		x1_old = x1_new
		x2_old = x2_new
		x3_old = x3_new
		
		y1_old = y1_new
		y2_old = y2_new
		y3_old = y3_new
		
		vx1_old = vx1_new
		vx2_old = vx2_new
		vx3_old = vx3_new
		
		vy1_old = vy1_new
		vy2_old = vy2_new
		vy3_old = vy3_new
		
		tempo = tempo + deltaT

		
	end do


	!=========================
	!	QUESTÃO B 		======
	!=========================


	! CONDIÇÕES INICIAIS ALTERNATIVAS (LITERATURA):
	! Obs: Não foram utilizados no relatório, foram apenas para conferir a questão do valor de GM
	!x1_old = -1.d0
	!y1_old = 0.d0
	!vx1_old = 0.3471128135672417d0
	!vy1_old = 0.532726851767674d0

	!x2_old = 1.d0
	!y2_old = 0.d0
	!vx2_old = 0.3471128135672417d0
	!vy2_old = 0.532726851767674d0
	
	!x3_old = 0.d0
	!y3_old = 0.d0
	!vx3_old = (-2.d0)*0.3471128135672417d0
	!vy3_old = (-2.d0)*0.532726851767674d0
	
	! CONDIÇÕES INICIAIS PROJETO:
	x1_old = 0.97000436d0
	y1_old = -0.24308753d0
	vx1_old = 0.466203685d0
	vy1_old = 0.43236573d0

	x2_old = -0.97000436d0
	y2_old = 0.24308753d0
	vx2_old = 0.466203685d0
	vy2_old = 0.43236573d0
	
	x3_old = 0.d0
	y3_old = 0.d0
	vx3_old = -0.93240737d0
	vy3_old = -0.86473146d0
	
	
	rp12 = dsqrt( (x1_old - x2_old)**(2.d0) + (y1_old - y2_old)**(2.d0) )
	rp13 = dsqrt( (x1_old - x3_old)**(2.d0) + (y1_old - y3_old)**(2.d0) )
	rp23 = dsqrt( (x2_old - x3_old)**(2.d0) + (y2_old - y3_old)**(2.d0) )	
	
	r3p12 = rp12**(3.d0)
	r3p13 = rp13**(3.d0)
	r3p23 = rp23**(3.d0)
	
	rp12aux = r3p12
	rp13aux = r3p13
	rp23aux = r3p23
	
	delta = 0.1d0	! delta = 0.d0 , 0.01d0 , 0.05d0 ou 0.1d0
	delta1 = 1.d0 + delta
	
	G_M = (-1.d0)*4.d0*(pi**(2.d0))
	!G_M = -1.d0
	
	write(*,*)rp12 , r3p12
	write(*,*)rp13 , r3p13
	write(*,*)rp23 , r3p23

	deltaT = 0.2d0	! Caso eu mude, utilizei: 0.0002d0
	tempo = 0.d0

	do pontos = 1 , 5, 1	! Caso eu mude, utilizei: pontos = 1, 50000, 1 <- Deve fornecer 10 anos para deltaT = 0.0002d0
	
		write(54,*)x1_old, y1_old, tempo, x2_old, y2_old, tempo, x3_old, y3_old, tempo
		write(55,*)vx1_old, vy1_old, tempo, vx2_old, vy2_old, tempo, vx3_old, vy3_old, tempo
		write(56,*)tempo, rp12, rp13, rp23

		!================================================================================================
		!================================================================================================
		! RK4 - CALCULO COEFICIENTES P/ {VX1, VX2 , VX3 , X1 , X2 , X3, VY1 , VY2 , VY3 , Y1 , Y2 , Y3 }:
		
		k1vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old)/r3p12) + ((x1_old - x3_old)/r3p13) )
		k1vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old)/r3p12) + ((x2_old - x3_old)/r3p23) )
		k1vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old)/r3p13) + ((x3_old - x2_old)/r3p23) )
		
		k1vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old)/r3p12) + ((y1_old - y3_old)/r3p13) )
		k1vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old)/r3p12) + ((y2_old - y3_old)/r3p23) )
		k1vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old)/r3p13) + ((y3_old - y2_old)/r3p23) )

		k1x1 = deltaT*vx1_old
		k1x2 = deltaT*vx2_old
		k1x3 = deltaT*vx3_old
		
		k1y1 = deltaT*vy1_old
		k1y2 = deltaT*vy2_old
		k1y3 = deltaT*vy3_old

		k1x12_2 = (k1x1+k1x2)/2.d0
		k1x13_2 = (k1x1+k1x3)/2.d0
		k1x23_2 = (k1x2+k1x3)/2.d0
		
		k1y12_2 = (k1y1+k1y2)/2.d0
		k1y13_2 = (k1y1+k1y3)/2.d0
		k1y23_2 = (k1y2+k1y3)/2.d0
		
		rp12 = dsqrt( (x1_old - x2_old + k1x12_2)**(2.d0) + (y1_old - y2_old + k1x12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k1x13_2)**(2.d0) + (y1_old - y3_old + k1x13_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k1x23_2)**(2.d0) + (y2_old - y3_old + k1x23_2)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		k2vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k1x12_2)/r3p12) + ((x1_old - x3_old + k1x13_2)/r3p13) )
		k2vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k1x12_2)/r3p12) + ((x2_old - x3_old + k1x23_2)/r3p23) )
		k2vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k1x13_2)/r3p13) + ((x3_old - x2_old + k1x23_2)/r3p23) )
		
		k2vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k1y12_2)/r3p12) + ((y1_old - y3_old + k1y13_2)/r3p13) )
		k2vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k1y12_2)/r3p12) + ((y2_old - y3_old + k1y23_2)/r3p23) )
		k2vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k1y13_2)/r3p13) + ((y3_old - y2_old + k1y23_2)/r3p23) )

		k2x1 = deltaT*(vx1_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0)
		k2x2 = deltaT*(vx2_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0)
		k2x3 = deltaT*(vx3_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0)
		
		k2y1 = deltaT*(vy1_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0)
		k2y2 = deltaT*(vy2_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0)
		k2y3 = deltaT*(vy3_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0)
		
		k2x12_2 = (k2x1+k2x2)/2.d0
		k2x13_2 = (k2x1+k2x3)/2.d0
		k2x23_2 = (k2x2+k2x3)/2.d0
		
		k2y12_2 = (k2y1+k2y2)/2.d0
		k2y13_2 = (k2y1+k2y3)/2.d0
		k2y23_2 = (k2y2+k2y3)/2.d0
		
		rp12 = dsqrt( (x1_old - x2_old + k2x12_2)**(2.d0) + (y1_old - y2_old + k2y12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k2x13_2)**(2.d0) + (y1_old - y3_old + k2y13_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k2x23_2)**(2.d0) + (y2_old - y3_old + k2y23_2)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		k3vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k2x12_2)/r3p12) + ((x1_old - x3_old + k2x13_2)/r3p13) )
		k3vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k2x12_2)/r3p12) + ((x2_old - x3_old + k2x23_2)/r3p23) )
		k3vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k2x13_2)/r3p13) + ((x3_old - x2_old + k2x23_2)/r3p23) )
		
		k3vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k2y12_2)/r3p12) + ((y1_old - y3_old + k2y13_2)/r3p13) )
		k3vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k2y12_2)/r3p12) + ((y2_old - y3_old + k2y23_2)/r3p23) )
		k3vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k2y13_2)/r3p13) + ((y3_old - y2_old + k2y23_2)/r3p23) )

		k3x1 = deltaT*(vx1_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0)
		k3x2 = deltaT*(vx2_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0)
		k3x3 = deltaT*(vx3_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0)
		
		k3y1 = deltaT*(vy1_old + k2vy1/2.d0 + k2vy2/2.d0+ k2vy3/2.d0)
		k3y2 = deltaT*(vy2_old + k2vy1/2.d0 + k2vy2/2.d0+ k2vy3/2.d0)
		k3y3 = deltaT*(vy3_old + k2vy1/2.d0 + k2vy2/2.d0+ k2vy3/2.d0)	
		
		k3x12 = (k3x1+k3x2)
		k3x13 = (k3x1+k3x3)
		k3x23 = (k3x2+k3x3)
		
		k3y12 = (k3y1+k3y2)
		k3y13 = (k3y1+k3y3)
		k3y23 = (k3y2+k3y3)
		
		rp12 = dsqrt( (x1_old - x2_old + k3x12)**(2.d0) + (y1_old - y2_old + k3y12)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k3x13)**(2.d0) + (y1_old - y3_old + k3y13)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k3x23)**(2.d0) + (y2_old - y3_old + k3y23)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		k4vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k3x12)/r3p12) + ((x1_old - x3_old + k3x13)/r3p13) )
		k4vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k3x12)/r3p12) + ((x2_old - x3_old + k3x23)/r3p23) )
		k4vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k3x13)/r3p13) + ((x3_old - x2_old + k3x23)/r3p23) )
		
		k4vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k3y12)/r3p12) + ((y1_old - y3_old + k3y13)/r3p13) )
		k4vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k3y12)/r3p12) + ((y2_old - y3_old + k3y23)/r3p23) )
		k4vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k3y13)/r3p13) + ((y3_old - y2_old + k3y23)/r3p23) )		
		
		k4x1 = deltaT*(vx1_old + k3vx1 + k3vx2 + k3vx3)
		k4x2 = deltaT*(vx2_old + k3vx1 + k3vx2 + k3vx3)
		k4x3 = deltaT*(vx3_old + k3vx1 + k3vx2 + k3vx3)
		
		k4y1 = deltaT*(vy1_old + k3vy1 + k3vy2 + k3vy3)
		k4y2 = deltaT*(vy2_old + k3vy1 + k3vy2 + k3vy3)
		k4y3 = deltaT*(vy3_old + k3vy1 + k3vy2 + k3vy3)
		
		
		!================================================================================================
		!================================================================================================
		!APLICAÇÃO DO RK4 EM {X1 , X2 , X3 , VX1, VX2, VX3}:
		
		somakx1 = (1.d0/6.d0)*(k1x1 + (2.d0*k2x1) + (2.d0*k3x1) + k4x1)
		somakx2 = (1.d0/6.d0)*(k1x2 + (2.d0*k2x2) + (2.d0*k3x2) + k4x2)
		somakx3 = (1.d0/6.d0)*(k1x3 + (2.d0*k2x3) + (2.d0*k3x3) + k4x3)
		
		somakvx1 = (1.d0/6.d0)*(k1vx1 + (2.d0*k2vx1) + (2.d0*k3vx1) + k4vx1)
		somakvx2 = (1.d0/6.d0)*(k1vx2 + (2.d0*k2vx2) + (2.d0*k3vx2) + k4vx2)
		somakvx3 = (1.d0/6.d0)*(k1vx3 + (2.d0*k2vx3) + (2.d0*k3vx3) + k4vx3)
		
		x1_new = x1_old + somakx1
		x2_new = x2_old + somakx2
		x3_new = x3_old + somakx3

		vx1_new = vx1_old + somakvx1
		vx2_new = vx2_old + somakvx2
		vx3_new = vx3_old + somakvx3

		
		!================================================================================================
		!================================================================================================
		!APLICAÇÃO DO RK4 EM { Y1 , Y2 , Y3 , VY1 , VY2 , VY3}:
		
		somaky1 = (1.d0/6.d0)*(k1y1 + (2.d0*k2y1) + (2.d0*k3y1) + k4y1)
		somaky2 = (1.d0/6.d0)*(k1y2 + (2.d0*k2y2) + (2.d0*k3y2) + k4y2)
		somaky3 = (1.d0/6.d0)*(k1y3 + (2.d0*k2y3) + (2.d0*k3y3) + k4y3)
		
		somakvy1 = (1.d0/6.d0)*(k1vy1 + (2.d0*k2vy1) + (2.d0*k3vy1) + k4vy1)
		somakvy2 = (1.d0/6.d0)*(k1vy2 + (2.d0*k2vy2) + (2.d0*k3vy2) + k4vy2)
		somakvy3 = (1.d0/6.d0)*(k1vy3 + (2.d0*k2vy3) + (2.d0*k3vy3) + k4vy3)

		y1_new = y1_old + somaky1
		y2_new = y2_old + somaky2
		y3_new = y3_old + somaky3

		vy1_new = vy1_old + somakvy1
		vy2_new = vy2_old + somakvy2
		vy3_new = vy3_old + somakvy3

		!write(*,*)somakvx2 , somakvy2
	
		!================================================================================================
		!================================================================================================
		
		rp12 = dsqrt((x1_new-x2_new)**(2.d0) + (y1_new-y2_new)**(2.d0))
		rp13 = dsqrt((x1_new-x3_new)**(2.d0) + (y1_new-y3_new)**(2.d0))
		rp23 = dsqrt((x2_new-x3_new)**(2.d0) + (y2_new-y3_new)**(2.d0))	
		
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p23 = rp23**(3.d0)
		
		x1_old = x1_new
		x2_old = x2_new
		x3_old = x3_new
		
		y1_old = y1_new
		y2_old = y2_new
		y3_old = y3_new
		
		vx1_old = vx1_new
		vx2_old = vx2_new
		vx3_old = vx3_new
		
		vy1_old = vy1_new
		vy2_old = vy2_new
		vy3_old = vy3_new
		
		tempo = tempo + deltaT

		
	end do



	!=========================
	!	QUESTÃO C 		======
	!=========================

	! CONDIÇÕES INICIAIS PROJETO:	(Verificar sinais, andei trocando para testar)
	x1_old = 1.382856843618412d0
	y1_old = 0.d0
	vx1_old = 0.d0
	vy1_old = 0.584872630814899d0

	x2_old = 0.d0
	y2_old = 0.157029922281204d0
	vx2_old = 1.871935245878693d0	
	vy2_old = 0.d0
	
	x3_old = -1.382856843618412d0
	y3_old = 0.d0
	vx3_old = 0.d0
	vy3_old = -0.584872630814899d0
	
	x4_old = 0.d0
	y4_old = -0.157029922281204d0
	vx4_old = -1.871935245878693d0	
	vy4_old = 0.d0
	
	rp12 = dsqrt( (x1_old - x2_old)**(2.d0) + (y1_old - y2_old)**(2.d0) )
	rp13 = dsqrt( (x1_old - x3_old)**(2.d0) + (y1_old - y3_old)**(2.d0) )
	rp14 = dsqrt( (x1_old - x4_old)**(2.d0) + (y1_old - y4_old)**(2.d0) )
	rp23 = dsqrt( (x2_old - x3_old)**(2.d0) + (y2_old - y3_old)**(2.d0) )	
	rp24 = dsqrt( (x2_old - x4_old)**(2.d0) + (y2_old - y4_old)**(2.d0) )	
	rp34 = dsqrt( (x3_old - x4_old)**(2.d0) + (y3_old - y4_old)**(2.d0) )	
	
	r3p12 = rp12**(3.d0)
	r3p13 = rp13**(3.d0)
	r3p14 = rp14**(3.d0)
	r3p23 = rp23**(3.d0)
	r3p24 = rp24**(3.d0)
	r3p34 = rp34**(3.d0)
	
	delta = 0.0d0	! delta = 0.d0 , 0.01d0 , 0.05d0 ou 0.1d0
	delta1 = 1.d0 + delta

	G_M = -1.d0
	
	write(*,*)"12 = ",rp12 , r3p12
	write(*,*)"13 = ",rp13 , r3p13
	write(*,*)"14 = ",rp14 , r3p14
	write(*,*)"23 = ",rp23 , r3p23
	write(*,*)"24 = ",rp24 , r3p24
	write(*,*)"34 = ",rp34 , r3p34

	deltaT = 0.0002d0	! Caso eu mude, utilizei: 0.0002d0
	tempo = 0.d0

	do pontos = 1 , 50000, 1	! Caso eu mude, utilizei: pontos = 1, 50000, 1 <- Deve fornecer 10 anos para deltaT = 0.0002d0
	
		write(54,*)x1_old, y1_old, tempo, x2_old, y2_old, tempo, x3_old, y3_old, tempo
		!write(54,*)x1_old, y1_old, x2_old, y2_old, x3_old, y3_old, tempo
		write(64,*)x4_old, y4_old, tempo
		!write(55,*)vx1_old, vy1_old, tempo, vx2_old, vy2_old, tempo, vx3_old, vy3_old, tempo, vx4_old, vy4_old, tempo
		!write(56,*)tempo, rp12, rp13, rp14, rp23, rp24, rp34

		!================================================================================================
		!================================================================================================
		! RK4 - CALCULO COEFICIENTES P/ {VX1, VX2 , VX3 , VX4 , X1 , X2 , X3 , X4 , VY1 , VY2 , VY3 , VY4 , Y1 , Y2 , Y3 , Y4}:
		
		k1vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old)/r3p12) + ((x1_old - x3_old)/r3p13) + ((x1_old - x4_old)/r3p14))
		k1vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old)/r3p12) + ((x2_old - x3_old)/r3p23) + ((x2_old - x4_old)/r3p24))
		k1vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old)/r3p13) + ((x3_old - x2_old)/r3p23) + ((x3_old - x4_old)/r3p34))
		k1vx4 = deltaT*G_M*( (delta1*(x4_old - x1_old)/r3p14) + ((x4_old - x2_old)/r3p24) + ((x4_old - x3_old)/r3p34))
		
		k1vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old)/r3p12) + ((y1_old - y3_old)/r3p13) + ((y1_old - y4_old)/r3p14))
		k1vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old)/r3p12) + ((y2_old - y3_old)/r3p23) + ((y2_old - y4_old)/r3p24))
		k1vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old)/r3p13) + ((y3_old - y2_old)/r3p23) + ((y3_old - y4_old)/r3p34))
		k1vy4 = deltaT*G_M*( (delta1*(y4_old - y1_old)/r3p14) + ((y4_old - y2_old)/r3p24) + ((y4_old - y3_old)/r3p34))

		k1x1 = deltaT*vx1_old
		k1x2 = deltaT*vx2_old
		k1x3 = deltaT*vx3_old
		k1x4 = deltaT*vx4_old
		
		k1y1 = deltaT*vy1_old
		k1y2 = deltaT*vy2_old
		k1y3 = deltaT*vy3_old
		k1y4 = deltaT*vy4_old

		k1x12_2 = (k1x1+k1x2)/2.d0
		k1x13_2 = (k1x1+k1x3)/2.d0
		k1x14_2 = (k1x1+k1x4)/2.d0
		k1x23_2 = (k1x2+k1x3)/2.d0
		k1x24_2 = (k1x2+k1x4)/2.d0
		k1x34_2 = (k1x3+k1x4)/2.d0
		
		k1y12_2 = (k1y1+k1y2)/2.d0
		k1y13_2 = (k1y1+k1y3)/2.d0
		k1y14_2 = (k1y1+k1y4)/2.d0
		k1y23_2 = (k1y2+k1y3)/2.d0
		k1y24_2 = (k1y2+k1y4)/2.d0
		k1y34_2 = (k1y3+k1y4)/2.d0
		
		rp12 = dsqrt( (x1_old - x2_old + k1x12_2)**(2.d0) + (y1_old - y2_old + k1y12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k1x13_2)**(2.d0) + (y1_old - y3_old + k1y13_2)**(2.d0) )
		rp14 = dsqrt( (x1_old - x4_old + k1x14_2)**(2.d0) + (y1_old - y4_old + k1y14_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k1x23_2)**(2.d0) + (y2_old - y3_old + k1y23_2)**(2.d0) )	
		rp24 = dsqrt( (x2_old - x4_old + k1x24_2)**(2.d0) + (y2_old - y4_old + k1y24_2)**(2.d0) )
		rp34 = dsqrt( (x3_old - x4_old + k1x34_2)**(2.d0) + (y3_old - y4_old + k1y34_2)**(2.d0) )	
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p14 = rp14**(3.d0)
		r3p23 = rp23**(3.d0)
		r3p24 = rp24**(3.d0)
		r3p34 = rp34**(3.d0)
		
		!k2vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k1x12_2)/r3p12) + ((x1_old - x3_old + k1x13_2)/r3p13) + ((x1_old - x4_old + k1x14_2)/r3p14) )
		aux1 = x1_old - x2_old + k1x12_2
		aux2 = x1_old - x3_old + k1x13_2
		aux3 = x1_old - x4_old + k1x14_2
		k2vx1 = deltaT*G_M*delta1*( ((aux1)/r3p12) + ((aux2)/r3p13) + ((aux3)/r3p14) )
		
		!k2vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k1x12_2)/r3p12) + ((x2_old - x3_old + k1x23_2)/r3p23) + ((x2_old - x4_old + k1x24_2)/r3p24) )
		aux1 = x2_old - x1_old + k1x12_2
		aux2 = x2_old - x3_old + k1x23_2
		aux3 = x2_old - x4_old + k1x24_2		
		k2vx2 = deltaT*G_M*( (delta1*(aux1)/r3p12) + ((aux2)/r3p23) + ((aux3)/r3p24) )
		
		!k2vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k1x13_2)/r3p13) + ((x3_old - x2_old + k1x23_2)/r3p23) + ((x3_old - x4_old + k1x34_2)/r3p34) )
		aux1 = x3_old - x1_old + k1x13_2
		aux2 = x3_old - x2_old + k1x23_2
		aux3 = x3_old - x4_old + k1x34_2			
		k2vx3 = deltaT*G_M*( (delta1*(aux1)/r3p13) + ((aux2)/r3p23) + ((aux3)/r3p34) )
		
		!k2vx4 = deltaT*G_M*( (delta1*(x4_old - x1_old + k1x14_2)/r3p14) + ((x4_old - x2_old + k1x24_2)/r3p24) + ((x4_old - x3_old + k1x34_2)/r3p34) )
		aux1 = x4_old - x1_old + k1x14_2
		aux2 = x4_old - x2_old + k1x24_2
		aux3 = x4_old - x3_old + k1x34_2			
		k2vx4 = deltaT*G_M*( (delta1*(aux1)/r3p14) + ((aux2)/r3p24) + ((aux3)/r3p34) )
		
		!k2vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k1y12_2)/r3p12) + ((y1_old - y3_old + k1y13_2)/r3p13) + ((y1_old - y4_old + k1y14_2)/r3p14) )
		aux1 = y1_old - y2_old + k1y12_2
		aux2 = y1_old - y3_old + k1y13_2
		aux3 = y1_old - y4_old + k1y14_2					
		k2vy1 = deltaT*G_M*delta1*( ((aux1)/r3p12) + ((aux2)/r3p13) + ((aux3)/r3p14) )
		
		!k2vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k1y12_2)/r3p12) + ((y2_old - y3_old + k1y23_2)/r3p23) + ((y2_old - y4_old + k1y24_2)/r3p24) )
		aux1 = y2_old - y1_old + k1y12_2
		aux2 = y2_old - y3_old + k1y23_2
		aux3 = y2_old - y4_old + k1y24_2		
		k2vy2 = deltaT*G_M*( (delta1*(aux1)/r3p12) + ((aux2)/r3p23) + ((aux3)/r3p24) )
		
		!k2vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k1y13_2)/r3p13) + ((y3_old - y2_old + k1y23_2)/r3p23) + ((y3_old - y4_old + k1y34_2)/r3p34) )
		aux1 = y3_old - y1_old + k1y13_2
		aux2 = y3_old - y2_old + k1y23_2
		aux3 = y3_old - y4_old + k1y34_2		
		k2vy3 = deltaT*G_M*( (delta1*(aux1)/r3p13) + ((aux2)/r3p23) + ((aux3)/r3p34) )
		
		!k2vy4 = deltaT*G_M*( (delta1*(y4_old - y1_old + k1y14_2)/r3p14) + ((y4_old - y2_old + k1y24_2)/r3p24) + ((y4_old - y3_old + k1y34_2)/r3p34) )
		aux1 = y4_old - y1_old + k1y14_2
		aux2 = y4_old - y2_old + k1y24_2
		aux3 = y4_old - y3_old + k1y34_2		
		k2vy4 = deltaT*G_M*( (delta1*(aux1)/r3p14) + ((aux2)/r3p24) + ((aux3)/r3p34) )

		k2x1 = deltaT*(vx1_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0 + k1vx4/2.d0)
		k2x2 = deltaT*(vx2_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0 + k1vx4/2.d0)
		k2x3 = deltaT*(vx3_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0 + k1vx4/2.d0)
		k2x4 = deltaT*(vx4_old + k1vx1/2.d0 + k1vx2/2.d0 + k1vx3/2.d0 + k1vx4/2.d0)
		
		k2y1 = deltaT*(vy1_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0 + k1vy4/2.d0)
		k2y2 = deltaT*(vy2_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0 + k1vy4/2.d0)
		k2y3 = deltaT*(vy3_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0 + k1vy4/2.d0)
		k2y4 = deltaT*(vy4_old + k1vy1/2.d0 + k1vy2/2.d0 + k1vy3/2.d0 + k1vy4/2.d0)
		
		k2x12_2 = (k2x1+k2x2)/2.d0
		k2x13_2 = (k2x1+k2x3)/2.d0
		k2x14_2 = (k2x1+k2x4)/2.d0
		k2x23_2 = (k2x2+k2x3)/2.d0
		k2x24_2 = (k2x2+k2x4)/2.d0
		k2x34_2 = (k2x3+k2x4)/2.d0
		
		k2y12_2 = (k2y1+k2y2)/2.d0
		k2y13_2 = (k2y1+k2y3)/2.d0
		k2y14_2 = (k2y1+k2y4)/2.d0
		k2y23_2 = (k2y2+k2y3)/2.d0
		k2y24_2 = (k2y2+k2y4)/2.d0
		k2y34_2 = (k2y3+k2y4)/2.d0
		
		rp12 = dsqrt( (x1_old - x2_old + k2x12_2)**(2.d0) + (y1_old - y2_old + k2y12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k2x13_2)**(2.d0) + (y1_old - y3_old + k2y13_2)**(2.d0) )
		rp14 = dsqrt( (x1_old - x4_old + k2x14_2)**(2.d0) + (y1_old - y4_old + k2y14_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k2x23_2)**(2.d0) + (y2_old - y3_old + k2y23_2)**(2.d0) )	
		rp24 = dsqrt( (x2_old - x4_old + k2x24_2)**(2.d0) + (y2_old - y4_old + k2y24_2)**(2.d0) )
		rp34 = dsqrt( (x3_old - x4_old + k2x34_2)**(2.d0) + (y3_old - y4_old + k2y34_2)**(2.d0) )
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p14 = rp14**(3.d0)
		r3p23 = rp23**(3.d0)
		r3p24 = rp24**(3.d0)
		r3p34 = rp34**(3.d0)
		
		!k3vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k2x12_2)/r3p12) + ((x1_old - x3_old + k2x13_2)/r3p13) + ((x1_old - x4_old + k2x14_2)/r3p14) )
		aux1 = x1_old - x2_old + k2x12_2
		aux2 = x1_old - x3_old + k2x13_2
		aux3 = x1_old - x4_old + k2x14_2	
		k3vx1 = deltaT*G_M*delta1*( ((aux1)/r3p12) + ((aux2)/r3p13) + ((aux3)/r3p14) )
		
		!k3vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k2x12_2)/r3p12) + ((x2_old - x3_old + k2x23_2)/r3p23) + ((x2_old - x4_old + k2x24_2)/r3p24) )
		aux1 = x2_old - x1_old + k2x12_2
		aux2 = x2_old - x3_old + k2x23_2
		aux3 = x2_old - x4_old + k2x24_2	
		k3vx2 = deltaT*G_M*( (delta1*(aux1)/r3p12) + ((aux2)/r3p23) + ((aux3)/r3p24) )
		
		!k3vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k2x13_2)/r3p13) + ((x3_old - x2_old + k2x23_2)/r3p23) + ((x3_old - x4_old + k2x34_2)/r3p34) )
		aux1 = x3_old - x1_old + k2x13_2
		aux2 = x3_old - x2_old + k2x23_2
		aux3 = x3_old - x4_old + k2x34_2
		k3vx3 = deltaT*G_M*( (delta1*(aux1)/r3p13) + ((aux2)/r3p23) + ((aux3)/r3p34) )
		
		!k3vx4 = deltaT*G_M*( (delta1*(x4_old - x1_old + k2x14_2)/r3p14) + ((x4_old - x2_old + k2x24_2)/r3p24) + ((x4_old - x3_old + k2x34_2)/r3p34) )
		aux1 = x4_old - x1_old + k2x14_2
		aux2 = x4_old - x2_old + k2x24_2
		aux3 = x4_old - x3_old + k2x34_2	
		k3vx4 = deltaT*G_M*( (delta1*(aux1)/r3p14) + ((aux2)/r3p24) + ((aux3)/r3p34) )
		
		!k3vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k2y12_2)/r3p12) + ((y1_old - y3_old + k2y13_2)/r3p13) + ((y1_old - y4_old + k2y14_2)/r3p14) )
		aux1 = y1_old - y2_old + k2y12_2
		aux2 = y1_old - y3_old + k2y13_2
		aux3 = y1_old - y4_old + k2y14_2	
		k3vy1 = deltaT*G_M*delta1*( ((aux1)/r3p12) + ((aux2)/r3p13) + ((aux3)/r3p14) )
		
		!k3vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k2y12_2)/r3p12) + ((y2_old - y3_old + k2y23_2)/r3p23) + ((y2_old - y4_old + k2y24_2)/r3p24) )
		aux1 = y2_old - y1_old + k2y12_2
		aux2 = y2_old - y3_old + k2y23_2
		aux3 = y2_old - y4_old + k2y24_2	
		k3vy2 = deltaT*G_M*( (delta1*(aux1)/r3p12) + ((aux2)/r3p23) + ((aux3)/r3p24) )
		
		!k3vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k2y13_2)/r3p13) + ((y3_old - y2_old + k2y23_2)/r3p23) + ((y3_old - y4_old + k2y34_2)/r3p34) )
		aux1 = y3_old - y1_old + k2y13_2
		aux2 = y3_old - y2_old + k2y23_2
		aux3 = y3_old - y4_old + k2y34_2		
		k3vy3 = deltaT*G_M*( (delta1*(aux1)/r3p13) + ((aux2)/r3p23) + ((aux3)/r3p34) )
		
		!k3vy4 = deltaT*G_M*( (delta1*(y4_old - y1_old + k2y14_2)/r3p14) + ((y4_old - y2_old + k2y24_2)/r3p24) + ((y4_old - y3_old + k2y34_2)/r3p34) )
		aux1 = y4_old - y1_old + k2y14_2
		aux2 = y4_old - y2_old + k2y24_2
		aux3 = y4_old - y3_old + k2y34_2		
		k3vy4 = deltaT*G_M*( (delta1*(aux1)/r3p14) + ((aux2)/r3p24) + ((aux3)/r3p34) )

		k3x1 = deltaT*(vx1_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0 + k2vx4/2.d0)
		k3x2 = deltaT*(vx2_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0 + k2vx4/2.d0)
		k3x3 = deltaT*(vx3_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0 + k2vx4/2.d0)
		k3x4 = deltaT*(vx4_old + k2vx1/2.d0 + k2vx2/2.d0 + k2vx3/2.d0 + k2vx4/2.d0)
		
		k3y1 = deltaT*(vy1_old + k2vy1/2.d0 + k2vy2/2.d0 + k2vy3/2.d0 + k2vy4/2.d0)
		k3y2 = deltaT*(vy2_old + k2vy1/2.d0 + k2vy2/2.d0 + k2vy3/2.d0 + k2vy4/2.d0)
		k3y3 = deltaT*(vy3_old + k2vy1/2.d0 + k2vy2/2.d0 + k2vy3/2.d0 + k2vy4/2.d0)
		k3y4 = deltaT*(vy4_old + k2vy1/2.d0 + k2vy2/2.d0 + k2vy3/2.d0 + k2vy4/2.d0)

		k3x12_2 = (k3x1+k3x2)
		k3x13_2 = (k3x1+k3x3)
		k3x14_2 = (k3x1+k3x4)
		k3x23_2 = (k3x2+k3x3)
		k3x24_2 = (k3x2+k3x4)
		k3x34_2 = (k3x3+k3x4)
		
		k3y12_2 = (k3y1+k3y2)
		k3y13_2 = (k3y1+k3y3)
		k3y14_2 = (k3y1+k3y4)
		k3y23_2 = (k3y2+k3y3)
		k3y24_2 = (k3y2+k3y4)
		k3y34_2 = (k3y3+k3y4)
		
		rp12 = dsqrt( (x1_old - x2_old + k3x12_2)**(2.d0) + (y1_old - y2_old + k3y12_2)**(2.d0) )
		rp13 = dsqrt( (x1_old - x3_old + k3x13_2)**(2.d0) + (y1_old - y3_old + k3y13_2)**(2.d0) )
		rp14 = dsqrt( (x1_old - x4_old + k3x14_2)**(2.d0) + (y1_old - y4_old + k3y14_2)**(2.d0) )
		rp23 = dsqrt( (x2_old - x3_old + k3x23_2)**(2.d0) + (y2_old - y3_old + k3y23_2)**(2.d0) )	
		rp24 = dsqrt( (x2_old - x4_old + k3x24_2)**(2.d0) + (y2_old - y4_old + k3y24_2)**(2.d0) )
		rp34 = dsqrt( (x3_old - x4_old + k3x34_2)**(2.d0) + (y3_old - y4_old + k3y34_2)**(2.d0) )
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p14 = rp14**(3.d0)
		r3p23 = rp23**(3.d0)
		r3p24 = rp24**(3.d0)
		r3p34 = rp34**(3.d0)
		
		!k4vx1 = deltaT*G_M*delta1*( ((x1_old - x2_old + k3x12_2)/r3p12) + ((x1_old - x3_old + k3x13_2)/r3p13) + ((x1_old - x4_old + k3x14_2)/r3p14) )
		aux1 = x1_old - x2_old + k3x12_2
		aux2 = x1_old - x3_old + k3x13_2
		aux3 = x1_old - x4_old + k3x14_2	
		k4vx1 = deltaT*G_M*delta1*( ((aux1)/r3p12) + ((aux2)/r3p13) + ((aux3)/r3p14) )
		
		!k4vx2 = deltaT*G_M*( (delta1*(x2_old - x1_old + k3x12_2)/r3p12) + ((x2_old - x3_old + k3x23_2)/r3p23) + ((x2_old - x4_old + k3x24_2)/r3p24) )
		aux1 = x2_old - x1_old + k3x12_2
		aux2 = x2_old - x3_old + k3x23_2
		aux3 = x2_old - x4_old + k3x24_2	
		k4vx2 = deltaT*G_M*( (delta1*(aux1)/r3p12) + ((aux2)/r3p23) + ((aux3)/r3p24) )

		!k4vx3 = deltaT*G_M*( (delta1*(x3_old - x1_old + k3x13_2)/r3p13) + ((x3_old - x2_old + k3x23_2)/r3p23) + ((x3_old - x4_old + k3x34_2)/r3p34) )
		aux1 = x3_old - x1_old + k3x13_2
		aux2 = x3_old - x2_old + k3x23_2
		aux3 = x3_old - x4_old + k3x34_2	
		k4vx3 = deltaT*G_M*( (delta1*(aux1)/r3p13) + ((aux2)/r3p23) + ((aux3)/r3p34) )

		!k4vx4 = deltaT*G_M*( (delta1*(x4_old - x1_old + k3x14_2)/r3p14) + ((x4_old - x2_old + k3x24_2)/r3p24) + ((x4_old - x3_old + k3x34_2)/r3p34) )
		aux1 = x4_old - x1_old + k3x14_2
		aux2 = x4_old - x2_old + k3x24_2
		aux3 = x4_old - x3_old + k3x34_2
		k4vx4 = deltaT*G_M*( (delta1*(aux1)/r3p14) + ((aux2)/r3p24) + ((aux3)/r3p34) )
		
		!k4vy1 = deltaT*G_M*delta1*( ((y1_old - y2_old + k3y12_2)/r3p12) + ((y1_old - y3_old + k3y13_2)/r3p13) + ((y1_old - y4_old + k3y14_2)/r3p14) )
		aux1 = y1_old - y2_old + k3y12_2
		aux2 = y1_old - y3_old + k3y13_2
		aux3 = y1_old - y4_old + k3y14_2
		k4vy1 = deltaT*G_M*delta1*( ((aux1)/r3p12) + ((aux2)/r3p13) + ((aux3)/r3p14) )
		
		!k4vy2 = deltaT*G_M*( (delta1*(y2_old - y1_old + k3y12_2)/r3p12) + ((y2_old - y3_old + k3y23_2)/r3p23) + ((y2_old - y4_old + k3y24_2)/r3p24) )
		aux1 = y2_old - y1_old + k3y12_2
		aux2 = y2_old - y3_old + k3y23_2
		aux3 = y2_old - y4_old + k3y24_2
		k4vy2 = deltaT*G_M*( (delta1*(aux1)/r3p12) + ((aux2)/r3p23) + ((aux3)/r3p24) )
		
		!k4vy3 = deltaT*G_M*( (delta1*(y3_old - y1_old + k3y13_2)/r3p13) + ((y3_old - y2_old + k3y23_2)/r3p23) + ((y3_old - y4_old + k3y34_2)/r3p34) )
		aux1 = y3_old - y1_old + k3y13_2
		aux2 = y3_old - y2_old + k3y23_2
		aux3 = y3_old - y4_old + k3y34_2		
		k4vy3 = deltaT*G_M*( (delta1*(aux1)/r3p13) + ((aux2)/r3p23) + ((aux3)/r3p34) )
		
		!k4vy4 = deltaT*G_M*( (delta1*(y4_old - y1_old + k3y14_2)/r3p14) + ((y4_old - y2_old + k3y24_2)/r3p24) + ((y4_old - y3_old + k3y34_2)/r3p34) )
		aux1 = y4_old - y1_old + k3y14_2
		aux2 = y4_old - y2_old + k3y24_2
		aux3 = y4_old - y3_old + k3y34_2
		k4vy4 = deltaT*G_M*( (delta1*(aux1)/r3p14) + ((aux2)/r3p24) + ((aux3)/r3p34) )
		
		k4x1 = deltaT*(vx1_old + k3vx1 + k3vx2 + k3vx3 + k3vx4)
		k4x2 = deltaT*(vx2_old + k3vx1 + k3vx2 + k3vx3 + k3vx4)
		k4x3 = deltaT*(vx3_old + k3vx1 + k3vx2 + k3vx3 + k3vx4)
		k4x4 = deltaT*(vx4_old + k3vx1 + k3vx2 + k3vx3 + k3vx4)
		
		k4y1 = deltaT*(vy1_old + k3vy1 + k3vy2 + k3vy3 + k3vy4)
		k4y2 = deltaT*(vy2_old + k3vy1 + k3vy2 + k3vy3 + k3vy4)
		k4y3 = deltaT*(vy3_old + k3vy1 + k3vy2 + k3vy3 + k3vy4)
		k4y4 = deltaT*(vy4_old + k3vy1 + k3vy2 + k3vy3 + k3vy4)
		
		!================================================================================================
		!================================================================================================
		!APLICAÇÃO DO RK4 EM {X1 , X2 , X3 , X4 , VX1 , VX2 , VX3 , VX4}:
		
		somakx1 = (1.d0/6.d0)*(k1x1 + (2.d0*k2x1) + (2.d0*k3x1) + k4x1)
		somakx2 = (1.d0/6.d0)*(k1x2 + (2.d0*k2x2) + (2.d0*k3x2) + k4x2)
		somakx3 = (1.d0/6.d0)*(k1x3 + (2.d0*k2x3) + (2.d0*k3x3) + k4x3)
		somakx4 = (1.d0/6.d0)*(k1x4 + (2.d0*k2x4) + (2.d0*k3x4) + k4x4)
		
		somakvx1 = (1.d0/6.d0)*(k1vx1 + (2.d0*k2vx1) + (2.d0*k3vx1) + k4vx1)
		somakvx2 = (1.d0/6.d0)*(k1vx2 + (2.d0*k2vx2) + (2.d0*k3vx2) + k4vx2)
		somakvx3 = (1.d0/6.d0)*(k1vx3 + (2.d0*k2vx3) + (2.d0*k3vx3) + k4vx3)
		somakvx4 = (1.d0/6.d0)*(k1vx4 + (2.d0*k2vx4) + (2.d0*k3vx4) + k4vx4)
		
		x1_new = x1_old + somakx1
		x2_new = x2_old + somakx2
		x3_new = x3_old + somakx3
		x4_new = x4_old + somakx4

		vx1_new = vx1_old + somakvx1
		vx2_new = vx2_old + somakvx2
		vx3_new = vx3_old + somakvx3
		vx4_new = vx4_old + somakvx4
		
		!================================================================================================
		!================================================================================================
		!APLICAÇÃO DO RK4 EM { Y1 , Y2 , Y3 , Y4 , VY1 , VY2 , VY3 , VY4}:
		
		somaky1 = (1.d0/6.d0)*(k1y1 + (2.d0*k2y1) + (2.d0*k3y1) + k4y1)
		somaky2 = (1.d0/6.d0)*(k1y2 + (2.d0*k2y2) + (2.d0*k3y2) + k4y2)
		somaky3 = (1.d0/6.d0)*(k1y3 + (2.d0*k2y3) + (2.d0*k3y3) + k4y3)
		somaky4 = (1.d0/6.d0)*(k1y4 + (2.d0*k2y4) + (2.d0*k3y4) + k4y4)
		
		somakvy1 = (1.d0/6.d0)*(k1vy1 + (2.d0*k2vy1) + (2.d0*k3vy1) + k4vy1)
		somakvy2 = (1.d0/6.d0)*(k1vy2 + (2.d0*k2vy2) + (2.d0*k3vy2) + k4vy2)
		somakvy3 = (1.d0/6.d0)*(k1vy3 + (2.d0*k2vy3) + (2.d0*k3vy3) + k4vy3)
		somakvy4 = (1.d0/6.d0)*(k1vy4 + (2.d0*k2vy4) + (2.d0*k3vy4) + k4vy4)
		
		y1_new = y1_old + somaky1
		y2_new = y2_old + somaky2
		y3_new = y3_old + somaky3
		y4_new = y4_old + somaky4

		vy1_new = vy1_old + somakvy1
		vy2_new = vy2_old + somakvy2
		vy3_new = vy3_old + somakvy3
		vy4_new = vy4_old + somakvy4
	
		!================================================================================================
		!================================================================================================

		rp12 = dsqrt( (x1_new - x2_new)**(2.d0) + (y1_new - y2_new)**(2.d0) )
		rp13 = dsqrt( (x1_new - x3_new)**(2.d0) + (y1_new - y3_new)**(2.d0) )
		rp14 = dsqrt( (x1_new - x4_new)**(2.d0) + (y1_new - y4_new)**(2.d0) )
		rp23 = dsqrt( (x2_new - x3_new)**(2.d0) + (y2_new - y3_new)**(2.d0) )	
		rp24 = dsqrt( (x2_new - x4_new)**(2.d0) + (y2_new - y4_new)**(2.d0) )
		rp34 = dsqrt( (x3_new - x4_new)**(2.d0) + (y3_new - y4_new)**(2.d0) )
		r3p12 = rp12**(3.d0)
		r3p13 = rp13**(3.d0)
		r3p14 = rp14**(3.d0)
		r3p23 = rp23**(3.d0)
		r3p24 = rp24**(3.d0)
		r3p34 = rp34**(3.d0)
		
		x1_old = x1_new
		x2_old = x2_new
		x3_old = x3_new
		x4_old = x4_new
		
		y1_old = y1_new
		y2_old = y2_new
		y3_old = y3_new
		y4_old = y4_new
		
		vx1_old = vx1_new
		vx2_old = vx2_new
		vx3_old = vx3_new
		vx4_old = vx4_new
		
		vy1_old = vy1_new
		vy2_old = vy2_new
		vy3_old = vy3_new
		vy4_old = vy4_new
		
		! LAÇO FACULTATIVO:
		! Restringindo o laço para lidar com menos dados para o relatório
		! (alternativa: alterar o intervalo i = inicio , final , incremento)
		! Desvantagem:	Utilizar aumenta o custo computacional
		! Vantagem:		Não altera nada mais no código
		! 
		if (tempo.GT.5.d0) then
			exit
		end if
		
		tempo = tempo + deltaT

		
	end do


end program coreografia_celeste