[org 0x0100]

box: dw 0
char: dw 0,0,0,0,0
delayT: dw 1,2,2,3,1
location: dw 0,0,0,0,0
scoreV: dw 0
life: dw 0
lifelocFlag: db 1,1,1,1,1,1,1,1,1,1
oldisr : dd 0

jmp start

clrscr:
	pusha
	mov ax, 0xB800
	mov es, ax
	xor di, di
	mov cx, 2000
	mov ax , 0x0720
	rep stosw
	popa
	ret
	
begin:
	pusha
	mov ax, 0xB800
	mov es, ax
	xor di, di
	mov cx, 2000
	mov ax , 0x0120
	rep stosw
	
	    ; Print "BEGIN" on the next line
	mov ah,0x83
    lea si, beginn
    mov di, 1970
    mov cx, 23
	print_begin:
    lodsb
    stosw
    loop print_begin
	
	popa
	ret
		
delay:
	pusha
	pushf

	mov cx,200
	mydelay:
	mov bx,500
	mydelay1:
	dec bx
	jnz mydelay1
	loop mydelay

	popf
	popa
	ret	
	
delay2:
	pusha
	pushf

	mov cx,1000
	mydelay2:
	mov bx,500
	mydelay3:
	dec bx
	jnz mydelay3
	loop mydelay2

	popf
	popa
	ret	
	
score:
    pusha
    mov ax, 0xB800
    mov es, ax    
    mov di, 1748        ; Location to display the score
    mov ax, [scoreV]    ; Get the score value
    xor cx, cx          ; Reset CX for digit count

    ; Convert the score to decimal digits
    mov bx, 10          ; Divisor for base 10
	convert_loop:
    xor dx, dx          ; Clear DX for division
    div bx              ; Divide AX by 10, quotient in AX, remainder in DX
    push dx             ; Store remainder (digit)
    inc cx              ; Increment digit count
    test ax, ax         ; Check if quotient is 0
    jnz convert_loop    ; If not, continue dividing

    ; If the score is 0, handle it explicitly
    cmp cx, 0
    jnz print_digits
    mov ax, '0'  
    stosw
    jmp score_done


	print_digits:
    pop ax 
    add al, '0' 
    mov ah, 0x06
    stosw
    loop print_digits 

	score_done:
    popa
    ret

	
	
	
	;; score 2 
scoree:
    pusha
    mov ax, 0xB800
    mov es, ax    
    mov di, 2152      ; Location to display the score
    mov ax, [scoreV]    ; Get the score value
    xor cx, cx          ; Reset CX for digit count

    ; Convert the score to decimal digits
    mov bx, 10          ; Divisor for base 10
	convertloop:
    xor dx, dx          ; Clear DX for division
    div bx              ; Divide AX by 10, quotient in AX, remainder in DX
    push dx             ; Store remainder (digit)
    inc cx              ; Increment digit count
    test ax, ax         ; Check if quotient is 0
    jnz convertloop    ; If not, continue dividing

    ; If the score is 0, handle it explicitly
    cmp cx, 0
    jnz printdigits
    mov ax, '0'  
    stosw
    jmp scoredone


	printdigits:
    pop ax 
    add al, '0' 
    mov ah, 0x03
    stosw
    loop printdigits 

	scoredone:
    popa
    ret

	
	
movalphabets:
	pusha
	mov ax, 0xB800
	mov es, ax
	mov si,0
	mov cx,5
	
	m1:
	mov di , [location+si]
	mov ax , 0x0720		;clear alphabet in above row
	stosw
	sub di,2 
	mov [location+si] ,di
	add si,2
	
	call draw  			; to deal with wen alphabets clrs the bar
	loop m1

	mov si ,0
	mov cx ,5
	
	m2:
	mov di , [location+si]
	mov ax, [delayT+si]
	mov ah , 0
	mov bl,160			;for diff speed controls jump
	mul bl
	add di,ax
	mov [location +si] , di
	mov ax , [char+si]
	stosw
	add si,2
	;SCORE & LIFE
	sub di,2
	cmp di,3840		;life
	jle endd
	cmp di , 4000 
	jg endd
	
	
	cmp word di, [box]		;score
	jne ll1
	add word [scoreV] ,10
	call draw2
	jmp endd
	ll1:
	sub di,2
	cmp word di, [box]		;score
	jne l2
	add word [scoreV] ,10
	call draw2
	jmp endd
	l2:
	sub di,2
	cmp word di, [box]		;score
	jne l4
	add word [scoreV] ,10
	call draw2
	jmp endd
	l4:
	add word [life],1
	call decrementLife		;for hearts print
	
	
	cmp word [life] , 10
	je gameover
	
	endd:
	
	loop m2
	call delay2
	call score
	popa
	ret 

initialPrint:
	pusha
	mov ax, 0xB800
	mov es, ax
	mov cx , 5
	mov si,0
	i1:
	mov di , [location+si]
	mov ax , [char+si]
	stosw
	add si,2
	loop i1
	popa
	ret 

GenRandom:
	pusha 
	mov si ,0
	
	a1:
	sub sp,2 
	push 25
	call RANDNUM 	;random alphabet
	pop ax	
	add al ,65		;for ascii
	mov ah, 0x04
	mov [char + si ] , ax
	
	sub sp,2 	
	push 158
	call RANDNUM 	;random location 
	pop di
	test di,1		; check if even or odd
	jz even
	inc di
	even:
	
	 
	sub sp,2
	push 5
	call RANDNUM
	pop dx
	inc dx
	mov ax,di		;mul location by RANDNUM num
	mul dl
	mov di,ax
	and di, 0x7F            ; Ensure the position is even (0, 2, 4, …, 158)
	
	
	mov [location+si] , di
	
	
	call delay
	;call delay
	
	add si,2
	cmp si,10
	jne a1
	
	popa
	ret
	

RANDNUM:
   push bp
   mov bp,sp
   push ax
   push cx
   push dx
   
   MOV AH, 00h  ; interrupts to get system time        
   INT 1AH      ; CX:DX now hold number of clock ticks since midnight      
   mov  ax, dx
   xor  dx, dx
   mov  cx, [bp+4] 
   inc cx   
   div  cx       ; here dx contains the remainder of the division - from 0 to 9
   mov [bp+6], dx
   pop dx
   pop cx
   pop ax
   pop bp   
   ret 2	


ISRint9:

	pusha

	mov ah, 0
	in al , 0x60

	cmp al, 0x4B
	je left
	cmp al, 0x4D
	je right
	cmp al, 0x01
	je gameover
	cmp al, 0x39
	je start
	jmp end

	left:
	cmp word [box] , 3840 		; does not go outside edge
	je end
	mov di , [box]
	mov ax , 0x0720
	mov cx ,3
	rep stosw
	sub word [box],2
	jmp end

	right:
	cmp word [box] , 3964 		; does not go outside edge
	je end
	mov di , [box]
	mov ax , 0x0720
	mov cx ,3
	rep stosw
	add word [box],2
	jmp end
	

	end:
	mov al, 0x20
	out 0x20,al
	call draw
	popa
	
	esc:
	iret

draw:
	pusha
	mov ax, 0xB800
	mov es, ax
	mov di, [box]
	mov al, 0xDC
	
	mov cx ,3
	mov ah, 0x03
	rep stosw
	popa
	ret

draw2:					;for wen contact
	pusha
	mov ax, 0xB800		;green color 
	mov es, ax
	mov di, [box]
	mov al, 0xDC
	mov cx ,3
	mov ah, 0x02
	rep stosw
	call delay
	call delay
	mov di, [box]		;blue color again
	mov al, 0xDC
	mov cx ,3
	mov ah, 0x03
	rep stosw
	
	popa
	ret

printHEARTS:
    pusha
    mov ax, 0xB800
    mov es, ax
    xor si, si
    mov di, 2376

	print_loop:
    cmp si, 10 
    jge enddp
    mov al, [lifelocFlag + si]
    cmp al, 1 
    jne clr

   
    mov ax, 0x0403
    stosw
    jmp next_heart

	clr:
    mov ax, 0x0720
    stosw

	next_heart:
    add si, 1
    cmp si, 2
    je switch_column
    cmp si, 4
    je switch_column
    cmp si, 6
    je switch_column
    cmp si, 8
    je switch_column
    add di, 2
    jmp print_loop

	switch_column:
    add di, 154
    jmp print_loop

	enddp:
    popa
    ret



decrementLife:
    pusha
    cmp word [life], 0 
    je noleft

    mov si, 0

	updateflag:
    cmp byte [lifelocFlag + si], 1 
    jne next_flag
    mov byte [lifelocFlag + si], 0
    jmp end_decrement

	next_flag:
    inc si
    cmp si, 10
    jl updateflag

	end_decrement:
    call printHEARTS
    jmp exit_decrement

	noleft:
    call printHEARTS 

	exit_decrement:
    popa
    ret




game:
    pusha
    mov ax, 0xB800      ; Set segment address for video memory
    mov es, ax

    ; Print "CATCH THE" at the top-right corner
    lea si, catchText
    mov di, 294 
    mov cx, 9
    mov ah, 0x02
	print_catch:
    lodsb
    stosw
    loop print_catch

    ; Print "ALPHABETS" on the next line
    lea si, alphabetsText
    mov di, 454
    mov cx, 9  
	print_alphabets:
    lodsb
    stosw
    loop print_alphabets

    ; Print "SCORE:" in the middle-right
    lea si, scoreText
    mov di, 1734
    mov cx, 6 
	mov ah, 0x06
	print_score:
    lodsb
    stosw
    loop print_score

    popa
    ret


start:
	mov ax, 0xB800
	mov es, ax	
	
	call begin
	mov ah, 00h      ; INT 16h function to read a key
    int 16h          ; Wait for a key press
    cmp al, 20h      ; Compare with ASCII code for space
    jne start   
	
	call clrscr
	call game
	call score
	call printHEARTS
	mov di, 3918	; intial box
	mov [box] ,di
	mov al, 0xDC
	mov ah, 0x03
	mov cx ,3
	rep stosw

	xor ax , ax
	mov es,ax
	mov ax , [es: 9 * 4]
	mov [oldisr] , ax
	mov ax , [es: 9 * 4 + 2]
	mov [oldisr+2] , ax
	cli
	mov word [es:9*4], ISRint9
	mov word [es:9*4+2], cs
	sti
	l3:
	call GenRandom
	call initialPrint
	mov cx,25
	l1:
	call movalphabets
	loop l1
	
	
	jmp l3

gameover : call delay
           ;call delay
		   mov ax , [oldisr]
		   mov bx , [oldisr+ 2]
		   cli 
		   mov [es:9*4] , ax
		   mov [es:9*4 + 2], bx
		   sti
           call clrscr
		   lea si, gameend
           mov di, 1980 
           mov cx, 12
           mov ah, 0x03
	       print:
           lodsb
           stosw
           loop print
		   lea si, scoreText
           mov di, 2140 
           mov cx, 6
           mov ah, 0x03
	       print2:
           lodsb
           stosw
           loop print2
		   call delay
		   call scoree
		   
		   
           mov ax,0x4c00
           int 0x21

catchText db 'CATCH THE', 0
alphabetsText db 'ALPHABETS', 0
scoreText db 'SCORE:', 0
beginn db 'PRESS SPACEBAR TO START' ,0
gameend db 'GAME OVER!!!',0