org 100h
jmp start

shape1:db '_'
shape2:db '@'
starting:dw 162
ending:dw 318
str1:db 'LIVES'
size1:dw 5
lives:dw 3
score:dw 0
level:dw 1
symbol_lives:db '='
symbol_size:dw 1
str2:db 'SCORE'
size2:dw 5
str3:db 'LEVEL'
size3:dw 5
mult_location:db 80
oldisr:dd 0
count dw 000h
hours:dw 0
mins:dw 0
secs:dw 0
strlen: db 20  
string: dw 0x020a,0x030a,0x040a,0x050a,0x060a,0x070a,0x080a,0x090a,0x0a0a,0x0b0a,0x0c0a,0x0d0a,0x0e0a,0x0f0a,0x100a,0x110a,0x120a,0x130a,0x140a,0x150a,0x0b0a,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0   ; contains the x-coordinate followed by y-quardinate ;;
Food: dw 2000,3000,1000,1500,3700,1694,2500,28,3900 
FoodPtr: dw 0 
DiscardedTail: dw 0       
StrPosition: db 0          
direction: db 2             
SubDirection: db 0   
gameover: db 'Game Over Snake Collided' 
newthing: db 0x4d 
Fruitshape:dw 0x8940,0x8941,0x8942,0x8943,0x893c,0x893d
Fruit_index:dw 1
sound_index: dw 0
sound_data:incbin "kingsv.wav" ; 51.529 bytes
speed_inner:dw 100
speed_outer:dw 400
speed_check:dw 20
rules0:db '  SNAKE GAME '
rules1:db '1.Use arrow keys to move the snake' ;32
rules2:db '2.you have three lives and four minutes to play'  ;45
rules3:db '3.if snake touches border or eat dangerous food then you lose one life'  ;68
rules4:db '4.Speed increases after 20 sec'  ;28
rules5:db '5.Touching itslef result in loss of one life'  ;42
rules6:db '6.Press any key to start game'  ;27

	
start:
    
	call clrscr
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0820 
	mov cx, 13
	push cs
	pop es 
	mov bp, rules0
	int 0x10
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0a09 
	mov cx, 34
	push cs
	pop es 
	mov bp, rules1
	int 0x10
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0b09
	mov cx, 47
	push cs
	pop es 
	mov bp, rules2
	int 0x10
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0c09
	mov cx, 70
	push cs
	pop es 
	mov bp, rules3
	int 0x10
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0d09
	mov cx, 30
	push cs
	pop es 
	mov bp, rules4
	int 0x10
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0e09 
	mov cx, 44
	push cs
	pop es 
	mov bp, rules5
	int 0x10
	
	mov ah, 0x13
    mov al, 0 
	mov bh, 0 
	mov bl, 5 
	mov dx, 0x0f09
	mov cx, 29
	push cs
	pop es 
	mov bp, rules6
	int 0x10
	
	mov ah,0
	int 16h
	
	call clrscr
	call background
	call clrscr_first_line

	push cs
	pop ds

	xor ax,ax
	mov es,ax

	cli

	mov ax,word[es:8h*4]
	mov word[oldisr],ax
	mov ax,word[es:8h*4+2]
	mov word[oldisr+2],ax

	mov word[es:8h*4],timer
	mov [es:8h*4+2],cs
	
	sti 
	
	repeat1:
	
	call PrintSnake
	call KeyOperation
	call FoodCheck
	call CollisionCheck
	call cls
	call background
	
	jmp repeat1
	
	mov dx,start
	add dx,15
	mov cl,4
	shr dx,cl

	mov ax,4ch
	int 21h

clrscr_first_line:

	push es
	push ax
	push cx
	push di
	mov ax,0xb800
	mov es,ax
	xor di,di
	mov ah,7
	mov al,0x20
	mov cx,80
	cld
	rep stosw
	pop di
	pop cx
	pop ax
	pop es
	ret

clrscr:

	push es
	push ax
	push cx
	push di
	mov ax,0xb800
	mov es,ax
	xor di,di
	mov ah,7 ;dont disturb
	mov al,0x20
	mov cx,2000
	cld
	rep stosw
	pop di
	pop cx
	pop ax
	pop es
	ret

printnum: 

	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es,ax 
	mov ax,word[bp+4] 
	mov bx,10 
	mov cx,0 

	nextdigit: 
		mov dx,0 
		div bx 
		add dl,0x30 
		push dx 
		inc cx 
		cmp ax,0 
		jnz nextdigit 

	nextpos: 
		pop dx 
		mov dh,1
		mov [es:di], dx
		add di,2 
		loop nextpos 

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2

background:

	mov di,word[starting]
	mov ax,0xb800
	mov es,ax
	mov word[es:160],0x720   ;left screen block clear
	mov word[es:318],0x720   ;right screen block clear
	mov bl,byte[shape1]
	mov cx,78

	label1:
		mov bh,7
		mov word[es:di],bx
		add di,2
		loop label1

	add di,160
	mov bl,byte[shape2]
	mov cx,23

	label2:
		mov bh,4
		mov word[es:di],bx
		add di,160
		loop label2

	sub di,162
	mov bl,byte[shape1]
	mov cx,78

	label3:
		mov bh,4
		mov word[es:di],bx
		sub di,2
		loop label3

	mov bl,byte[shape2]
	mov cx,23

	label4:
		mov bh,4
		mov word[es:di],bx
		sub di,160
		loop label4

	ret

printstr: 

	push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	mov ax, 0xb800
	mov es, ax 
	mov al, 80 
	mul byte [bp+10] 
	add ax, [bp+12] 
	shl ax, 1 
	mov di,ax 
	mov si, [bp+6] 
	mov cx, [bp+4] 
	mov ah, [bp+8] 
	cld 

	nextchar: 
		lodsb 
		stosw 
		loop nextchar
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
	ret 10

print_lives_String:

	mov ax,40
	push ax 
	mov ax,0
	push ax 
	mov ax, 1 
	push ax 
	mov ax, str1
	push ax 
	push word [size1]
	call printstr       ;print the string lives 
	ret

print_symbol_lives:

	mov ax,39
	add ax,word[size1]
	add ax,1
	push ax 	
	mov ax,0
	push ax 
	mov ax, 1 
	push ax 
	mov ax,symbol_lives
	push ax 
	push word[symbol_size]
	call printstr     ;print the symbol equal for lives
	ret

print_remaining_lives:

	mov ax,word[lives]
	push ax
	mov ax,0
	mul byte[mult_location]
	add ax,46
	shl ax,1
	mov di,ax
	call printnum     ;print the lives	
    mov ax,0xb800
    mov es,ax
    mov al,'/'
    mov ah,1
    mov word[es:94],ax
    mov al,'3'
    mov ah,1
    mov word[es:96],ax	
	;mov word[es:104],0x720
	ret

print_score_string:

	mov ax, 55
	push ax 
	mov ax,0
	push ax 
	mov ax, 1 
	push ax 
	mov ax, str2
	push ax 
	push word [size2]
	call printstr    ;print the string of score
	ret
 
print_symbol_score:

	mov ax,54
	add ax,word[size2]
	add ax,1
	push ax 
	mov ax,0
	push ax 
	mov ax, 1 
	push ax 
	mov ax,symbol_lives
	push ax 
	push word[symbol_size]
	call printstr     ;print the symbol for scores
	ret

print_remaining_score:

	mov ax,word[score]
	push ax
	mov ax,0
	mul byte[mult_location]
	add ax,61
	shl ax,1
	mov di,ax
	call printnum    ;print the scores
	ret
	
print_level_string:

    mov ax,66
	push ax 
	mov ax,0
	push ax 
	mov ax,4 
	push ax 
	mov ax, str3
	push ax 
	push word [size3]
	call printstr       ;print the string level
	ret	
	
print_symbol_level:

	mov ax,71
	push ax 
	mov ax,0
	push ax 
	mov ax,4
	push ax
	mov ax,symbol_lives
	push ax 
	push word[symbol_size]
	call printstr     ;print the symbol for scores
	ret	
	
print_remaining_level:

	mov ax,word[level]
	push ax
	mov di,144
	call printnum    ;print the scores
	ret	

display:

	mov ah,7
	mov word[es:di],ax
	sub bx,2
	ret


time:

	cmp word[secs],58
	jbe label_2
	jmp label_1

	label_1:
		add word[mins],1
		cmp word[mins],4
		je label_exceed
		jmp skip
	
    label_exceed:
         jmp inner_terminate	

	skip:
		mov word[secs],0
		mov word[es:156],' '

	label_2:
		mov di,150
		mov ax,word[mins]
		push ax
		call printnum
		mov al,':'
		add di,2
		call display
        	
		add word[secs],1
		mov ax,word[speed_check]
		cmp word[secs],ax
		je label_speed
		jmp label_speed_2
		
		label_speed:
		sub word[speed_inner],30
		sub word[speed_outer],30
		add word[speed_check],20
		
		label_speed_2:
		mov ax,word[secs]
		push ax
		add di,2
		call printnum

	ret

timer:		
	push ax
	push bx
	push cx
	push dx
	push cs
	push es
	push si
	push di

	mov ax,0xb800
	mov es,ax
	mov cx,[count]
	inc cx
	mov [count],cx
	cmp cx,011h     ;hexa of 17
	jne exit_2
	mov cx,0000h
	mov [count],cx
	call time
	
	repeatit:
		
		call print_lives_String
		call print_symbol_lives
		call print_remaining_lives
		call print_score_string
		call print_symbol_score
		call print_remaining_score
		call print_level_string
		call print_symbol_level
		call print_remaining_level
       call Sound
		
	exit_2:

	pop di
	pop si
	pop es
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax

	jmp far[cs:oldisr]
	iret

exit:
    mov ax,4ch
	int 21h	
	
KeyOperation:

  jmp next_one
  
  blink: 
	jmp keyagain 

		
terminate: 
     
	call Sound 
	sub word[lives],1 
	cmp word[lives],0
    jne repeat1
	
	inner_terminate:
	call cls 
	mov ax,0xb800
	mov es,ax
	mov word[es:102],0x130
	mov ah,0x13 
	mov al,1            
	mov bh,0 
	mov bl,0x87
	mov dx,0x0c08
	mov cx,24
	push cs 
	pop es 
	mov bp,gameover	
	int 0x10

	call delay 
	call delay
	call delay
	
	call delay 
	call delay
	call delay
	call delay 
	call delay
	call delay
	
	; again:       ;disable screen after terminate
	; call clrscr
	; jmp again

	jmp exit	
		
 next_one:

	push bx
	call GetKey
 
	mov bh,0
	mov bl,[strlen]
	sub bl,1
	shl bx,1 ; multiply by 2
	mov dx,[string+bx]
 
	cmp ah,0x48
	jne NextKeyOperation2
	cmp dl,1 ; give new head position
	;je exit
    je terminate
 
	cmp byte[direction],3
	je blink
 
	sub dl,1
	mov byte[newthing],0x48 
	mov [string+bx],dx
	mov byte[direction],1
	jmp exitKeyOperation
 
	NextKeyOperation2:
		cmp ah,0x4d
		jne NextKeyOperation3
		cmp dh,78 ; give new head position
		;je exit
		je terminate
		cmp byte[direction],4
		je blink
		add dh,1
 
	mov byte[newthing],0x4d 
	mov [string+bx],dx
	mov byte[direction],2
	jmp exitKeyOperation
 
	NextKeyOperation3:
		cmp ah,0x50
		jne NextKeyOperation4
		cmp dl,23 ; give new head position
		;je exit
		je terminate
		cmp byte[direction],1
		je blink
		add dl,1
 
	mov byte[newthing],0x50             ;;;;;;;;;;;;;;
	mov [string+bx],dx
	mov byte[direction],3
	jmp exitKeyOperation
 
	NextKeyOperation4:
		cmp ah,0x4b
		jne keyagain
		cmp dh,1 ; give new head position
		;je exit
		je terminate
		cmp byte[direction],2
		je blink
		sub dh,1
 
	mov byte[newthing],0x4b           
	mov [string+bx],dx
	mov byte[direction],4
	jmp exitKeyOperation
 
	keyagain:
		call KeyOperation
 
	exitKeyOperation:
		pop bx
		ret			

		
GetKey: 

    mov ah,1 
	int 0x16 

	jz leavethis 
	mov [newthing],ah 
 
	mov ax,0 
	int 0x16 
	jmp leave1 
 
	leavethis: 
		mov ah,[newthing] 
		
	leave1: 
		call delay 
		call delay 
		call delay 
		call delay 
	ret
			
delay:
 
	push ax 
	push bx 
	mov bx,0
	
	outerdelay: 
		mov ax,0 
		
	innerdelay: 
		add ax,1 ;
		cmp ax,word[speed_inner]   
		jne innerdelay 	
 
	add bx,1 
	cmp bx,word[speed_outer]
	jne outerdelay 
 
	pop bx 
	pop ax 
	ret	
	
PrintSnake: 
 
	call PrintHead 
	call PrintTrunk 
	call PrintTail 
	call ShiftLeft 
	call PrintFood
	ret	

	
PrintHead: 
	push ax
	push bx
	push 0xb800
	pop es
	mov al,[strlen] ; requirement of the function
	mov [StrPosition],al
	call Position                                       ; this function returns postion in ax (so for safety ax is pushed in stack at the start!)
	mov bx,ax ; copy position is pc
 
	cmp byte[direction],0x1
	jne nextcmp2
	mov al,0x3a  ; the ascii of head directed upwards
	mov [es:bx],al
 
	mov [SubDirection],al ; update 'subdirection' memory place
	jmp headexit
 
	nextcmp2:
		cmp byte[direction],0x2
		jne nextcmp3
		mov al,0x3a ; the ascii of head directed rightwards
		mov [es:bx],al
		mov [SubDirection],al ; update 'subdirection' memory place
		jmp headexit
 
	nextcmp3:
		cmp byte[direction],0x3
		jne nextcmp4
		mov al,0x3a ; the ascii of head directed downwards
		mov [es:bx],al
		mov [SubDirection],al ; update 'subdirection' memory place
		jmp headexit
 
	nextcmp4:
		mov al,0x3a  ; the ascii of head directed leftwards
		mov [es:bx],al
		mov [SubDirection],al ; update 'subdirection' memory place
 
	headexit:
		pop bx
		pop ax
		ret 
		
PrintTrunk: 
 
	againPrintTrunk:
		cmp byte[StrPosition],2
		je exitPrintTrunk
		sub byte[StrPosition],1 
		call TrunkOrganPrint 
		jmp againPrintTrunk 
		
	exitPrintTrunk: 
		ret	

TrunkOrganPrint: 

	mov al,[StrPosition]
	call Position
	mov bx,ax                                       ; copy first position position in bx
	mov al,[StrPosition]
	sub al,1
	call Position
	cmp byte[SubDirection],1
	jne OrganCmp2 ; this is main comparison
	sub ax,2
	cmp ax,bx
	jne sub1num2 ; this is sub comparison
	mov al,0x2a
	mov [es:bx],al
	mov byte[SubDirection],4
	jmp exitTrunkOrganPrit
 
	sub1num2:
		sub ax,158
		cmp ax,bx
		jne sub1num3
		mov al,0x2a
		mov [es:bx],al
		jmp exitTrunkOrganPrit
 
	sub1num3:
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],2
		jmp exitTrunkOrganPrit
 
	OrganCmp2:
 
		cmp byte[SubDirection],2
		jne OrganCmp3 ; this is main comparison
		add ax,160
		cmp ax,bx
		jne sub2num2 ; this is sub comparison
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],3
		jmp exitTrunkOrganPrit
 
	sub2num2:
		sub ax,158
		cmp ax,bx
		jne sub2num3
		mov al,0x2a
		mov [es:bx],al
		jmp exitTrunkOrganPrit
 
	sub2num3:
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],1
		jmp exitTrunkOrganPrit
 
	OrganCmp3:	
		cmp byte[SubDirection],3
		jne OrganCmp4 ; this is main comparison
		sub ax,2
		cmp ax,bx
		jne sub3num2 ; this is sub comparison
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],4
		jmp exitTrunkOrganPrit
 
	sub3num2:
		add ax,162
		cmp ax,bx
		jne sub3num3
		mov al,0x2a
		mov [es:bx],al
		jmp exitTrunkOrganPrit
 
	sub3num3:
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],2
		jmp exitTrunkOrganPrit
 
	OrganCmp4:
		add ax,160
		cmp ax,bx
		jne sub4num2 
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],3
		jmp exitTrunkOrganPrit
 
	sub4num2:
		sub ax,162
		cmp ax,bx
		jne sub4num3
		mov al,0x2a
		mov [es:bx],al
		jmp exitTrunkOrganPrit
 
	sub4num3:
		mov al,0x2a
		mov [es:bx],al
		mov byte[SubDirection],1
		
	exitTrunkOrganPrit: 
		ret	

PrintTail: 
	push ax
	push bx 
	mov al,1
	call Position ; function returns position in ax
	mov bx,ax         ; copy  
	cmp byte[SubDirection],1
	jne NextTailCmp2
	mov al,0x2a
	mov [es:bx],al
	jmp exitPrintTail
 
	NextTailCmp2:
		cmp byte[SubDirection],2
		jne NextTailCmp3
		mov al,0x2a
		mov [es:bx],al
		jmp exitPrintTail
 
	NextTailCmp3:
		cmp byte[SubDirection],3
		jne NextTailCmp4
		mov al,0x2a
		mov [es:bx],al
		jmp exitPrintTail
 
	NextTailCmp4:
		mov al,0x2a
		mov [es:bx],al
 
	exitPrintTail: 
		pop bx 
		pop ax 
		ret	

ShiftLeft: 
	push ax
	push bx
	push cx
	mov bx,[string]
	mov [DiscardedTail],bx
	mov ch,0             ;make sure there is nothing else in ch
	mov cl,[strlen]
	mov bx,0
	sub cx,1
 
	ShiftLeftAgain:
		mov ax,[string+bx+2]
		mov [string+bx],ax
		add bx,2
		sub cx,1
		cmp cx,0
		jne ShiftLeftAgain
		
	pop cx
	pop bx
	pop ax
	ret		
		
PrintFood: 

	push bx
	push di
	push ax
	mov bx,[FoodPtr]
	mov di,[Food+bx]
	mov ax,word[Fruitshape]
	;call random_food
	mov word[es:di],ax   ;blinking food
	pop ax
	pop di
	pop bx
	ret
	
random_food:
    push ax
	push bx
	mov bl,5
    mov ax,1000
    div bl
    pop bx
    pop ax
    ret	
	
		
Position:

	push bx
	mov bh,0
	mov bl,al            ; moves the length of the string in bx
	mov ax,0 ; make sure that ah has 0 in it
	shl bx,1                    ; multiply by two as we are dealing with word
	sub bx,2 ; start direction the head position
	mov al,80
	push bx
	mov bl,[string+bx] ;load y-coordinate from the memore
	mul bl
	pop bx
	mov bl,[string+bx+1] ;multiplication algo (leading x-coordinate)
	mov bh,0 ; make sure that bh shas 0 for proper addition with ax
	add ax,bx
	shl ax,1
	pop bx
	ret	

increase_level:       ;to increase the level
    inc word[level]
	push ax
	mov ax,0
	mov word[score],ax
	mov ax,0xb800
	mov es,ax
	mov word[es:124],0x720
	mov ax,0
	mov al,18
	mov byte[strlen],al
	mov word[lives],3
	pop ax
    jmp repeat1	

FoodCheck: 

	push ax 
	push bx
	push cx 
	push dx
	push si
	push di
 
	mov al,[strlen]
	call Position
	mov si,ax       ; copy position of the head
 
	mov bx,[FoodPtr]
	mov di,[Food+bx]
 
	cmp si,di
	jne exitFoodCheck
	jmp continue
	
	exitFoodCheck: 
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax 
		ret	
 
    continue:
	call Sound
    push ax
	push si
	
	mov si,0
	mov si,word[Fruit_index]       ;fruit chnaging indexing
	mov ax,word[Fruitshape+si]
	mov word[Fruitshape],ax
	add si,1
	cmp word[Fruit_index],2
	je label_danger
	jmp nothing_food
	
	label_danger:
	  dec word[lives]
	
	nothing_food:
	mov word[Fruit_index],si
	cmp word[Fruit_index],5
	jae label_Food
	jmp return_Food
	
	label_Food:
	 mov byte[Fruit_index],0
	
	return_Food:
	pop si
	pop ax
	
    add word[score],2
	cmp word[score],20
	je increase_level
	mov bh,0
	mov bl,[strlen]
	shl bx,1
	sub bx,2
	
	againFoodloop:
		mov ax,[string+bx]          ; shiftig right loop to increase length
		mov [string+bx+2],ax
		sub bx,2
		cmp bx,-2
		jne againFoodloop

	mov ax,[DiscardedTail]
	mov [string],ax
 
	add byte[strlen],1
	add word[FoodPtr],2
	cmp word[FoodPtr],12
	jna exitFoodCheck
	mov word[FoodPtr],0
	jmp exitFoodCheck
		
		
CollisionCheck: 

	push bx 
	push ax
	mov bh,0
	mov bl,[strlen]
	shl bx,1
	sub bx,2
	mov ax,[string+bx]
	
	againCollisionCheck:
		sub bx,2
		cmp ax,[string+bx]
		je terminate
		cmp bx,0
		je exitCollisionCheck
		jmp againCollisionCheck
 
	exitCollisionCheck:
		pop ax 
		pop bx
		ret

Sound:

	.loop:
        ; send DSP Command 10h
		mov dx, 22ch
		mov al, 10h
		out dx, al

	; send byte audio sample
		mov si, [sound_index]
		mov al, [sound_data + si]
		out dx, al

		mov cx, 30
			
	.delay:
		nop
		loop .delay
			
		inc word [sound_index]
		cmp word [sound_index], 5152
		jb .loop
		mov word[sound_index],0
		;jmp .delay

      ret			


cls:
	push es
	push di

	mov di,162
	push 0xb800
	pop es
 
	mov ax,0x620
	mov cx,2000
	rep stosw 

	pop di
	pop es

	ret	
		