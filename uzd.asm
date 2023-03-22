.model small
.stack 100h
.data

new_line db 10, 13, '$'
output_msg db "Zingsninio rezimo pertraukimas!", '$'

command_d db 0
command_w db 0
daliklis db 10h

masyvas_reg_2B dw 8 dup (0)
masyvas_reg_1B db 8 dup (0)

poslinkis_baitais db 0

baitas1 db 0
baitas2 db 0
baitas3 db 0
baitas4 db 0

info_mod db 0
info_reg dw 0
info_rm db 0

msg_mov db "mov ", '$'
msg_ax db "ax", '$'
msg_bx db "bx", '$'
msg_cx db "cx", '$'
msg_dx db "dx", '$'
msg_sp db "sp", '$'
msg_bp db "bp", '$'
msg_si db "si", '$'
msg_di db "di", '$'
msg_al db "al", '$'
msg_cl db "cl", '$'
msg_dl db "dl", '$'
msg_bl db "bl", '$'
msg_ah db "ah", '$'
msg_ch db "ch", '$'
msg_dh db "dh", '$'
msg_bh db "bh", '$'
msg_bxsi_p db "[bx + si + poslinkis]", '$'
msg_bxdi_p db "[bx + di + poslinkis]", '$'
msg_bpsi_p db "[bp + si + poslinkis]", '$'
msg_bpdi_p db "[bp + di + poslinkis]", '$'
msg_si_p db "[si + poslinkis]", '$'
msg_di_p db "[di + poslinkis]", '$'
msg_bp_p db "[bp + poslinkis]", '$'
msg_bx_p db "[bx + poslinkis]", '$'
msg_poslinkis db " + poslinkis]", '$'

.code

start:
    mov ax, @data
    mov ds, ax

	mov ax, 0
	mov es, ax
	
	push es:[1*4]	
	push es:[1*4 + 2]   ;issaugom sena adresa
	
	pushf
	mov word ptr es:[1*4], offset FindMov   ; proceduros(Ip)
	mov word ptr es:[1*4 + 2], seg FindMov  ;proceduros(cs)
	popf
	
	pushf
	pushf
	pop ax
	or ax, 0100h ;set trap flag
	push ax
	popf
	
	nop
	mov ax, 0
	mov bx, 0
	mov ax, bx
	mov al, byte ptr[bx+si+10h]
	mov byte ptr[bx+di+20h], cl
	mov ch, byte ptr[bx+di+20h]
	mov ax, word ptr[bx+si+1000h]
	mov word ptr[si], cx
		
	popf
	
	pop es:[1*4+2] ;atstatom sena procceduros adresa
	pop es:[1*4]
	
	mov ax, 4c00h
    int 21h
	
	FindMov proc
		call SaveRegisters
		
		pop si ;ip
		pop di ;cs
		push di
		push si
		
		mov ax, cs:[si] 
		mov bx, cs:[si+2] 
		
		mov baitas1, al
		mov baitas2, ah
		mov baitas3, bl
		mov baitas4, bh
		
		and al, 0FCh ; neskaitom d, w
		cmp al, 088h ;mov reg <-> r/m, 1000 10dw 
		je MovFound
		
		jmp EndInt
		
		MovFound:
		
			mov al, baitas1
			and al, 1 ; randam w
			mov command_w, al
			
			mov al, baitas1
			and al, 2  
			shr al, 1
			mov command_d, al ;randam d 

			mov dx, offset output_msg
			call PrintLine
			
			mov dx, offset new_line
			call PrintLine
			
			mov ax, di ;print cs
			call Print
			
			mov dl, ":"
			call PrintSymbol
			
			mov ax, si ;print ip
			call Print
			
			mov dl, " "
			call PrintSymbol
			
			mov al, baitas2
			mov ah, baitas1 
			call Print   ;print machine code

			;find mod reg r/m, ah - adresavimo baitas
			mov dl, baitas2
			and dl, 0C0h 
			mov info_mod, dl
			shr info_mod, 6
			
			mov dl, baitas2
			and dl, 038h
			xor dh, dh
			mov info_reg, dx ;0-7 
			shr info_reg, 3
			
			mov dl, baitas2
			and dl, 07h
			mov info_rm, dl ; 0-7
			
			cmp info_mod, 1
			jne testi7
			mov al, baitas3
			jmp poslinkis1
			testi7:
			
			cmp info_mod, 2
			jne next2
			mov al, baitas4
			mov ah, baitas3
			jmp poslinkis2
			next2:   
			
			mov dl, " "
			call PrintSymbol
			
			mov dx, offset msg_mov
			call PrintLine
			
			cmp command_d, 0
			jne testi8
			jmp Check_rm
			testi8:
			
			Check_reg:
				mov bx, info_reg
			
				cmp info_reg, 0
				jne testi9
				
				mov si, offset msg_al ;1B
				mov di, offset msg_ax ;2B
				jmp printReg
				testi9:
				
				cmp info_reg, 1
				jne testi10
				
				mov si, offset msg_cl ;1B
				mov di, offset msg_cx ;2B
				jmp printReg
				testi10:
				
				cmp info_reg, 2
				jne testi11
				
				mov si, offset msg_dl ;1B
				mov di, offset msg_dx ;2B
				jmp printReg
				testi11:
				
				cmp info_reg, 3
				jne testi12
				
				mov si, offset msg_bl ;1B
				mov di, offset msg_bx ;2B
				jmp printReg
				testi12:
				
				cmp info_reg, 4
				jne testi13
				
				mov si, offset msg_ah ;1B
				mov di, offset msg_sp ;2B
				jmp printReg
				testi13:
				
				cmp info_reg, 5
				jne testi14
				
				mov si, offset msg_ch ;1B
				mov di, offset msg_bp ;2B
				jmp printReg
				testi14:
				
				cmp info_reg, 6
				jne testi15
				
				mov si, offset msg_dh ;1B
				mov di, offset msg_si ;2B
				jmp printReg
				testi15:
				
				cmp info_reg, 7
				jne next3 ;nebereik
				
				mov si, offset msg_bh ;1B
				mov di, offset msg_di ;2B
				jmp printReg
				next3:
			
				cmp command_d, 1
				jne testi24  
				
				mov dl, ","
				call PrintSymbol
				
				mov dl, " "
				call PrintSymbol
				
				jmp Check_rm
				testi24:
				
				mov dl, ";"
				call PrintSymbol
				   
				mov dl, " "
				call PrintSymbol
				
				jmp uzbaigti
	
				;---------------------------start---------------------------
				printReg:
					cmp command_w, 1
					je print_2Bytes
					
					mov dx, si ;1B, mnemonika
					call PrintLine
					
					xor cx, cx
					mov cl, byte ptr[masyvas_reg_1B + bx]
					push cx  
					push dx
					jmp next3
					
					print_2Bytes:
						mov dx, di ;mnemonika
						call PrintLine
						mov cx, word ptr[masyvas_reg_2B + bx]
						
						push cx
						push dx
						jmp next3
				;--------------------------end------------------------------- 
			Check_rm:  ;ieskom r/m
				xor bx, bx
				cmp poslinkis_baitais, 1
				je posl1
				
				cmp poslinkis_baitais, 2
				je posl2
				
				posl1:
					mov bl, baitas3
					jmp next5
				posl2:
					mov bl, baitas4
					mov bh, baitas3
				next5:
				
				cmp info_rm, 0
				je rm0
				
				cmp info_rm, 1
				je rm1
				
				cmp info_rm, 2
				je rm2
				
				cmp info_rm, 3
				je rm3
				
				cmp info_rm, 4
				je rm4
				
				cmp info_rm, 5
				je rm5
				
				cmp info_rm, 6
				je rm6
				
				cmp info_rm, 7
				je rm7
				
				rm0:
					mov dx, offset msg_bxsi_p ;ismetu mneumonika
					call PrintLine
					
					add bx, [masyvas_reg_2B + 3]
					add bx, [masyvas_reg_2B + 6]
					mov bx, word ptr[bx]
					jmp next4
				rm1:
					mov dx, offset msg_bxdi_p 
					call PrintLine
					
					add bx, [masyvas_reg_2B + 3]
					add bx, [masyvas_reg_2B + 7]  
					mov bx, word ptr[bx]
					jmp next4
				rm2:
					mov dx, offset msg_bpsi_p 
					call PrintLine
					
					add bx, [masyvas_reg_2B + 5]
					add bx, [masyvas_reg_2B + 6]
					mov bx, word ptr[bx]
					jmp next4
				rm3:
					mov dx, offset msg_bpdi_p 
					call PrintLine
					
					add bx, [masyvas_reg_2B + 5]
					add bx, [masyvas_reg_2B + 7]
					mov bx, word ptr[bx]
					jmp next4
				rm4:
					mov dx, offset msg_si_p
					call PrintLine
				
					add bx, [masyvas_reg_2B + 6]
					mov bx, word ptr[bx]
					jmp next4
				rm5:
					mov dx, offset msg_di_p
					call PrintLine
				
					add bx, [masyvas_reg_2B + 7]
					mov bx, word ptr[bx]
					jmp next4
				rm6:
					mov dx, offset msg_bp_p
					call PrintLine
					
					add bx, [masyvas_reg_2B + 5]
					mov bx, word ptr[bx]
					jmp next4
				rm7:
					mov dx, offset msg_bx_p
					call PrintLine
									
					add bx, [masyvas_reg_2B + 3]
					mov bx, word ptr[bx]
					jmp next4
				next4:

				push bx ;kartojasi funkcijoj
				push dx
						
				cmp command_d, 0
				jne testi6
				
				mov dl, ","
				call PrintSymbol
				
				mov dl, " "
				call PrintSymbol
				
				jmp Check_reg
				testi6: 
				
				mov dl, ";"
				call PrintSymbol
				
				mov dl, " "
				call PrintSymbol
				
				jmp uzbaigti
				
			poslinkis1: ;Isprinting poslinki jei toks yra
				mov poslinkis_baitais, 1
				call Print1B
				jmp next2
			poslinkis2:
				mov poslinkis_baitais, 2
				call Print
				jmp next2
			
			uzbaigti:
				xor cx, cx
				mov cl, 2
				
				jmp Output
			
		Print proc
			push ax  ;print ah tada al
			mov al, ah
			call Print1B
			pop ax
			call Print1B
			ret
		endp Print
		
		Print1B proc  ;print j.b.
			xor ah, ah
			div daliklis ;shr galima 
			push ax
			call PrintHexNumber
			pop ax
			mov al, ah
			call PrintHexNumber
			ret 
		endp Print1B
		
		PrintHexNumber proc
			cmp al, 9
			jbe decimal
			
			mov dl, al
			add dl, 37h ;A-F 
			call PrintSymbol
			jmp return
	
			decimal:
				mov dl, al
				add dl, 30h
				call PrintSymbol
				jmp return
			return:
			ret 
		endp PrintHexNumber
		
		SaveRegisters proc
			mov word ptr[masyvas_reg_2B], ax
			mov word ptr[masyvas_reg_2B + 1], cx
			mov word ptr[masyvas_reg_2B + 2], dx
			mov word ptr[masyvas_reg_2B + 3], bx
			mov word ptr[masyvas_reg_2B + 4], sp
			mov word ptr[masyvas_reg_2B + 5], bp
			mov word ptr[masyvas_reg_2B + 6], si
			mov word ptr[masyvas_reg_2B + 7], di
			
			mov [masyvas_reg_1B], al
			mov [masyvas_reg_1B + 1], cl
			mov [masyvas_reg_1B + 2], dl
			mov [masyvas_reg_1B + 3], bl
			mov [masyvas_reg_1B + 4], ah
			mov [masyvas_reg_1B + 5], ch
			mov [masyvas_reg_1B + 6], dh
			mov [masyvas_reg_1B + 7], bh
			
			ret
		endp SaveRegisters
		
		GetRegisters proc
			mov ax, word ptr[masyvas_reg_2B]
			mov cx, word ptr[masyvas_reg_2B + 1]
			mov dx, word ptr[masyvas_reg_2B + 2]
			mov bx, word ptr[masyvas_reg_2B + 3]
			mov bp, word ptr[masyvas_reg_2B + 5]
			mov si, word ptr[masyvas_reg_2B + 6]
			mov di, word ptr[masyvas_reg_2B + 7]
		
			ret
		endp GetRegisters
		
		PrintSymbol proc
			push ax ;dl = simbolis
			mov ah, 02h
			int 21h
			pop ax
			ret
		endp PrintSymbol
		
		PrintLine proc
			push ax ;dx - offset
			mov ah, 09h
			int 21h
			pop ax
			ret
		endp PrintLine
			
		Output:
			pop dx ;offset
			call PrintLine 
			  
			mov dl, "="
			call PrintSymbol
		    
			pop ax ;value
			call Print 
			
			mov dl, "h"
			call PrintSymbol
			
			mov dl, " "
			call PrintSymbol
			
			loop Output
			
			mov dx, offset new_line
			call PrintLine
			call PrintLine
			
		EndInt:
			call GetRegisters
			
			IRET
	endp FindMov

end start
