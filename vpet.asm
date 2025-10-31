;--------------------------------
;Assembly Program: Virtual Pet  |
;Target: Linux x86-64			|
;Assembler: NASM				|
;Developer: Skeome				|
; -------------------------------

section .data
	; --- SYSTEM CONSTANTS ---
	EXIT_SUCCESS equ 0
	EXIT_FAILURE equ 1
	STDIN equ 0
	STDOUT equ 1
	SYS_READ equ 0
	SYS_WRITE equ 1
	SYS_EXIT equ 60
	SYS_NANOSLEEP equ 35

	; --- ANSI COLOR CODES (Demoscene Flair) ---
	ANSI_RESET db 0x1B, "[0m"
	LEN_RESET equ $ - ANSI_RESET
	ANSI_RED db 0x1B, "[31m"
	LEN_RED equ $ - ANSI_RED
	ANSI_GREEN db 0x1B, "[32m"
	LEN_GREEN equ $ - ANSI_GREEN
	ANSI_YELLOW db 0x1B, "[33m"
	LEN_YELLOW equ $ - ANSI_YELLOW

	; --- TEXT MESSAGES ---
	msg_title db '--- V-PET: Hatchling Console ---', 0xA, 0xA
	len_title equ $ - msg_title

	msg_error db 'ERROR: Program encountered a critical failure.', 0xA
	len_error equ $ - msg_error

	msg_newline db 0xA
	len_newline equ $ - msg_newline

	; --- TIME STRUCTURE (for nanosleep) ---
	; struct timespec { long tv_sec; long tv_nsec; };
	; We will sleep for 1 second per cycle.
	sleep_req_sec dq 1			; tv_sec = 1
	sleep_req_nsec dq 0      		; tv_nsec = 0

section .bss
	; --- CORE PET STATE (32-bit DWORDS) ---
	pet_health resd 1   			; 0-100 (100 = full)
	pet_hunger resd 1   			; 0-100 (100 = starved)
	pet_age    resd 1   			; Game cycles elapsed
	pet_stage  resd 1   			; 0=Egg, 1=Baby1, 2=Baby2, 3=Child1, etc.

	; --- RANDOM SEED (64-bit QWORD) ---
	prng_seed resq 1    			; Used for generating random events/battles.

	; --- I/O BUFFERS ---
	input_choice resb 4 			; Buffer for reading menu choice + newline
	num_buffer resb 10  			; Buffer for ASCII conversion of multi-digit numbers

	; --- TIME REMAINING (Required by nanosleep syscall) ---
	sleep_rem resb 16   			; Placeholder for remaining time structure

section .text
	global _start

; ----------------------------------------------------------------------
; ROUTINE: print_string
; Prints a string of a given length to stdout.
; Args:
;	rdi: Address of the string to print
;	rsi: Length of the string to print
; ----------------------------------------------------------------------
print_string:
	push rbp
	mov rbp, rsp
	push rdx                		; Save RDX (length) for syscall

	; rdi (address) and rsi (length) are set by caller
	mov rdx, rsi            		; Syscall arg 3: length
	mov rsi, rdi            		; Syscall arg 2: address
	mov rax, SYS_WRITE      		; Syscall 1 (write)
	mov rdi, STDOUT         		; Syscall arg 1: stdout
	syscall
    
	pop rdx
	mov rsp, rbp
	pop rbp
	ret

; ----------------------------------------------------------------------
; ROUTINE: print_num
; Converts a 32-bit integer (in EAX) to a string and prints it to stdout.
; Args:
;	eax: The number to print (32-bit)
;	Uses num_buffer (in .bss) to store the reversed digits.
; ----------------------------------------------------------------------
print_num:
	push rbp
	mov rbp, rsp
	push rbx                		; Save caller-saved registers
	push rcx
	push rdx
	push rsi

	mov rsi, num_buffer     		; Use buffer as scratchpad
	mov ecx, 0              		; Digit counter (length of the number string)
	mov ebx, 10             		; Divisor (10)

.divide_loop:
	; Use 32-bit division: IDIV uses EDX:EAX as dividend.
	mov edx, 0              		; Clear EDX (high 32 bits of dividend)
	div ebx                 		; EAX = quotient, EDX = remainder (the digit)

	; Convert digit (EDX) to ASCII and store it in the buffer (reversed)
	add dl, '0'             		; Convert remainder in DL to ASCII
	mov [rsi+rcx], dl       		; Store ASCII digit
	inc ecx                 		; Increment digit counter
	cmp eax, 0              		; Check if quotient is zero
	jnz .divide_loop        		; Loop if quotient is not zero

; --- Print the digits from the buffer (which are currently reversed) ---
	; RCX holds the count (length of number string)

	; We need to print them in reverse order by iterating backward from the end
	; RSI is the start of the buffer. Start printing from (RSI + RCX - 1)

	mov rdx, rcx            		; RDX = total length (digit count)
	mov rsi, num_buffer     		; RSI = starting address
	add rsi, rdx            		; Point RSI to the byte AFTER the last digit
	dec rsi                 		; Point RSI to the last digit (first to print)

.print_loop:
	; Print the character pointed to by RSI
	mov rax, SYS_WRITE
	mov rdi, STDOUT
	mov rdx, 1              		; Length is 1 byte
	syscall                 		; Print the single character

	dec rsi                 		; Move to the previous character
	loop .print_loop        		; Decrement RCX and loop until RCX is 0 (this is an efficient loop instruction)

	; Note: LOOP instruction uses RCX implicitly.

	pop rsi                 		; Restore caller-saved registers
	pop rdx
	pop rcx
	pop rbx
	mov rsp, rbp
	pop rbp
	ret

; ----------------------------------------------------------------------
; ROUTINE: clean_exit
; Exits the program with a success status (0).
; ----------------------------------------------------------------------
clean_exit:
	mov rax, SYS_EXIT			; Syscall 60 (exit)
	mov rdi, EXIT_SUCCESS			; Exit status 0
	syscall
; ----------------------------------------------------------------------

; ----------------------------------------------------------------------
; ROUTINE: error_exit
; Displays an error message and exits the program with a failure status (1).
; ----------------------------------------------------------------------
error_exit:
	; Print Error Message
	mov rdi, msg_error
	mov rsi, len_error
	call print_string

	; Exit with Error Code
	mov rax, SYS_EXIT			; Syscall 60 (exit)
	mov rdi, EXIT_FAILURE			; Exit status 1
	syscall
; ----------------------------------------------------------------------


_start:
	; 1. Initialize Pet State
	mov dword [pet_health], 100
	mov dword [pet_hunger], 50
	mov dword [pet_age], 0
	mov dword [pet_stage], 0		; Start at Egg stage

	; 2. Initialize PRNG Seed
	; Use the current time-stamp counter for a non-deterministic seed
	rdtsc                       		; RDX:RAX = 64-bit time-stamp counter
	mov [prng_seed], rax

	; 3. Print Welcome Message
	mov rdi, msg_title
	mov rsi, len_title
	call print_string
    
.game_loop_start:
	; A. DISPLAY: Call display_status
	; B. INPUT: Call handle_input (User action: Feed, Train, Clean, Quit)

	; C. EVENT CHECK: Call check_random_event (Uses PRNG_SEED)

	; D. UPDATE: Call update_state (Decay logic, Age++)
	; E. CHECK: Call check_death (If Health <= 0 or Age > MAX_AGE)

	; F. PAUSE: Syscall 35 (nanosleep) for 1 second.
	; (This requires the time_req structure to be setup, which we have in .data)
    
	; --- Implementation of the PAUSE step (Syscall 35) ---
	mov rax, SYS_NANOSLEEP          	; Syscall 35
	mov rdi, sleep_req_sec          	; Request time structure (1 second)
	mov rsi, sleep_rem              	; Remaining time structure (placeholder)
	syscall
    
	jmp .game_loop_start
    
	; This jump should eventually lead to clean_exit, but for now, it loops.
	; We'll add the quit condition inside the game loop later.

; --- Subroutines will be placed here (e.g., print_num, display_status, update_state) ---

